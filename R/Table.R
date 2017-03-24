# Constructors and other methods for the various classes of Table
# Note:  These classes are all suitable for uploading to Synapse, but don't
# have common accessor methods for accessing their data.
# 
# Author: brucehoff
###############################################################################

setMethod(
  f = "Table",
  signature = signature("TableSchemaOrCharacter", "data.frame"),
  definition = function(tableSchema, values, updateEtag=character(0)) {
    result<-new("TableDataFrame")
    result@schema<-tableSchema
    result@values<-values
    result@updateEtag<-updateEtag
    result
  }
)

showSchemaOrEntityId<-function(schema) {
  if (is(schema, "Entity")) {
    cat("Table Schema Name: ", properties(schema)$name, "\n", sep="")
    cat("Table Schema Id: ", properties(schema)$id, "\n", sep="")
  } else {
    cat("Table Schema Id: ", schema, "\n", sep="")
  }
}

setMethod(
  f = "show",
  signature = signature("TableDataFrame"),
  definition = function(object){
    cat('An object of class "', class(object), '"\n', sep="")
    showSchemaOrEntityId(object@schema)
    cat(sprintf("data frame columns:\n%s\n", paste(names(object@values), collapse="\n")))
  }
)

setMethod(
  f = "show",
  signature = signature("TableFilePath"),
  definition = function(object){
    cat('An object of class "', class(object), '"\n', sep="")
    showSchemaOrEntityId(object@schema)
    cat(sprintf("filePath:\n%s\n", object@filePath))
  }
)

setMethod(
  f = "show",
  signature = signature("TableRowCount"),
  definition = function(object){
    cat('An object of class "', class(object), '"\n', sep="")
    showSchemaOrEntityId(object@schema)
    cat(sprintf("row count:\n%s\n", object@rowCount))
  }
)


setMethod(
  f = "Table",
  signature = signature("TableSchemaOrCharacter", "character"),
  definition = function(
    tableSchema, 
    values,
    linesToSkip,
    quoteCharacter,
    isFirstLineHeader,
    escapeCharacter,
    lineEnd,
    separator,
    updateEtag) {
    result<-new("TableFilePath")
    result@schema<-tableSchema
    result@filePath<-values
    if (!missing(linesToSkip)) result@linesToSkip<-linesToSkip
    if (!missing(quoteCharacter)) result@quoteCharacter<-quoteCharacter
    if (!missing(isFirstLineHeader)) result@isFirstLineHeader<-isFirstLineHeader
    if (!missing(escapeCharacter)) result@escapeCharacter<-escapeCharacter
    if (!missing(lineEnd)) result@lineEnd<-lineEnd
    if (!missing(separator)) result@separator<-separator
    if (!missing(updateEtag)) result@updateEtag<-updateEtag
    result
  }
)

setMethod(
  f = "Table",
  signature = signature("TableSchemaOrCharacter", "integer"),
  definition = function(
    tableSchema, 
    values,
    linesToSkip,
    quoteCharacter,
    isFirstLineHeader,
    escapeCharacter,
    lineEnd,
    separator,
    updateEtag) {
    result<-new("TableFileHandleId")
    result@schema<-tableSchema
    result@fileHandleId<-values
    if (!missing(linesToSkip)) result@linesToSkip<-linesToSkip
    if (!missing(quoteCharacter)) result@quoteCharacter<-quoteCharacter
    if (!missing(isFirstLineHeader)) result@isFirstLineHeader<-isFirstLineHeader
    if (!missing(escapeCharacter)) result@escapeCharacter<-escapeCharacter
    if (!missing(lineEnd)) result@lineEnd<-lineEnd
    if (!missing(separator)) result@separator<-separator
    if (!missing(updateEtag)) result@updateEtag<-updateEtag
    result
  }
)

ensureTableSchemaStored<-function(tableSchema) {
  # ensure that the schema is stored
  id<-propertyValue(tableSchema, "id")
  if (is.null(id)) {
    tableSchema<-synStore(tableSchema)
  }
  tableSchema
}

ensureTableSchemaIsRetrieved<-function(tableSchemaOrID) {
  if (is(tableSchemaOrID, "TableSchema")) {
    tableSchemaOrID
  } else if (is(tableSchemaOrID, "character")) {
    tableId<-tableSchemaOrID
    if (!isSynapseId(tableId)) stop(sprintf("%s is not a Synapse ID.", tableId))
    synGet(tableId)
  } else {
    stop("argument is neither TableSchema nor character")
  }
}

parseRowAndVersion<-function(x) {
  parsed<-strsplit(x, "_", fixed=T)
	as.integer.or.na<-function(x) {
		result<-as.integer(x)
		if (length(result)<2) result<-c(NA,NA)
		result
	}
  sapply(X=parsed, FUN=function(x){as.integer.or.na(x)})
}

# uploads a data frame to a table
# column labels in data frame must match those in table schema
# if updateEtag is omitted then this is treated as an upload of new rows
# if updateEtag is included then this is treated as an update of existing
# rows.  The row labels must be integers and are taken to be the indices of
# the rows to be updated
storeDataFrame<-function(tableSchema, dataframe, retrieveData, verbose, updateEtag=character(0)) {
  if (nrow(dataframe)<1 | ncol(dataframe)<1) stop("data frame is empty.")
  # validate column headers
  schemaColumns<-synGetColumns(tableSchema)
  schemaColumnMap<-list()
  for (column in schemaColumns@content) schemaColumnMap[[column@name]]<-column
  for (dfColumnName in names(dataframe)) {
    schemaColumn<-schemaColumnMap[[dfColumnName]]
    if (is.null(schemaColumn)) stop(sprintf("Data frame has column %s but schema has no such column.", dfColumnName))
    dfColumnType<-class(dataframe[[dfColumnName]])[1]
    expectedTableColumnTypes<-getTableColumnTypeForDataFrameColumnType(dfColumnType)
    tableColumnType<-schemaColumn@columnType
    if (!any(tableColumnType==expectedTableColumnTypes)) {
      stop(sprintf("Column %s has type %s in Synapse but %s in the data frame. Allowed data frame columns types: %s.", 
					  dfColumnName, tableColumnType, dfColumnType, 
          paste(expectedTableColumnTypes, collapse=" or ")))
    }
  }
  
  if (length(updateEtag)==0) {
    dataFrameToWrite<-dataframe
  } else {
    rownamesAsInteger<-parseRowAndVersion(row.names(dataframe))
    # Now we append the rows labels
    ROW_ID<-rownamesAsInteger[1,]
    ROW_VERSION<-rownamesAsInteger[2,]
    # Calling the appended vector 'ROW_ID' causes the column label to be 'ROW_ID' as well.  Ditto for ROW_VERSION.
    dataFrameToWrite<-cbind(ROW_ID, ROW_VERSION, dataframe)
    # this allows us to control the column label for the row column. 
  }
  # documentation for textConnection states:
  # "they are relatively expensive to use, and it is often better to 
  # use an anonymous 'file()' connection to collect output."
  filePath<-tempfile()
  writeDataFrameToCSV(dataFrameToWrite, filePath)
  s3FileHandle<-chunkedUploadFile(filePath)
  
  rowsProcessed<-uploadFileHandleIdToTable(as.integer(s3FileHandle$id), tableId=propertyValue(tableSchema, "id"), verbose=verbose, updateEtag=updateEtag)
}

# returns the Synapse types which can hold the given R type, with the first one being the preferred
getTableColumnTypeForDataFrameColumnType<-function(dfColumnType) {
  map<-list(integer=c("INTEGER","DOUBLE","FILEHANDLEID"), 
    factor=c("STRING","FILEHANDLEID","ENTITYID","LINK"), 
    character=c("STRING","FILEHANDLEID","ENTITYID","LINK"), 
    numeric=c("DOUBLE","INTEGER","FILEHANDLEID"), 
	# note, if a column is all NA then the type is 'logical', so we must allow
	# any table column type for this R type
    logical=c("BOOLEAN","STRING","FILEHANDLEID","ENTITYID","LINK","INTEGER","DOUBLE", "DATE"),
    Date=c("DATE","STRING"),
    POSIXct=c("DATE","STRING"))
  result<-map[[dfColumnType]]
  if (is.null(result)) stop(sprintf("No column type for %s", dfColumnType))
  result
}

writeDataFrameToCSV<-function(dataFrame, filePath) {
  for (i in 1:dim(dataFrame)[2]) {
    if (is.numeric(dataFrame[[i]])) {
      dataFrame[[i]][is.nan(dataFrame[[i]])]<-"NaN"
  	} else if (is(dataFrame[[i]], "POSIXct")) {
	  # convert POSIXct before uploading to Synapse
			   dataFrame[[i]]<-format(as.POSIXlt(dataFrame[[i]], 'UTC', usetz=TRUE), "%Y-%m-%d %H:%M:%S.000")
    }
  }
  write.csv(x=dataFrame, file=filePath, row.names=FALSE, na="")
}

readDataFrameFromCSV<-function(filePath) {
  read.csv(filePath, encoding="UTF-8", stringsAsFactors=FALSE, check.names=FALSE, na.strings=c(""))
}

setMethod(
  f = "synStore",
  signature = "TableDataFrame",
  definition = function(entity, retrieveData=FALSE, verbose=TRUE, filePath=NULL) {
    if (!is(entity@values, "data.frame")) stop("data frame required.")
    entity@schema<-ensureTableSchemaIsRetrieved(entity@schema)
    entity@schema<-ensureTableSchemaStored(entity@schema)
    rowsProcessed<-storeDataFrame(entity@schema, entity@values, retrieveData, verbose, entity@updateEtag)
    if (retrieveData) {
      tableId<-propertyValue(entity@schema, "id")
      sql<-sprintf("select * from %s", tableId)
      downloadResult<-downloadTableToCSVFile(sql, verbose, filePath=filePath)
      dataframe<-loadCSVasDataFrame(downloadResult$filePath)
      dataframe<-convertDataFrameTypeToSchemaType(dataframe, downloadResult$headers)
      Table(entity@schema, dataframe, downloadResult$etag)
    } else {
      TableRowCount(entity@schema, rowsProcessed)
    }
  }
)

TableRowCount<-function(schema, rowCount, updateEtag) {
  result<-new("TableRowCount")
  result@schema<-schema
  result@rowCount<-as.numeric(rowCount)
  if (!missing(updateEtag)) result@updateEtag<-updateEtag
  result
}

convertDataFrameTypeToSchemaType<-function(dataframe, headers) {
  for (header in headers@content) {
    columnIndex<-match(header@name, names(dataframe))
    if (length(columnIndex)>0 && !is.na(columnIndex)) { # make sure the the column is in the data frame
      if (header@columnType=="BOOLEAN") {
        # Synapse returns values "true", "false", which have to be converted to TRUE, FALSE
        dataframe[[columnIndex]]<-(dataframe[[columnIndex]]=="true")
      } else if (header@columnType=="DATE") {
        dataframe[[columnIndex]]<-as.POSIXct(dataframe[[columnIndex]]/1000, origin="1970-01-01")
      } else if (header@columnType=="INTEGER"){
        dataframe[[columnIndex]] <- as.integer(dataframe[[columnIndex]])
      } else if (header@columnType %in% c("STRING", "FILEHANDLEID", "ENTITYID", "LINK")){
        dataframe[[columnIndex]] <- as.character(dataframe[[columnIndex]])
      } else if (header@columnType=="DOUBLE"){
        dataframe[[columnIndex]] <- as.numeric(dataframe[[columnIndex]])
      }
    }
  }
  dataframe
}

setMethod(
  f = "synStore",
  signature = "TableFilePath",
  definition = function(entity, 
    retrieveData=FALSE, 
    verbose=TRUE,
    filePath=NULL) {
    s3FileHandle<-chunkedUploadFile(entity@filePath)
    synStore(entity=
        Table(
          tableSchema=entity@schema, 
          values=as.integer(s3FileHandle$id),
          linesToSkip=entity@linesToSkip,
          quoteCharacter=entity@quoteCharacter,
          isFirstLineHeader=entity@isFirstLineHeader,
          escapeCharacter=entity@escapeCharacter,
          lineEnd=entity@lineEnd,
          separator=entity@separator,
          updateEtag=entity@updateEtag
        ),
        retrieveData=retrieveData,
        verbose=verbose,
        filePath=filePath)
    
   }
)

setMethod(
  f = "synStore",
  signature = "TableFileHandleId",
  definition = function(entity, 
    retrieveData=FALSE, 
    verbose=TRUE,
    filePath=NULL) {
    entity@schema<-ensureTableSchemaIsRetrieved(entity@schema)
    entity@schema<-ensureTableSchemaStored(entity@schema)
    tableId<-propertyValue(entity@schema, "id")
    rowsProcessed<-uploadFileHandleIdToTable(
      entity@fileHandleId, 
      tableId,
      verbose,
      entity@linesToSkip,
      entity@quoteCharacter,
      entity@isFirstLineHeader,
      entity@escapeCharacter,
      entity@separator,
      entity@lineEnd
    )
    if (retrieveData) {
      sql=sprintf("select * from %s", tableId)
      downloadResult<-downloadTableToCSVFile(sql, verbose, filePath=filePath)
      Table(tableSchema=entity@schema, values=downloadResult$filePath, updateEtag=downloadResult$etag)
    } else {
      TableRowCount(entity@schema, rowsProcessed)
    }
  }
)

# adds content of uploaded file as rows in Table given by TableId
# returns the number of rows processed
uploadFileHandleIdToTable<-function(fileHandleId, tableId, 
  verbose=TRUE, linesToSkip=as.integer(0), quoteCharacter=character(0), isFirstLineHeader=TRUE,
  escapeCharacter=character(0), separator=character(0), lineEnd=character(0), updateEtag=character(0)) {
   
  request<-UploadToTableRequest(
    linesToSkip=linesToSkip,
    csvTableDescriptor=CsvTableDescriptor(
      quoteCharacter=quoteCharacter,
      isFirstLineHeader=isFirstLineHeader,
      escapeCharacter=escapeCharacter,
      separator=separator,
      lineEnd=lineEnd
    ),
    tableId=tableId,
    uploadFileHandleId=as.character(fileHandleId),
    updateEtag=updateEtag
  )
  
  asyncJobId<-createS4ObjectFromList(synRestPOST(sprintf("/entity/%s/table/upload/csv/async/start", tableId), createListFromS4Object(request)) ,"AsyncJobId")
  responseBodyAsList<-trackProgress(sprintf("/entity/%s/table/upload/csv/async/get/%s", tableId, asyncJobId@token), verbose)
  responseBody<-createS4ObjectFromList(responseBodyAsList, "UploadToTableResult")
  if (verbose) cat(sprintf("Complete.  Processed %d rows.\n", responseBody@rowsProcessed))
  responseBody@rowsProcessed
}

trackProgress<-function(checkCompleteUri, verbose=TRUE, endpoint="REPO") {
  asyncJobState<-"PROCESSING"
  startTime<-Sys.time()
  maxWaitSeconds<-60
  lastProgressCurrent<-as.integer(-1)
  while (asyncJobState=="PROCESSING") {
    curlHandle=getCurlHandle()
    checkResultAsList<-synapseGet(uri=checkCompleteUri, endpoint=synapseServiceEndpoint(endpoint), curlHandle=curlHandle, checkHttpStatus=FALSE)
    statusCode<-getStatusCode(curlHandle)
    if (statusCode==202) {
      if (is.null(checkResultAsList$progressCurrent)) {
        # remove when PLFM-3008 is fixed
        checkResultAsList$progressCurrent<-as.integer(0)
      }
      jobStatus<-createS4ObjectFromList(checkResultAsList, "AsynchronousJobStatus")
      asyncJobState<-jobStatus@jobState # PROCESSING, FAILED, or COMPLETE
      if (asyncJobState!="PROCESSING") break
      if (Sys.time()-startTime>maxWaitSeconds) stop(sprintf("Failed to obtain result after %s seconds.", maxWaitSeconds))
      if (verbose) {
        if (jobStatus@progressCurrent>lastProgressCurrent) {
          cat(sprintf("\nCompleted %.1f%%.", as.numeric(jobStatus@progressCurrent)/as.numeric(jobStatus@progressTotal)*100))
        } else {
          cat(".")
        }
      }
      lastProgressCurrent<-jobStatus@progressCurrent
      Sys.sleep(1);
    } else {
      # this handles non-2xx statuses
      .checkCurlResponse(object=curlHandle, response=toJSON(checkResultAsList))
      # the job is finished
      if (verbose) cat("\n")
      return(checkResultAsList)
    }
  }
  if (verbose) cat("\n")
  if (asyncJobState=="FAILED") stop(sprintf("%s\nDetails:\n%s", jobStatus@errorMessage, jobStatus@errorDetails))
  stop(sprintf("Unexcepted status %s for %s", asyncJobState, checkCompleteUri))
}

# execute a query and download the results
# returns the download file path and etag
downloadTableToCSVFile<-function(sql, verbose, includeRowIdAndRowVersion=TRUE, filePath=NULL) {
  tableId<-findSynIdInSql(sql)
  request<-DownloadFromTableRequest(sql=sql, includeRowIdAndRowVersion=includeRowIdAndRowVersion, writeHeader=TRUE)
  asyncJobId<-createS4ObjectFromList(synRestPOST(sprintf("/entity/%s/table/download/csv/async/start", tableId), createListFromS4Object(request)) ,"AsyncJobId")
  responseBodyAsList<-trackProgress(sprintf("/entity/%s/table/download/csv/async/get/%s", tableId, asyncJobId@token), verbose)
  responseBody<-createS4ObjectFromList(responseBodyAsList, "DownloadFromTableResult")
  downloadUri<-sprintf("/fileHandle/%s/url?redirect=FALSE", responseBody@resultsFileHandleId)
  if (is.null(filePath)) {
    fileName<-sprintf("queryResult_%s.csv", responseBody@resultsFileHandleId)
    downloadLocation<- NULL
  } else {
    fileName <- basename(filePath)
    downloadLocation <- dirname(filePath)
  }
  fileHandle<-S3FileHandle(id=responseBody@resultsFileHandleId, fileName=fileName)
  fileHandleAsList<-createListFromS4Object(fileHandle)
  filePath<-downloadFromServiceWithCaching(downloadUri, "FILE", fileHandleAsList$id, NULL, downloadLocation, ifcollision="overwrite.local")
  list(filePath=filePath, etag=responseBody@etag, headers=responseBody@headers)
}

loadCSVasDataFrame<-function(filePath) {
  if (file.info(filePath)$size==0) {
    # this may occur if the table or query result set is empty
    return(data.frame())
  }
  dataframe<-readDataFrameFromCSV(filePath)
  rowIdIndex<-match("ROW_ID", names(dataframe))
  rowVersionIndex<-match("ROW_VERSION", names(dataframe))
  if (!is.na(rowIdIndex) && !is.na(rowVersionIndex)) {
    # the read-in dataframe has row numbers and versions to remove
    # 'drop=FALSE' is needed to avoid converting a one-column data frame into a vector (SYNR-828)
    strippedframe<-dataframe[,-c(rowIdIndex, rowVersionIndex), drop=FALSE]
    # use the two stripped columns as the row names
    row.names(strippedframe)<-paste(dataframe[[rowIdIndex]], dataframe[[rowVersionIndex]], sep="_")
    strippedframe
  } else {
    dataframe
  }
}

findSynIdInSql<-function(sqlString) {
  lowerSql<-tolower(sqlString)
  fromIndex<-regexpr("from(\\s)+syn", lowerSql)[1]
  if (fromIndex<0) stop(sprintf("Not a valid table query: %s", sqlString))
  fromString<-substring(lowerSql, fromIndex)
  synIndex<-regexpr("syn", fromString)[1]
  synString<-substring(fromString, synIndex)
  endIndex<-regexpr("(\\s)+", synString)
  if (endIndex<0) {
    result<-synString
  } else {
    result<-substring(synString, 1, endIndex-1)
  }
  if (!isSynapseId(result)) stop(sprintf("Could not find Synapse ID in %s", sqlString))
  result
}

# execute a query against a table
# if loadResult=T, return a TableDataFrame, else return
# a TableFilePath (i.e. providing the path to the query result)
synTableQuery<-function(sqlString, loadResult=TRUE, verbose=TRUE, filePath=NULL) {
  tableId<-findSynIdInSql(sqlString)
  downloadResult<-downloadTableToCSVFile(sql=sqlString, verbose=verbose, includeRowIdAndRowVersion=TRUE, filePath=filePath)
  if (loadResult) {
    # if it's an aggregation query there are no row labels
    dataframe<-loadCSVasDataFrame(downloadResult$filePath)
    dataframe<-convertDataFrameTypeToSchemaType(dataframe, downloadResult$headers)
    Table(tableSchema=tableId, values=dataframe, updateEtag=downloadResult$etag)
  } else {
    Table(tableSchema=tableId, values=downloadResult$filePath, updateEtag=downloadResult$etag)
  }
}

synDeleteRows<-function(tableDataFrame) {
  schema<-tableDataFrame@schema
  if (is(schema, "TableSchema")) {
    tableId<-propertyValue(schema, "id")
  } else if (is (schema, "character")) {
    tableId<-schema
  }
  rowIds<-parseRowAndVersion(row.names(tableDataFrame@values))[1,]
  request<-RowSelection(tableId=tableId, etag=tableDataFrame@updateEtag, rowIds=rowIds)
  responseBodyAsList<-synRestPOST(sprintf("/entity/%s/table/deleteRows", tableId), createListFromS4Object(request))
  response<-createS4ObjectFromList(responseBodyAsList, "RowReferenceSet")
  TableRowCount(tableDataFrame@schema, length(response@rows), response@etag)
}

getFileHandleIdFromTableCell<-function(tableId, selectColumn, rowId, versionNumber) {
	selectColumnList<-SelectColumnList(selectColumn)
	rowReferenceList<-RowReferenceList(RowReference(rowId=rowId, versionNumber=versionNumber))
	rowReferenceSet<-RowReferenceSet(
			tableId=tableId,
			headers=selectColumnList,
			rows=rowReferenceList
	)
	result<-synRestPOST(sprintf("/entity/%s/table/filehandles", tableId), createListFromS4Object(rowReferenceSet))
	tableFileHandleResults<-createS4ObjectFromList(result, "TableFileHandleResults")
	if (length(tableFileHandleResults@rows)!=1) stop(sprintf("Expected one row but found %s.", length(tableFileHandleResults@rows)))
	row<-tableFileHandleResults@rows[[1]]@list
	if (length(row)!=1) stop(sprintf("Expected one column but found %s.", length(row)))
	fileHandle<-row[[1]]
	fileHandleId<-fileHandle@id
	if (is.null(fileHandleId)) stop("fileHandleId is null")
	fileHandleId
}

# NOTE:  This function is deprecated
synDownloadTableFile<-function(table, rowIdAndVersion, columnName, downloadLocation=NULL, ifcollision="keep.both") {
	pair<-parseRowAndVersion(rowIdAndVersion)
	rowId<-pair[1]
	versionNumber<-pair[2]
	# first get the tableId
	if (is(table, "character")) {
		if (!isSynapseId(table)) stop(sprintf("%s is not a Synapse ID.", table))
		tableId<-table
	} else if (is(table, "Table")) {
		if (is(table@schema, "character")) {
			if (!isSynapseId(table@schema)) stop(sprintf("%s is not a Synapse ID.", table@schema))
			tableId<-table@schema
		} else if (is(table@schema, "TableSchema")) {
			tableId<-propertyValue(table@schema, "id")
		} else {
			stop(sprintf("Unexpected type: %s", class(table@schema)[[1]]))
		}
	} else {
		stop(sprintf("Unexpected type %s", class(table)))
	}
	# second get the columnId
	columns<-synGetColumns(table)
	selectColumn<-NULL
	for (column in columns@content) {
		if (column@name==columnName) {
			if (!is.null(selectColumn)) stop(sprintf("Multiple columns match %s", columnName))
			selectColumn<-SelectColumn(id=column@id, columnType=column@columnType, name=column@name)
		}
	}
	if (is.null(selectColumn)) stop(sprintf("Specified column, %s, not associated with entity %s.", columnName, tableId))
	columnId<-selectColumn@id
	# third, get the fileHandleId
	if (is(table, "character")) {
		fileHandleId<-getFileHandleIdFromTableCell(tableId, selectColumn, rowId, versionNumber)
	} else if (is(table, "Table")) {
		if (is(table, "TableDataFrame")) {
			dataframe<-table@values
		} else if (is(table, "TableFilePath")) {
			dataframe<-loadCSVasDataFrame(table@filePath)
		} else {
			stop(sprintf("Unexpected type %s", class(table)))
		}
		# if possible get the fileHandleId from the in-memory data frame
		fileHandleId<-dataframe[rowIdAndVersion, columnName]
		# if the in-memory data doesn't overlap with the row/column of interest we will get
		# NULL back.  In this case we query to get the fileHandleId of interest
		if (is.null(fileHandleId)) {
			fileHandleId<-getFileHandleIdFromTableCell(tableId, selectColumn, rowId, versionNumber)
		}
	} else {
		stop(sprintf("Unexpected type %s", class(table)))
	}
	uri<-sprintf("/entity/%s/table/column/%s/row/%s/version/%s/file?redirect=FALSE", tableId, columnId, rowId, versionNumber)
	downloadFromServiceWithCaching(uri, "REPO", fileHandleId, NULL, downloadLocation, ifcollision)
}


