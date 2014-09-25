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
    cat(sprintf("data frame columns:\n%s", paste(names(object@values), collapse="\n")))
  }
)

setMethod(
  f = "show",
  signature = signature("TableFilePath"),
  definition = function(object){
    cat('An object of class "', class(object), '"\n', sep="")
    showSchemaOrEntityId(object@schema)
    cat(sprintf("filePath:\n%s", object@filePath))
  }
)

setMethod(
  f = "show",
  signature = signature("TableRowCount"),
  definition = function(object){
    cat('An object of class "', class(object), '"\n', sep="")
    showSchemaOrEntityId(object@schema)
    cat(sprintf("row count:\n%s", object@rowCount))
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
    escapeCharacter,
    separator,
    updateEtag) {
    result<-new("TableFilePath")
    result@schema<-tableSchema
    result@filePath<-values
    if (!missing(linesToSkip)) result@linesToSkip<-linesToSkip
    if (!missing(quoteCharacter)) result@quoteCharacter<-quoteCharacter
    if (!missing(escapeCharacter)) result@escapeCharacter<-escapeCharacter
    if (!missing(separator)) result@separator<-separator
    if (!missing(updateEtag)) result@updateEtag<-updateEtag
    result
  }
)

setMethod(
  f = "Table",
  signature = signature("TableSchemaOrCharacter", "integer"),
  definition = function(tableSchema, values, updateEtag) {
    result<-new("TableRowCount")
    result@schema<-tableSchema
    result@rowCount<-values
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

synGetColumns<-function(id) {
  listResult<-synRestGET(sprintf("/entity/%s/column", id))
  objectResult<-createS4ObjectFromList(listResult, "PaginatedColumnModels")
  objectResult@results
}

parseRowAndVersion<-function(x) {
  parsed<-strsplit(x, "-", fixed=T)
  parsedLengths<-sapply(X=parsed, FUN=length)
  lengthNotTwo<-parsedLengths!=2
  if (any(lengthNotTwo)) {
    stop(sprintf("Invalid row labels as position(s) %s", paste(which(lengthNotTwo), collapse=",")))
  }
  parsedAsInteger<-sapply(X=parsed, FUN=as.integer)
  rowHasNa<-apply(X=parsedAsInteger, MARGIN=2, FUN=function(x){any(is.na(x))})
  if(any(rowHasNa)) {
    stop(sprintf("Non-numeric row labels as position(s) %s.", paste(which(rowHasNa), collapse=",")))
  }
  rowHasNonInt<-sapply(X=parsed, FUN=function(x){any(as.integer(x)!=as.numeric(x))})
  if (any(rowHasNonInt)) {
    stop(sprintf("Non integer in row label as position(s) %s.", paste(which(rowHasNonInt), collapse=",")))
  }
  parsedAsInteger
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
  schemaColumns<-synGetColumns(propertyValue(tableSchema,"id"))
  schemaColumnMap<-list()
  for (column in schemaColumns@content) schemaColumnMap[[column@name]]<-column
  for (dfColumnName in names(dataframe)) {
    schemaColumn<-schemaColumnMap[[dfColumnName]]
    if (is.null(schemaColumn)) stop(sprintf("Data frame has column %s but schema has no such column.", dfColumnName))
    dfColumnType<-class(dataframe[[dfColumnName]])[1]
    expectedTableColumnTypes<-getTableColumnTypeForDataFrameColumnType(dfColumnType)
    tableColumnType<-schemaColumn@columnType
    if (!any(tableColumnType==expectedTableColumnTypes)) {
      stop(sprintf("Column %s has type %s but %s is expected.", dfColumnName, tableColumnType, 
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
  # use an anonymous ‘file()’ connection to collect output."
  filePath<-tempfile()
  writeDataFrameToCSV(dataFrameToWrite, filePath)
  rowsProcessed<-uploadCSVFileToTable(filePath=filePath, tableId=propertyValue(tableSchema, "id"), verbose=verbose, updateEtag=updateEtag)
}

writeDataFrameToCSV<-function(dataFrame, filePath) {
  write.csv(x=dataFrame, file=filePath, row.names=FALSE, na="")
}

readDataFrameFromCSV<-function(filePath) {
  read.csv(filePath, encoding="UTF-8")
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
      dataframe<-convertDataFrameTypeToSchemaType(dataframe, tableId)
      Table(entity@schema, dataframe, downloadResult$etag)
    } else {
      Table(entity@schema, rowsProcessed)
    }
  }
)

convertDataFrameTypeToSchemaType<-function(dataframe, tableId) {
  columns<-synGetColumns(tableId)
  for (columnModel in columns@content) {
    columnIndex<-match(columnModel@name, names(dataframe))
    if (!is.na(columnIndex)) { # make sure the the column is in the data frame
      if (columnModel@columnType=="BOOLEAN") {
        # Synapse returns values "true", "false", which have to be converted to TRUE, FALSE
        dataframe[[columnIndex]]<-(dataframe[[columnIndex]]=="true")
      } else if (columnModel@columnType=="DATE") {
        dataframe[[columnIndex]]<-as.Date(dataframe[[columnIndex]]/(24*3600*1000), "1970-01-01")
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
    entity@schema<-ensureTableSchemaIsRetrieved(entity@schema)
    entity@schema<-ensureTableSchemaStored(entity@schema)
    tableId<-propertyValue(entity@schema, "id")
    rowsProcessed<-uploadCSVFileToTable(
      entity@filePath, 
      tableId,
      verbose,
      entity@linesToSkip,
      entity@quoteCharacter,
      entity@escapeCharacter,
      entity@separator
    )
    if (retrieveData) {
      sql=sprintf("select * from %s", tableId)
      downloadResult<-downloadTableToCSVFile(sql, verbose, filePath=filePath)
      Table(tableSchema=entity@schema, values=downloadResult$filePath, updateEtag=downloadResult$etag)
    } else {
      Table(entity@schema, rowsProcessed)
    }
  }
)

# upload CSV file to the given table ID
# returns the number of rows processed
uploadCSVFileToTable<-function(filePath, tableId, 
  verbose=TRUE, linesToSkip=as.integer(0), quoteCharacter=character(0), 
  escapeCharacter=character(0), separator=character(0), lineEnd=character(0), updateEtag=character(0)) {
  s3FileHandle<-chunkedUploadFile(filePath)
  
  request<-UploadToTableRequest(
    linesToSkip=linesToSkip,
    csvTableDescriptor=CsvTableDescriptor(
      quoteCharacter=quoteCharacter,
      escapeCharacter=escapeCharacter,
      separator=separator,
      lineEnd=lineEnd
      ),
    tableId=tableId,
    uploadFileHandleId=s3FileHandle$id,
    updateEtag=updateEtag
  )

  asyncJobId<-createS4ObjectFromList(synRestPOST("/table/upload/csv/async/start", createListFromS4Object(request)) ,"AsyncJobId")
  responseBodyAsList<-trackProgress(sprintf("/table/upload/csv/async/get/%s", asyncJobId@token), verbose)
  responseBody<-createS4ObjectFromList(responseBodyAsList, "UploadToTableResult")
  if (verbose) cat(sprintf("Complete.  Processed %d rows.\n", responseBody@rowsProcessed))
  responseBody@rowsProcessed
}

trackProgress<-function(checkCompleteUri, verbose=TRUE) {
  asyncJobState<-"PROCESSING"
  startTime<-Sys.time()
  maxWaitSeconds<-60
  lastProgressCurrent<-as.integer(-1)
  while (asyncJobState=="PROCESSING") {
    curlHandle=getCurlHandle()
    checkResultAsList<-synapseGet(uri=checkCompleteUri, curlHandle=curlHandle, checkHttpStatus=FALSE)
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
      .checkCurlResponse(curlHandle, toJSON(checkResultAsList))
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
  request<-DownloadFromTableRequest(sql=sql, includeRowIdAndRowVersion=includeRowIdAndRowVersion, writeHeader=TRUE)
  asyncJobId<-createS4ObjectFromList(synRestPOST("/table/download/csv/async/start", createListFromS4Object(request)) ,"AsyncJobId")
  responseBodyAsList<-trackProgress(sprintf("/table/download/csv/async/get/%s", asyncJobId@token), verbose)
  responseBody<-createS4ObjectFromList(responseBodyAsList, "DownloadFromTableResult")
  downloadUri<-sprintf("/fileHandle/%s/url", responseBody@resultsFileHandleId)
  if (is.null(filePath)) {
    fileName<-sprintf("queryResult_%s.csv", responseBody@resultsFileHandleId)
    downloadLocation<- NULL
  } else {
    fileName <- basename(filePath)
    downloadLocation <- dirname(filePath)
  }
  fileHandle<-S3FileHandle(id=responseBody$resultsFileHandleId, fileName=fileName)
  fileHandleAsList<-createListFromS4Object(fileHandle)
  downloadResult<-synGetFileAttachment(downloadUri, "FILE", fileHandleAsList, downloadFile=T, downloadLocation=downloadLocation, ifcollision="overwrite.local", load=F)
  list(filePath=downloadResult$filePath, etag=responseBody@etag)
}

loadCSVasDataFrame<-function(filePath, includeRowIdAndRowVersion=TRUE) {
  dataframe<-readDataFrameFromCSV(filePath)
  if (includeRowIdAndRowVersion) {
    # the read-in dataframe has row numbers and versions to remove
    rowIdIndex<-match("ROW_ID", names(dataframe))
    if (is.na(rowIdIndex)) stop("Could not find ROW_ID column in data frame.")
    rowVersionIndex<-match("ROW_VERSION", names(dataframe))
    if (is.na(rowVersionIndex)) stop("Could not find ROW_VERSION column in data frame.")
    strippedframe<-dataframe[,-c(rowIdIndex, rowVersionIndex)]
    # use the two stripped columns as the row names
    row.names(strippedframe)<-paste(dataframe[[rowIdIndex]], dataframe[[rowVersionIndex]], sep="-")
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

# aggregation queries 
isAggregationQuery<-function(sql) {
  lowerSql<-tolower(sql)
  selectIndex<-regexpr("select", lowerSql)[1]
  if (selectIndex<0) stop(sprintf("Not a valid table query: %s", sql))
  selectString<-substring(lowerSql, selectIndex+nchar("select"))
  
  fromIndex<-regexpr("from", selectString)[1]
  if (fromIndex<0) stop(sprintf("Not a valid table query: %s", sql))
  fromString<-substring(selectString, 1, fromIndex-1)
  
  regexpr("(count|max|min|avg|sum)(\\s)*\\(", tolower(fromString))[1]>=0
}

# execute a query against a table
# if loadResult=T, return a TableDataFrame, else return
# a TableFilePath (i.e. providing the path to the query result)
synTableQuery<-function(sqlString, loadResult=TRUE, verbose=TRUE, filePath=NULL) {
  isAggregationQuery<-isAggregationQuery(sqlString)
  tableId<-findSynIdInSql(sqlString)
  downloadResult<-downloadTableToCSVFile(sql=sqlString, verbose=verbose, includeRowIdAndRowVersion=!isAggregationQuery, filePath=filePath)
  if (loadResult) {
    # if it's an aggregation query there are no row labels
    dataframe<-loadCSVasDataFrame(downloadResult$filePath, !isAggregationQuery)
    dataframe<-convertDataFrameTypeToSchemaType(dataframe, tableId)
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
  rowIds<-IntegerList()
  rowIds@content<-as.list(parseRowAndVersion(row.names(tableDataFrame@values))[1,])
  request<-RowSelection(tableId=tableId, etag=tableDataFrame@updateEtag, rowIds=rowIds)
  responseBodyAsList<-synRestPOST(sprintf("/entity/%s/table/deleteRows", tableId), createListFromS4Object(request))
  response<-createS4ObjectFromList(responseBodyAsList, "RowReferenceSet")
  Table(tableDataFrame@schema, length(response@rows), response@etag)
}


