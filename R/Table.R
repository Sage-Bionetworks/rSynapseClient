# Constructors and other methods for the various classes of Table
# Note:  These classes are all suitable for uploading to Synapse, but don't
# have common accessor methods for accessing their data.
# 
# Author: brucehoff
###############################################################################

setMethod(
  f = "Table",
  signature = signature("TableSchema", "data.frame"),
  definition = function(tableSchema, values, updateEtag=character(0)) {
    result<-new("TableDataFrame")
    result@schema<-tableSchema
    result@values<-values
    result@updateEtag<-updateEtag
    result
  }
)

setMethod(
  f = "Table",
  signature = signature("TableSchema", "character"),
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

ensureTableSchemaStored<-function(tableSchema) {
  # ensure that the schema is stored
  id<-propertyValue(tableSchema, "id")
  if (is.null(id)) {
    tableSchema<-synStore(tableSchema)
  }
  tableSchema
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
  for (column in schemaColumns@content) schemaColumnMap[[column@name]]<-column@id
  for (matrixColumnName in names(dataframe)) {
    schemaColumnId<-schemaColumnMap[[matrixColumnName]]
    if (is.null(schemaColumnId)) stop(sprintf("Data frame has column %s but schema has no such column.", matrixColumnName))
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
  write.csv(x=dataFrameToWrite, file=filePath, row.names=FALSE)
  rowsProcessed<-uploadCSVFileToTable(filePath=filePath, tableId=propertyValue(tableSchema, "id"), verbose=verbose, updateEtag=updateEtag)
}

setMethod(
  f = "synStore",
  signature = "TableDataFrame",
  definition = function(entity, retrieveData=FALSE, verbose=TRUE) {
    entity@schema<-ensureTableSchemaStored(entity@schema)
    rowsProcessed<-storeDataFrame(entity@schema, entity@values, retrieveData, verbose, entity@updateEtag)
    if (retrieveData) {
      tableId<-propertyValue(entity@schema, "id")
      sql<-sprintf("select * from %s", tableId)
      downloadResult<-downloadTableToCSVFile(sql, verbose)
      dataframe<-loadCSVasDataFrame(downloadResult$filePath)
      Table(entity@schema, dataframe, downloadResult$etag)
    } else {
      rowsProcessed
    }
  }
)

setMethod(
  f = "synStore",
  signature = "TableFilePath",
  definition = function(entity, 
    retrieveData=FALSE, 
    verbose=TRUE) {
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
      downloadResult<-downloadTableToCSVFile(sql, verbose)
      Table(tableSchema=entity@schema, values=downloadResult$filePath, updateEtag=downloadResult$etag)
    } else {
      rowsProcessed
    }
  }
)

# upload CSV file to the given table ID
# returns the number of rows processed
uploadCSVFileToTable<-function(filePath, tableId, 
  verbose=TRUE, linesToSkip=as.integer(0), quoteCharacter=character(0), 
  escapeCharacter=character(0), separator=character(0), updateEtag=character(0)) {
  s3FileHandle<-chunkedUploadFile(filePath)
  request<-AsynchUploadToTableRequestBody(
    tableId=tableId,
    uploadFileHandleId=s3FileHandle$id,
    linesToSkip=linesToSkip,
    quoteCharacter=quoteCharacter,
    escapeCharacter=escapeCharacter,
    separator=separator,
    updateEtag=updateEtag
  )
  responseBody<-submitJobAndTrackProgress(request, verbose)
  # returns an AsynchUploadToTableResponseBody
  rowsProcessed<-responseBody@rowsProcessed
  if (verbose) cat(sprintf("Complete.  Processed %d rows.\n", rowsProcessed))
  rowsProcessed
}

submitJobAndTrackProgress<-function(request, verbose=TRUE) {
  jobStatusAsList<-synRestPOST("/asynchronous/job", createListFromS4Object(request))
  jobStatus<-createS4ObjectFromList(jobStatusAsList, "AsynchronousJobStatus")
  asyncJobState<-jobStatus@jobState # PROCESSING, FAILED, or COMPLETE
  while (asyncJobState=="PROCESSING") {
    moreThanZeroProgress <- (jobStatus@progressCurrent>0)
    if (verbose) {
      cat(sprintf("Completed %d of %d.  %s\n", 
          jobStatus@progressCurrent, 
          jobStatus@progressTotal, 
          jobStatus@progressMessage))
    }
    jobStatus<-createS4ObjectFromList(
      synRestGET(sprintf("/asynchronous/job/%s", jobStatus$jobId)), "AsynchronousJobStatus")
    asyncJobState<-jobStatus@jobState
    if (asyncJobState=="PROCESSING") Sys.sleep(1);
  }
  if (asyncJobState=="FAILED") stop(jobStatus@errorMessage)
  # TODO log jobStatus@errorDetails to the new client logging service
  jobStatus@responseBody
}

# execute a query and download the results
# returns the download file path and etag
downloadTableToCSVFile<-function(sql, verbose) {
  request<-AsynchDownloadFromTableRequestBody(sql=sql)
  responseBody<-submitJobAndTrackProgress(request, verbose)
  # returns a AsynchDownloadFromTableResponseBody
  downloadUri<-sprintf("/fileHandle/%s/url", responseBody@resultsFileHandleId)
  fileName<-sprintf("queryResult_%s.csv", responseBody@resultsFileHandleId)
  fileHandle<-S3FileHandle(id=responseBody$resultsFileHandleId, fileName=fileName)
  fileHandleAsList<-createListFromS4Object(fileHandle)
  downloadResult<-synGetFileAttachment(downloadUri, "FILE", fileHandleAsList, downloadFile=T, downloadLocation=NULL, ifcollision="overwrite.local", load=F)
  list(filePath=downloadResult$filePath, etag=responseBody@etag)
}

loadCSVasDataFrame<-function(filePath) {
  dataframe<-read.csv(filePath, encoding="UTF-8")
  # the read-in dataframe has row numbers and versions to remove
  strippedframe<-dataframe[,-1:-2] # could also reference by names "ROW_ID","ROW_VERSION"
  # use the two stripped columns as the row names
  row.names(strippedframe)<-paste(dataframe[[1]], dataframe[[2]], sep="-") # could also reference by names "ROW_ID","ROW_VERSION"
  strippedframe
}


