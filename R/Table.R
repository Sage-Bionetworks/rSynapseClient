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
    separator) {
    result<-new("TableFilePath")
    result@schema<-tableSchema
    result@filePath<-values
    if (!missing(linesToSkip)) result@linesToSkip<-linesToSkip
    if (!missing(quoteCharacter)) result@quoteCharacter<-quoteCharacter
    if (!missing(escapeCharacter)) result@escapeCharacter<-escapeCharacter
    if (!missing(separator)) result@separator<-separator
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
  
  filePath<-tempfile()
  if (length(updateEtag)==0) {
    dataFrameToWrite<-dataframe
  } else {
    # TODO:  use <row>-<version> as the format, not integer
    
    # validate the row headers.  If present they must be integral.
    rownamesAsInteger<-as.integer(rownames(dataframe))
    if (any(is.na(rownamesAsInteger))) {
      stop("Row labels should all be integers, but one or more row labels is not numeric.")
    }
    if (!all(as.integer(rownames(dataframe))==as.numeric(rownames(dataframe)))) {
      stop("One or more rows labels is not an integer.")
    }
    # Now we append the rows labels
    row<-rownames(rownamesAsInteger)
    # Calling the appended vector 'row' causes the column label to be 'row' as well.
    dataFrameToWrite<-cbind(row, dataframe)
    # this allows us to control the column label for the row column. 
  }
  # we would prefer to serialize in memory but R doesn't support connections 
  # wrapping strings/byte arrays, so instead we serialize to a file
  write.csv(x=dataFrameToWrite, file=filePath, row.names=FALSE)
  rowsProcessed<-uploadCSVFileToTable(filePath=filePath,tableId=propertyValue(tableSchema, "id"), updateEtag=updateEtag)
}

setMethod(
  f = "synStore",
  signature = "TableDataFrame",
  definition = function(entity, retrieveData=FALSE, verbose=TRUE) {
    rowsProcessed<-storeDataFrame(entity@schema, entity@values, retrieveData, verbose, entity@updateEtag)
    if (retrieveData) {
      sql=sprintf("select * from %s", tableId)
      downloadResult<-downloadTableToCSVFile(sql)
      dataframe<-loadCSVasDataFrame(downloadResult$filePath)
      TableFilePath(entity@schema, dataframe, downloadResult$etag)
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
    tableId<-propertyValue(entity@schema, "id")
    uploadCSVFileToTable(
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
      downloadResult<-downloadTableToCSVFile(sql)
      # TODO is the downloaded file in a form that can later be uploaded?
      TableFilePath(entity@schema, downloadResult$filePath, downloadResult$etag)
    } else {
      rowsProcessed
    }
  }
)

# upload CSV file to the given table ID
# returns the number of rows processed
uploadCSVFileToTable<-function(filePath, tableId, 
  verbose=TRUE, linesToSkip=character(0), quoteCharacter=character(0), 
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

submitJobAndTrackProgress<-function(request, verbose) {
  jobStatusAsList<-synRestPOST("/asynchronous/job", createListFromS4Object(request))
  jobStatus<-createS4ObjectFromList(jobStatusAsList, "AsynchronousJobStatus")
  asyncJobState<-jobStatus@jobState # PROCESSING, FAILED, or COMPLETE
  while (asyncJobState=="PROCESSING") {
    if (verbose) cat(sprintf("Completed %d of %d.  %s\n", 
          jobStatus@progressCurrent, jobStatus@progressTotal, jobStatus@progressMessage))
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
downloadTableToCSVFile<-function(sql) {
  request<-AsyncDownloadFromTableRequestBody(sql=sql)
  responseBody<-submitJobAndTrackProgress(request, verbose)
  # returns a AsyncDownloadFromTableResponseBody
  downloadUri<-sprintf("/fileHandle/%s/url", responseBody@resultsFileHandleId)
  fileName = sprintf("queryResult_%d.csv", responseBody@resultsFileHandleId)
  fileHandle<-S3FileHandle(id=responseBody$resultsFileHandleId, fileName=fileName)
  fileHandleAsList<-createListFromS4Object(fileHandle)
  downloadResult<-synGetFileAttachment(downloadUri, fileHandleAsList, downloadFile=T, downloadLocation=NULL, ifcollision="overwrite.local", load=F)
  list(filePath=downloadResult$fileName, etag=responseBody@etag)
}

loadCSVasDataFrame<-function(filePath) {
  dataframe<-read.csv(filePath)
  # the read-in dataframe has row numbers and versions to remove
  strippedframe<-dataframe[[-1:-2]]
  # use the two stripped columns as the row names
  row.names(strippedframe)<-paste(dataframe[[1]], dataframe[[2]], sep="-")
  strippedframe
}


