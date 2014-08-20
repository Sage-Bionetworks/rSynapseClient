# Constructors and other methods for the various classes of Table
# Note:  These classes are all suitable for uploading to Synapse, but don't
# have common accessor methods for accessing their data.
# 
# Author: brucehoff
###############################################################################

setMethod(
  f = "Table",
  signature = signature("TableSchema", "RowList"),
  definition = function(tableSchema, values) {
    result<-new("TableRowList")
    result@schema<-tableSchema
    result@values<-values
    result
  }
)

setMethod(
  f = "Table",
  signature = signature("TableSchema", "matrix"),
  definition = function(tableSchema, values) {
    result<-new("TableMatrix")
    result@schema<-tableSchema
    result@values<-values
    result
  }
)

setMethod(
  f = "Table",
  signature = signature("TableSchema", "data.frame"),
  definition = function(tableSchema, values) {
    result<-new("TableDataFrame")
    result@schema<-tableSchema
    result@values<-values
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


# Given a schemaId, return the max number of rows that 
# can be uploaded or downloaded at a time (assuming that
# each row includes all the fields in the schema
maxRowTransferForSchema<-function(schemaId) {
  if (!is(schemaId, "character") || length(schemaId)>1) stop("Illegal value for schemaid")
  # mask to retrieve max rows is x1000 or 4096 (decimal)
  bundle<-synRestGET(sprintf("/entity/%s/bundle?mask=4096",schemaId))
  tableDataList<-bundle$tableBundle
  tableData<-createS4ObjectFromList(tableDataList, "TableBundle")
  tableData$maxRowsPerPage
}

# Given a schema and a list of columns, return the max number of rows that 
# can be uploaded or downloaded at a time
maxRowTransferForSchemaAndColumnNames<-function(schemaId, columnNames) {
  # to get the maxRowsPerPage use mask=8
  queryString<-sprintf("select %s from %s", paste(columnNames, collapse=","), schemaId)
  query<-Query(sql=queryString)
  bundleAsList<-synRestPOST("/table/query/bundle?partsMask=8", createListFromS4Object(query))
  bundle<-createS4ObjectFromList(bundleAsList, "QueryResultBundle")
  bundle@maxRowsPerPage
}

setMethod(
  f = "synStore",
  signature = signature("TableRowSet"),
  definition = function(entity, retrieveData=FALSE, verbose=TRUE) {
    
    id<-entity@tableId # character
    columnIds<-entity@headers # CharacterList
    etag<-entity@etag # character 
    rows<-entity@rows # RowList
    # validate input
    headerCount<-length(columnIds)
    r<-length(rows)
    if (r<1) stop("Missing rows.")
    for (i in 1:r) {
      rowValueCount<-length(rows[[i]]$values)
      if (rowValueCount!=headerCount) {
        stop(sprintf("Expected %d values for row %d but found %d", headerCount, i, rowValueCount))
      }
    }
    # store the values
    # TODO it would be nice to do the following for just the columns being uploaded,
    # but doing so requires retrieving all the column names.  For expediency we just
    # get the max rows when uploading _all_ columns.
    maxRowsPerRequest<-maxRowTransferForSchema(id)
    # updates can't be broken up into chunks
    if (maxRowsPerRequest<r && length(etag)>0) stop("Row set is too large for an update operation.")
    currentRow<-1
    rowReferenceSet<-RowReferenceSet(rows=RowReferenceList())
    tableRowSet<-TableRowSet(rows=RowList())
    while (currentRow<=r) {
      rowLimit<-min(r, currentRow+maxRowsPerRequest)
      chunk<-RowList()
      chunk@content<-rows@content[currentRow:rowLimit]
      tableRowSetChunk<-TableRowSet(headers=columnIds, etag=etag, tableId=id, rows=chunk)
      if (verbose) cat(sprintf("Sending rows %d through %d of %d for TableSchema %s.\n", currentRow, rowLimit, r, id))
      rowReferenceSetChunkAsList<-synRestPOST(sprintf("/entity/%s/table", id), createListFromS4Object(tableRowSetChunk))
      
      if (retrieveData) {
        # retrieve the data into a TableRowSet
        tableRowSetChunkAsList<-synRestPOST(sprintf("/entity/%s/table/getRows", id), rowReferenceSetChunkAsList)
          tableRowSetChunk<-createS4ObjectFromList(tableRowSetChunkAsList, "TableRowSet")
        tableRowSet@tableId<-tableRowSetChunk@tableId
        if (r<=maxRowsPerRequest) tableRowSet@etag<-tableRowSetChunk@etag
        tableRowSet@headers<-tableRowSetChunk@headers
        # TODO implement a strongly typed version of this
        tableRowSet@rows@content<-append(tableRowSet@rows@content, tableRowSetChunk@rows@content)
      } else {
        rowReferenceSetChunk<-createS4ObjectFromList(rowReferenceSetChunkAsList, "RowReferenceSet")
        rowReferenceSet@tableId<-rowReferenceSetChunk@tableId
        if (r<=maxRowsPerRequest) rowReferenceSet@etag<-rowReferenceSetChunk@etag
        rowReferenceSet@headers<-rowReferenceSetChunk@headers
        # TODO implement a strongly typed version of this
        rowReferenceSet@rows@content<-append(rowReferenceSet@rows@content, rowReferenceSetChunk@rows@content)
      }
      currentRow<-rowLimit+1
    }
    if (retrieveData) {
      tableRowSet
    } else {
      rowReferenceSet
    }
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

setMethod(
  f = "synStore",
  signature = "TableRowList",
  definition = function(entity, retrieveData=FALSE, verbose=TRUE) {
    tableSchema<-ensureTableSchemaStored(entity@schema)
    # create a CharacterList from a character vector
    headers<-createTypedList(propertyValue(tableSchema, "columnIds"))
    tableRowSet<-TableRowSet(tableId=propertyValue(tableSchema, "id"), headers=headers, rows=entity@values)
    synStore(tableRowSet, retrieveData, verbose)
  }
)

synGetColumns<-function(id) {
  listResult<-synRestGET(sprintf("/entity/%s/column", id))
  objectResult<-createS4ObjectFromList(listResult, "PaginatedColumnModels")
  objectResult@results
}

storeMatrix<-function(tableSchema, matrix, retrieveData, verbose) {
  if (nrow(matrix)<1 | ncol(matrix)<1) stop("Matrix is empty.")
  if (is.null(colnames(matrix))) stop("Matrix must have column names.")
  
  tableSchema<-ensureTableSchemaStored(tableSchema)
  
  # map column names to ids
  schemaColumns<-synGetColumns(propertyValue(tableSchema,"id"))
  schemaColumnMap<-list()
  for (column in schemaColumns@content) schemaColumnMap[[column@name]]<-column@id
  
  # get the order of the TableColumns
  headers<-CharacterList()
  for (matrixColumnName in colnames(matrix)) {
    schemaColumnId<-schemaColumnMap[[matrixColumnName]]
    if (is.null(schemaColumnId)) stop(sprintf("Matrix has column %s but schema has no such column.", matrixColumnName))
    headers<-add(headers, schemaColumnId)
  }
  
  # now build up the Rows
  rowList<-RowList()
  for (i in 1:nrow(matrix)) {
    rowList<-add(rowList, Row(values=createTypedList(as.character(matrix[i,]))))
  }
  tableRowSet<-TableRowSet(tableId=propertyValue(tableSchema, "id"), headers=headers, rows=rowList)
  synStore(tableRowSet, retrieveData, verbose)
}

setMethod(
  f = "synStore",
  signature = "TableMatrix",
  definition = function(entity, retrieveData=FALSE, verbose=TRUE) {
    storeMatrix(entity@schema, entity@values, retrieveData, verbose)
  }
)

setMethod(
  f = "synStore",
  signature = "TableDataFrame",
  definition = function(entity, retrieveData=FALSE, verbose=TRUE) {
    storeMatrix(entity@schema, as.matrix(entity@values), retrieveData, verbose)
  }
)

setMethod(
  f = "synStore",
  signature = "TableFilePath",
  definition = function(entity, 
    retrieveData=FALSE, 
    verbose=TRUE) {
    # TODO update this message once Table Query is implemented
    if (retrieveData) stop("To retrieve uploaded data please use the Table Query command.")
    s3FileHandle<-chunkedUploadFile(entity@filePath)
    request<-AsynchUploadToTableRequestBody(
      tableId=propertyValue(entity@schema, "id"),
      concreteType="org.sagebionetworks.repo.model.table.AsynchUploadToTableRequestBody",
      uploadFileHandleId=s3FileHandle$id,
      linesToSkip=entity@linesToSkip,
      quoteCharacter=entity@quoteCharacter,
      escapeCharacter=entity@escapeCharacter,
      separator=entity@separator
    )
    jobStatusAsList<-synRestPOST("/asynchronous/job", createListFromS4Object(request))
    jobStatus<-createS4ObjectFromList(jobStatusAsList, "AsynchronousJobStatus")
    asyncJobState<-jobStatus@jobState # PROCESSING, FAILED, or COMPLETE
    while (asyncJobState=="PROCESSING") {
      if (verbose) cat(sprintf("Completd %d of %d.  %s\n", 
          jobStatus@progressCurrent, jobStatus@progressTotal, jobStatus@progressMessage))
      jobStatus<-createS4ObjectFromList(
        synRestGET(sprintf("/asynchronous/job/{jobId}", jobStatus$jobId)), "AsynchronousJobStatus")
      if (asyncJobState=="PROCESSING") Sys.sleep(1);
    }
    if (asyncJobState=="FAILED") stop(jobStatus@errorMessage)
    # TODO log jobStatus@errorDetails to the new client logging service
    rowsProcessed<-jobStatus@responseBody@rowsProcessed
    if (verbose) cat(sprintf("Complete.  Processed %d rows.\n", rowsProcessed))
    rowsProcessed
  }
)

