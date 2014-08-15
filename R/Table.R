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
  definition = function(tableSchema, values) {
    result<-new("TableFilePath")
    result@schema<-tableSchema
    result@filePath<-values
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
  definition = function(entity, retrieveData, verbose) {
#    #get the optional args.  Doesn't see to work to declare them in the line above
#    args = list(...)
#    retrieveData<-args[["retrieveData"]]
#    if (is.null(retrieveData)) retrieveData<-FALSE
#    verbose<-args[["verbose"]]
#    if (is.null(verbose)) verbose<-TRUE
    
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
    if (maxRowsPerRequest<r && length(etag)>0) stop("Row set is too large for an update operation.")
    currentRow<-1
    rowReferenceSet<-RowReferenceSet(rows=RowReferenceList())
    tableRowSet<-TableRowSet(rows=RowList())
    while (currentRow<=r) {
      rowLimit<-min(r, currentRow+maxRowsPerRequest)
      # updates can't be broken up into chunks
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

setMethod(
  f = "synStore",
  signature = "TableRowList",
  definition = function(entity, retrieveData=FALSE, verbose=TRUE) {
    # ensure that the schema is stored
    id<-propertyValue(entity@schema, "id")
    if (is.null(id)) {
      entity@schema<-synStore(entity@schema)
    }
    # create a CharacterList from a character vector
    headers<-createTypedList(propertyValue(entity@schema, "columnIds"))
    tableRowSet<-TableRowSet(tableId=id, headers=headers, rows=entity@values)
    synStore(tableRowSet)
  }
)

setMethod(
  f = "synStore",
  signature = "TableMatrix",
  definition = function(entity, retrieveData=FALSE, verbose=TRUE) {
    stop("Not yet implemented.")
  }
)

setMethod(
  f = "synStore",
  signature = "TableDataFrame",
  definition = function(entity, retrieveData=FALSE, verbose=TRUE) {
    stop("Not yet implemented.")
  }
)

setMethod(
  f = "synStore",
  signature = "TableFilePath",
  definition = function(entity, retrieveData=FALSE, verbose=TRUE) {
    stop("Not yet implemented.")
  }
)

