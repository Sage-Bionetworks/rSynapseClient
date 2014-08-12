# Methods for TableValues
# 
# Author: brucehoff
###############################################################################

setMethod(
  f = "Table",
  signature = signature("TableSchema", "RowList"),
  definition = function(tableSchema, values) {
    result<-new("TableValuesRowList")
    result@schema<-tableSchema
    result@values<-values
    result
  }
)

setMethod(
  f = "Table",
  signature = signature("TableSchema", "matrix"),
  definition = function(tableSchema, values) {
    result<-new("TableValuesMatrix")
    result@schema<-tableSchema
    result@values<-values
    result
  }
)

setMethod(
  f = "Table",
  signature = signature("TableSchema", "data.frame"),
  definition = function(tableSchema, values) {
    result<-new("TableValuesDataFrame")
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

setMethod(
  f = "Table",
  signature = signature("TableRowSet"),
  definition = function(tableSchema) {
    result<-tableSchema
    # TODO do we just return the TableRowSet as the result?
    result
  }
)

setMethod(
  f = "synStore",
  signature = "TableValuesRowList",
  definition = function(entity, retrieveData=FALSE, verbose=TRUE) {
    # validate input
    headerCount<-length(entity@schema$columnIds)
    if (length(entity@values)<1) stop("Missing rows.")
    for (i in 1:length(entity@values)) {
      rowValueCount<-length(entity@values[[i]]$values)
      if (rowValueCount!=headerCount) {
        stop(sprintf("Expected %d values for row %d but found %d", headerCount, i, rowValueCount))
      }
    }
    # ensure that the schema is stored
    if (is.null(entity@schema$id)) {
      entity@schema<-synStore(entity@schema)
    }
    # store the values
    headers<-entity@schema$columnIds
    # TODO calculate the maximum row size and break up if necessary
    tableRowSet<-TableRowSet(headers=headers, tableId=entity@schema$id, rows=entity@values)
    rowReferenceSetAsList<-synRestPOST(sprintf("/entity/%s/table", entity@schema$id), tableRowSet)
    if (retrieveData) {
      # TODO if verbose then show progress bar
      # retrieve the data into a TableRowSet
      response<-synRestPOST(sprintf("/entity/%s/table/getRows", entity@schema$id), rowReferenceSetAsList)
      result<-createS4ObjectFromList(response, "TableRowSet")
    } else {
      result<-createS4ObjectFromList(response, "RowReferenceSet")
    }
    result
  }
)

setMethod(
  f = "synStore",
  signature = "TableValuesMatrix",
  definition = function(entity, retrieveData=FALSE, verbose=TRUE) {
    # TODO
  }
)

setMethod(
  f = "synStore",
  signature = "TableValuesDataFrame",
  definition = function(entity, retrieveData=FALSE, verbose=TRUE) {
    # TODO
  }
)

