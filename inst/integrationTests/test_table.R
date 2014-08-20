# test for operations on Tables
# 
# Author: brucehoff
###############################################################################



.setUp <- function() {
  # create project
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)
  
}

.tearDown <- function() {
  # delete the project, cascading to the table
  deleteEntity(synapseClient:::.getCache("testProject"))
  
}

createColumns<-function() {
  
  tableColumns<-list()
  for (i in 1:3) {
    tableColumn<-TableColumn(
      name=sprintf("R_Client_Integration_Test_Column_Name_%d", i), 
      columnType="STRING")
    stored<-synStore(tableColumn)
    tableColumns<-append(tableColumns, stored)
  }
  tableColumns
}

createTableSchema<-function(projectId, tableColumns) {
  name<-sprintf("R_Client_Integration_Test_Create_Schema_%s", sample(999999999, 1))
  
  tableSchema<-TableSchema(name=name, parent=projectId, columns=tableColumns)
  tableSchema<-synStore(tableSchema) # TODO also check the variation in which we don't save the schema in advance of storing the table
  tableSchema
}

integrationTestSynStore <- function() {
  project<-synapseClient:::.getCache("testProject")
  
  tableColumns<-createColumns()
  tableColumnNames<-list()
  for (column in tableColumns) tableColumnNames<-append(tableColumnNames, column@name)
  tableSchema<-createTableSchema(propertyValue(project, "id"), tableColumns)
  
  # test some utilities used by synStore
  id<-propertyValue(tableSchema, "id")
  # don't know what the answer is, just test that it works
  checkTrue(synapseClient:::maxRowTransferForSchema(id)>0)
  checkTrue(synapseClient:::maxRowTransferForSchemaAndColumnNames(id, tableColumnNames)>0)
  
  rowList<-RowList()
  rowList<-add(rowList, Row(values=CharacterList("a1", "b1", "c1")))
  rowList<-add(rowList, Row(values=CharacterList("a2", "b2", "c2")))
  table<-Table(tableSchema, rowList)
  rowReferenceSet<-synStore(table, retrieveData=FALSE, verbose=FALSE)
  checkEquals(rowReferenceSet$tableId, propertyValue(tableSchema, "id"))
  checkTrue(length(rowReferenceSet$etag)>0)
  checkEquals(as.list(propertyValue(tableSchema, "columnIds")), rowReferenceSet$headers@content)
  checkEquals(length(rowReferenceSet$rows), 2)
  
  # rerun with retrieveData=TRUE
  rowList<-RowList()
  rowList<-add(rowList, Row(values=CharacterList("a3", "b3", "c3")))
  rowList<-add(rowList, Row(values=CharacterList("a4", "b4", "c4")))
  table<-Table(tableSchema, rowList)
  tableRowSet<-synStore(table, retrieveData=TRUE, verbose=FALSE)
  checkEquals(tableRowSet$tableId, propertyValue(tableSchema, "id"))
  # checkTrue(length(tableRowSet$etag)>0) restore once PLFM-2947 is fixed
  checkEquals(as.list(propertyValue(tableSchema, "columnIds")), tableRowSet$headers@content)
  checkEquals(length(tableRowSet$rows), 2)
  checkEquals(tableRowSet@rows[[1]]@values@content, list("a3", "b3", "c3"))
  checkEquals(tableRowSet@rows[[2]]@values@content, list("a4", "b4", "c4"))
  
  # now can we update and push back?
  # TODO enable once PLFM-2947 is fixed
  if (FALSE) {
    # this also tests calling synStore directly with a TableRowSet
    tableRowSet@rows[[2]]@values<-CharacterList("A5", "B5", "C5")
    updated<-synStore(tableRowSet, retrieveData=TRUE, verbose=FALSE)
    checkEquals(updated$tableId, propertyValue(tableSchema, "id"))
    checkTrue(length(updated$etag)>0)
    checkEquals(as.list(propertyValue(tableSchema, "columnIds")), updated$headers@content)
    checkEquals(length(updated$rows), 2)
    checkEquals(updated@rows[[1]]@values@content, list("a3", "b3", "c3"))
    checkEquals(updated@rows[[2]]@values@content, list("A5", "B5", "C5"))
  }
}

integrationTestSynStoreTableMatrix <- function() {
  project<-synapseClient:::.getCache("testProject")
  
  tableColumns<-createColumns()
  tableColumnNames<-list()
  for (column in tableColumns) tableColumnNames<-append(tableColumnNames, column@name)
  tableSchema<-createTableSchema(propertyValue(project, "id"), tableColumns)
  
  id<-propertyValue(tableSchema, "id")
  # note we permute the column order in the matrix values and headers, then
  # test that it comes out right
  if (FALSE) {
    # TODO reenable this once PLFM-2954 is fixed
    matrix <- matrix(c("b1", "a1", "c1", "b2", "a2", "c2"), nrow = 2, ncol = 3, byrow = TRUE,
     dimnames = list(c("row1", "row2"), tableColumnNames[c(2,1,3)]))
  } else {
    # the simple version, without rearranging the columns when uploading
    matrix <- matrix(c("a1", "b1", "c1", "a2", "b2", "c2"), nrow = 2, ncol = 3, byrow = TRUE,
      dimnames = list(c("row1", "row2"), tableColumnNames))
  }
  table<-Table(tableSchema=tableSchema, values=matrix)
  tableRowSet<-synStore(table, retrieveData=TRUE, verbose=FALSE)
  checkEquals(tableRowSet$tableId, propertyValue(tableSchema, "id"))
  # checkTrue(length(tableRowSet$etag)>0) restore once PLFM-2947 is fixed
  checkEquals(as.list(propertyValue(tableSchema, "columnIds")), tableRowSet$headers@content)
  checkEquals(length(tableRowSet$rows), 2)
  checkEquals(tableRowSet@rows[[1]]@values@content, list("a1", "b1", "c1"))
  checkEquals(tableRowSet@rows[[2]]@values@content, list("a2", "b2", "c2"))
}

integrationTestSynStoreDataFrame <- function() {
  project<-synapseClient:::.getCache("testProject")
  
  tableColumns<-createColumns()
  tableColumnNames<-list()
  for (column in tableColumns) tableColumnNames<-append(tableColumnNames, column@name)
  tableSchema<-createTableSchema(propertyValue(project, "id"), tableColumns)
  
  id<-propertyValue(tableSchema, "id")
  # note we permute the column order in the data frame values and headers, then
  # test that it comes out right
  if (FALSE) {
    # TODO reenable this once PLFM-2954 is fixed
    dataFrame <- as.data.frame(matrix(c("b1", "a1", "c1", "b2", "a2", "c2"), nrow = 2, ncol = 3, byrow = TRUE,
      dimnames = list(c("row1", "row2"), tableColumnNames[c(2,1,3)])))
  } else {
    # the simple version, without rearranging the columns when uploading
    dataFrame <- as.data.frame(matrix(c("a1", "b1", "c1", "a2", "b2", "c2"), nrow = 2, ncol = 3, byrow = TRUE,
      dimnames = list(c("row1", "row2"), tableColumnNames)))
  }
  table<-Table(tableSchema=tableSchema, values=dataFrame)
  tableRowSet<-synStore(table, retrieveData=TRUE, verbose=FALSE)
  checkEquals(tableRowSet$tableId, propertyValue(tableSchema, "id"))
  # checkTrue(length(tableRowSet$etag)>0) restore once PLFM-2947 is fixed
  checkEquals(as.list(propertyValue(tableSchema, "columnIds")), tableRowSet$headers@content)
  checkEquals(length(tableRowSet$rows), 2)
  checkEquals(tableRowSet@rows[[1]]@values@content, list("a1", "b1", "c1"))
  checkEquals(tableRowSet@rows[[2]]@values@content, list("a2", "b2", "c2"))
}

integrationTestSynStoreCSVFile <- function() {
  project<-synapseClient:::.getCache("testProject")
  
  tableColumns<-createColumns()
  tableColumnNames<-list()
  for (column in tableColumns) tableColumnNames<-append(tableColumnNames, column@name)
  tableSchema<-createTableSchema(propertyValue(project, "id"), tableColumns)
  
  id<-propertyValue(tableSchema, "id")
  table<-Table(tableSchema=tableSchema, values=system.file("resources/test/test.csv", package = "synapseClient"))
  lineCount<-synStore(table)
  checkEquals(2, lineCount)
}

  