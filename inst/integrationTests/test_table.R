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

integrationTestSynStore <- function() {
  project<-synapseClient:::.getCache("testProject")
  
  
  tableColumns<-list()
  tableColumnNames<-list()
  for (i in 1:3) {
    tableColumn<-TableColumn(
      name=sprintf("R_Client_Integration_Test_Column_Name_%d", i), 
      columnType="STRING")
    stored<-synStore(tableColumn)
    tableColumns<-append(tableColumns, stored)
    tableColumnNames<-append(tableColumnNames, stored$name)
  }
  
  name<-sprintf("R_Client_Integration_Test_Create_Schema_%s", sample(999999999, 1))
  
  tableSchema<-TableSchema(name=name, parent=propertyValue(project, "id"), columns=tableColumns)
  tableSchema<-synStore(tableSchema) # TODO also check the variation in which we don't save the schema in advance of storing the table
  
  # test some utilities used by synStore
  id<-propertyValue(tableSchema, "id")
  # don't know what the answer is, just test that it works
  checkTrue(synapseClient:::maxRowTransferForSchema(id)>0)
  checkTrue(synapseClient:::maxRowTransferForSchemaAndColumnNames(id, tableColumnNames)>0)
  
  rowList<-RowList()
  rowList<-add(rowList, Row(values=CharacterList("a1", "b1", "c1")))
  rowList<-add(rowList, Row(values=CharacterList("a2", "b2", "c2")))
  table<-Table(tableSchema, rowList)
  rowReferenceSet<-synStore(table)
  checkEquals(rowReferenceSet$tableId, propertyValue(tableSchema, "id"))
  checkTrue(length(rowReferenceSet$etag)>0)
  checkEquals(as.list(propertyValue(tableSchema, "columnIds")), rowReferenceSet$headers@content)
  checkEquals(length(rowReferenceSet$rows), 2)
  
  #rerun with retrieveData=TRUE
  rowList<-RowList()
  rowList<-add(rowList, Row(values=CharacterList("a3", "b3", "c3")))
  rowList<-add(rowList, Row(values=CharacterList("a4", "b4", "c4")))
  table<-Table(tableSchema, rowList)
  tableRowSet<-synStore(table, retrieveData=TRUE, verbose=FALSE)
  checkEquals(tableRowSet$tableId, propertyValue(tableSchema, "id"))
  checkTrue(length(tableRowSet$etag)>0)
  checkEquals(as.list(propertyValue(tableSchema, "columnIds")), tableRowSet$headers@content)
  checkEquals(length(tableRowSet$rows), 2)
  checkEquals(tableRowSet@rows[[1]]@values@content, list("a3", "b3", "c3"))
  checkEquals(tableRowSet@rows[[2]]@values@content, list("a4", "b4", "c4"))
}
  