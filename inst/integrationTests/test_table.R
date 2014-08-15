# test for operations on Tables
# 
# Author: brucehoff
###############################################################################



setUp <- function() {
  # create project
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)
  
}

.tearDown <- function() {
  # delete the project, cascading to the table
  deleteEntity(synapseClient:::.getCache("testProject"))
  
}

testSynStore <- function() {
  project<-synapseClient:::.getCache("testProject")
  
  
  tableColumns<-list()
  for (i in 1:3) {
    tableColumn<-TableColumn(
      name=sprintf("R_Client_Integration_Test_Column_Name_%d", i), 
      columnType="STRING")
    stored<-synStore(tableColumn)
    tableColumns<-append(tableColumns, stored)
  }
  
  name<-sprintf("R_Client_Integration_Test_Create_Schema_%s", sample(999999999, 1))
  
  tableSchema<-TableSchema(name=name, parent=propertyValue(project, "id"), columns=tableColumns)
  tableSchema<-synStore(tableSchema) # TODO also check the variation in which we don't save the schema in advance of storing the table
  
  rowList<-RowList()
  row<-Row(CharacterList("a1", "b1", "c1"))
  rowList<-add(rowList, row)
  row<-Row(CharacterList("a2", "b2", "c2"))
  rowList<-add(rowList, row)
  table<-Table(tableSchema, rowList)
  rowReferenceSet<-synStore(table, retrieveData=FALSE, verbose=TRUE)
  checkEquals(rowReferenceSet$id, tableSchema$id)
  checkTrue(length(rowReferenceSet$etag)>0)
  checkTrue(identical(tableSchema$headers, rowReferenceSet$headers))
  checkEquals(length(rowReferenceSet$rows), 2)
}