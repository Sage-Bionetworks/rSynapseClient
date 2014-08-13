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

integrationTestCreateTableSchema<-function() {
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
 
  tableSchema<-TableSchema(name, propertyValue(project, "id"), tableColumns,  foo="bar", "pi"=3.14)
  for (i in 1:3) {
    checkEquals(tableColumns[[i]]$id, propertyValue(tableSchema, "columnIds")[[i]])
  }
  
  storedSchema<-synStore(tableSchema)
  id<-propertyValue(storedSchema, "id")
  checkEquals(propertyValue(storedSchema, "name"), name)
  
  # should be identical except for the ids, so we fill in the ID and check that they're identical
  propertyValue(tableSchema, "id")<-id
  checkIdentical(storedSchema, tableSchema)
  
  retrievedSchema<-synGet(id)
  checkIdentical(retrievedSchema, storedSchema)
  
  checkEquals(synGetAnnotations(retrievedSchema, "foo"), "bar")
  checkEquals(synGetAnnotations(retrievedSchema, "pi"), 3.14)
  
  synDelete(storedSchema)
  
  # check synDelete
  checkException(synGet(id))
 
}
