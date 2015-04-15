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
 
  tableSchema<-TableSchema(name=name, parent=propertyValue(project, "id"), columns=tableColumns,  foo="bar", "pi"=3.14)
  for (i in 1:3) {
		checkEquals(tableColumns[[i]], tableSchema@columns[[i]])
	}
  
  storedSchema<-synStore(tableSchema)
  id<-propertyValue(storedSchema, "id")
  checkTrue(!is.null(id))
  checkTrue(!is.null(propertyValue(storedSchema, "etag")))
  checkTrue(!is.null(propertyValue(storedSchema, "createdOn")))
  checkTrue(!is.null(propertyValue(storedSchema, "modifiedOn")))
  checkTrue(!is.null(propertyValue(storedSchema, "uri")))
  checkTrue(!is.null(propertyValue(storedSchema, "createdBy")))
  checkTrue(!is.null(propertyValue(storedSchema, "modifiedBy")))
  checkEquals(propertyValue(storedSchema, "name"), name)
  checkEquals(propertyValue(storedSchema, "parentId"), propertyValue(project, "id"))
  checkEquals(synGetAnnotations(storedSchema), synGetAnnotations(tableSchema))
  
  retrievedSchema<-synGet(id)
  checkTrue(identical(retrievedSchema, storedSchema))
  
  checkEquals(synGetAnnotation(retrievedSchema, "foo"), "bar")
  checkEquals(synGetAnnotation(retrievedSchema, "pi"), 3.14)
  
  synDelete(storedSchema)
  
  # check synDelete
  checkException(synGet(id))
 
}
