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
	
	  # it should work whether or not you store the column first
    if (i<3) tableColumn<-synStore(tableColumn)
		if (i==2) {
			# it should work if you just provide the ID
			tableColumns<-append(tableColumns, tableColumn@id)
		} else {
			tableColumns<-append(tableColumns, tableColumn)
		}
  }
	# this point tableColumns is a list of (1) a stored column, (2) an unstored column, (3) a column ID
  
  name<-sprintf("R_Client_Integration_Test_Create_Schema_%s", sample(999999999, 1))
 
  tableSchema<-TableSchema(name=name, parent=propertyValue(project, "id"), columns=tableColumns,  foo="bar", "pi"=3.14)
	
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
	
	# check that that retrieved columns match the retrieved column IDs
	checkEquals(3, length(retrievedSchema@columns))
	checkEquals(3, length(propertyValue(retrievedSchema, "columnIds")))
	for (i in 1:3) {
		checkTrue(any(retrievedSchema@columns[[i]]$id==propertyValue(retrievedSchema, "columnIds")))
	}
	
	# now check that the retrieved table columns match the originals
	# since we didn't save #3 ahead of time, we just copy over the id here
	tableColumns[[3]]@id<-retrievedSchema@columns[[3]]@id
	columnIdToColumnMap<-list()
	for (i in 1:3) {
		columnIdToColumnMap[[retrievedSchema@columns[[i]]@id]]<-retrievedSchema@columns[[i]]
	}
	
  synDelete(storedSchema)
  
  # check synDelete
  checkException(synGet(id))
 
}
