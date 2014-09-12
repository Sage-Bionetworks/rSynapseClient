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
      name=sprintf("R_Integration_Test_Column_%d", i), 
      columnType="STRING")
    stored<-synStore(tableColumn)
    tableColumns<-append(tableColumns, stored)
  }
  tableColumns
}

createTableSchema<-function(projectId, tableColumns) {
  name<-sprintf("R_Client_Integration_Test_Create_Schema_%s", sample(999999999, 1))
  
  tableSchema<-TableSchema(name=name, parent=projectId, columns=tableColumns)
  tableSchema
}

integrationTestSynStoreDataFrame <- function() {
  project<-synapseClient:::.getCache("testProject")
  
  tableColumns<-createColumns()
  tableColumnNames<-list()
  for (column in tableColumns) tableColumnNames<-append(tableColumnNames, column@name)
  tableSchema<-createTableSchema(propertyValue(project, "id"), tableColumns)

  dataFrame <- as.data.frame(matrix(c("a1", "b1", "c1", "a2", "b2", "c2"), nrow = 2, ncol = 3, byrow = TRUE,
      dimnames = list(c(1,2), tableColumnNames)))
  # note we permute the column order in the data frame values and headers, then
  # test that it comes out right
  permutedDataFrame <- as.data.frame(matrix(c("b1", "a1", "c1", "b2", "a2", "c2"), nrow = 2, ncol = 3, byrow = TRUE,
      dimnames = list(c(1,2), tableColumnNames[c(2,1,3)])))
  table<-Table(tableSchema=tableSchema, values=permutedDataFrame)
  retrievedTable<-synStore(table, retrieveData=TRUE, verbose=FALSE)
  checkTrue(is(retrievedTable, "TableDataFrame"))
  checkTrue(!is.null(propertyValue(retrievedTable@schema, "id")))
  checkTrue(length(retrievedTable@updateEtag)>0)
  # now check that the data frames are the same
  checkTrue(all(dataFrame==retrievedTable@values))
  checkTrue(all(names(dataFrame)==names(retrievedTable@values)))
  # make sure the row labels are valid
  synapseClient:::parseRowAndVersion(row.names(retrievedTable@values))
  
  # modify the retrieved table (we exchange the two values)
  retrievedTable@values[1,3]<-"c2"
  retrievedTable@values[2,3]<-"c1"
  # update in Synapse
  
  updatedTable<-synStore(retrievedTable, retrieveData=TRUE, verbose=FALSE)
  checkTrue(is(updatedTable, "TableDataFrame"))
  checkEquals(propertyValue(updatedTable@schema, "id"), propertyValue(retrievedTable@schema, "id"))
  checkTrue(length(updatedTable@updateEtag)>0)
  # now check that the data frames are the same
  checkTrue(all(retrievedTable@values==updatedTable@values))
  checkTrue(all(names(retrievedTable@values)==names(updatedTable@values)))
  # make sure the row labels are valid
  synapseClient:::parseRowAndVersion(row.names(updatedTable@values))   

}

integrationTestSynStoreDataFrameNORetrieveData <- function() {
  project<-synapseClient:::.getCache("testProject")
  
  tableColumns<-createColumns()
  tableColumnNames<-list()
  for (column in tableColumns) tableColumnNames<-append(tableColumnNames, column@name)
  tableSchema<-createTableSchema(propertyValue(project, "id"), tableColumns)
  tableSchema<-synStore(tableSchema)
  dataFrame <- as.data.frame(matrix(c("b1", "a1", "c1", "b2", "a2", "c2"), nrow = 2, ncol = 3, byrow = TRUE,
      dimnames = list(c(1,2), tableColumnNames[c(2,1,3)])))
  table<-Table(tableSchema=propertyValue(tableSchema, "id"), values=dataFrame)
  stored<-synStore(table, verbose=FALSE)
  checkEquals(stored@rowCount, 2)
}

integrationTestSynStoreDataFrameWRONGColumns <- function() {
  project<-synapseClient:::.getCache("testProject")
  
  tableColumns<-createColumns()
  tableColumnNames<-list()
  for (column in tableColumns) tableColumnNames<-append(tableColumnNames, column@name)
  tableSchema<-createTableSchema(propertyValue(project, "id"), tableColumns)
  
  id<-propertyValue(tableSchema, "id")
  # replace the third column name with "foo"
  dataFrame <- as.data.frame(matrix(c("a1", "b1", "c1", "a2", "b2", "c2"), nrow = 2, ncol = 3, byrow = TRUE,
      dimnames = list(c(1,2), c(tableColumnNames[1:2], "foo"))))
  table<-Table(tableSchema=tableSchema, values=dataFrame)
  # the erroneous column name should cause an error
  checkException(synStore(table, verbose=FALSE))
}

integrationTestSynStoreDataFrameWrongColumnType <- function() {
  project<-synapseClient:::.getCache("testProject")
  
  tableColumns<-createColumns()
  tableColumn<-TableColumn(
    name=sprintf("R_Integration_Test_Column_%d", 3), 
    columnType="INTEGER") # WRONG TYPE FOR DATA!
  tableColumns[[3]]<-synStore(tableColumn)
  
  tableColumnNames<-list()
  for (column in tableColumns) tableColumnNames<-append(tableColumnNames, column@name)
  tableSchema<-createTableSchema(propertyValue(project, "id"), tableColumns)
  
  id<-propertyValue(tableSchema, "id")
  dataFrame <- as.data.frame(matrix(c("a1", "b1", "c1", "a2", "b2", "c2"), nrow = 2, ncol = 3, byrow = TRUE,
      dimnames = list(c(1,2), tableColumnNames)))
  table<-Table(tableSchema=tableSchema, values=dataFrame)
  # the erroneous column type should cause an error
  checkException(synStore(table, verbose=FALSE))
}

integrationTestSynStoreMixedDataFrame<-function() {
  project<-synapseClient:::.getCache("testProject")
  
  tc1 <- TableColumn(name="sweet", columnType="STRING", enumValues=CharacterList("one", "two", "three"))
  tc1 <- synStore(tc1)
  tc2 <- TableColumn(name="sweet2", columnType="INTEGER")
  tc2 <- synStore(tc2)
  
  pid<-propertyValue(project, "id")
  tschema <- TableSchema(name = "testDataFrameTable", parent=pid, columns=c(tc1, tc2))
  tschema <- synStore(tschema, createOrUpdate=FALSE)
  
  rowsToUpload<-30
  myTable <- Table(propertyValue(tschema, "id"), values=data.frame(sweet=sample(c("one", "two", "three"), size = rowsToUpload, replace = T), sweet2=sample.int(rowsToUpload, replace = T)))
  stored <- synStore(myTable)
  # returns the number of rows uploaded
  checkEquals(stored@rowCount, rowsToUpload)
}

integrationTestSynStoreRetrieveAndQueryMixedDataFrame<-function() {
  project<-synapseClient:::.getCache("testProject")
  
  tc1 <- TableColumn(name="sweet", columnType="STRING")
  tc1 <- synStore(tc1)
  tc2 <- TableColumn(name="sweet2", columnType="INTEGER")
  tc2 <- synStore(tc2)
  
  pid<-propertyValue(project, "id")
  tschema <- TableSchema(name = "testDataFrameTable", parent=pid, columns=c(tc1, tc2))
  tschema <- synStore(tschema, createOrUpdate=FALSE)
  
  rowsToUpload<-30
  dataFrame <- data.frame(sweet=sample(c("one", "two", "three"), 
      size = rowsToUpload, replace = T), 
    sweet2=sample.int(rowsToUpload, replace = T))
  myTable <- Table(tschema, values=dataFrame)
  myTable <- synStore(myTable, retrieveData=T)
  checkTrue(is(myTable, "TableDataFrame"))
  checkEquals(propertyValue(myTable@schema, "id"), propertyValue(tschema, "id"))
  checkTrue(length(myTable@updateEtag)>0)
  # now check that the data frames are the same
  checkTrue(all(dataFrame==myTable@values))
  checkTrue(all(names(dataFrame)==names(myTable@values)))
  # make sure the row labels are valid
  synapseClient:::parseRowAndVersion(row.names(myTable@values))
  
  # test synTableQuery
  queryResult<-synTableQuery(sprintf("select * from %s", propertyValue(tschema, "id")), verbose=FALSE)
  checkTrue(is(queryResult, "TableDataFrame"))
  checkEquals(queryResult@schema, propertyValue(tschema, "id"))
  checkTrue(all(dataFrame==queryResult@values))
  checkTrue(all(names(dataFrame)==names(queryResult@values)))
  checkTrue(length(queryResult@updateEtag)>0)
  
  # test no load
  queryResult<-synTableQuery(sprintf("select * from %s", propertyValue(tschema, "id")), loadResult=FALSE, verbose=FALSE)
  checkTrue(is(queryResult, "TableFilePath"))
  checkEquals(queryResult@schema, propertyValue(tschema, "id"))
  checkTrue(file.exists(queryResult@filePath))
  checkTrue(length(queryResult@updateEtag)>0)
  
  downloadLocation<-tempdir()
  queryResult<-synTableQuery(sprintf("select * from %s", propertyValue(tschema, "id")), loadResult=FALSE, verbose=FALSE, downloadLocation=downloadLocation)
  checkTrue(file.exists(queryResult@filePath))
  checkEquals(downloadLocation, substring(queryResult@filePath, 1, nchar(downloadLocation)))
  
  # test a simple aggregation query
  queryResult<-synTableQuery(sprintf("select count(*) from %s", propertyValue(tschema, "id")), verbose=FALSE)
  if (FALSE) { # reenable when PLFM-2987 is fixed
    checkEquals(rowsToUpload, queryResult@values[1,1])
  }
  
  # test a more complicated aggregation query
  queryResult<-synTableQuery(sprintf("select sweet, count(sweet) from %s", propertyValue(tschema, "id")), verbose=FALSE)
  expected<-data.frame(sweet="one", X=as.integer(rowsToUpload))
  checkTrue(all(expected==queryResult@values))
  checkTrue(all(names(expected)==names(queryResult@values)))
  
  # finally, check row deletion
  queryResult<-synTableQuery(sprintf("select * from %s", propertyValue(tschema, "id")), loadResult=TRUE, verbose=FALSE)
  deletionResult<-synDeleteRows(queryResult)
  checkEquals(deletionResult@rowCount, rowsToUpload)
}

integrationTestSynStoreRetrieveAndQueryNumericDataFrame<-function() {
  project<-synapseClient:::.getCache("testProject")
  
  tc1 <- TableColumn(name="sweet", columnType="DOUBLE")
  tc1 <- synStore(tc1)
  tc2 <- TableColumn(name="sweet2", columnType="DOUBLE")
  tc2 <- synStore(tc2)
  
  pid<-propertyValue(project, "id")
  tschema <- TableSchema(name = "testDataFrameTable", parent=pid, columns=c(tc1, tc2))
  tschema <- synStore(tschema, createOrUpdate=FALSE)
  
  dataFrame <- data.frame(sweet=c(1:5, 1.234e-10, 5.678e+10), sweet2=c(6:10, 1.234567, 9.876543))
  myTable <- Table(tschema, values=dataFrame)
  myTable <- synStore(myTable, retrieveData=T)
  # now check that the data frames are the same
  checkTrue(all(dataFrame==myTable@values))
  checkTrue(all(names(dataFrame)==names(myTable@values)))
}

integrationTestSynStoreCSVFileNoRetrieve <- function() {
  project<-synapseClient:::.getCache("testProject")
  
  tableColumns<-createColumns()
  tableColumnNames<-list()
  for (column in tableColumns) tableColumnNames<-append(tableColumnNames, column@name)
  tableSchema<-createTableSchema(propertyValue(project, "id"), tableColumns)
  tableSchema<-synStore(tableSchema)
  id<-propertyValue(tableSchema, "id")
  table<-Table(tableSchema=tableSchema, values=system.file("resources/test/test.csv", package = "synapseClient"))
  stored<-synStore(table)
  checkEquals(2, stored@rowCount)
}

integrationTestSynStoreAndRetrieveCSVFile <- function() {
  project<-synapseClient:::.getCache("testProject")
  
  tableColumns<-createColumns()
  tableColumnNames<-list()
  for (column in tableColumns) tableColumnNames<-append(tableColumnNames, column@name)
  tableSchema<-createTableSchema(propertyValue(project, "id"), tableColumns)
  
  csvFilePath<-system.file("resources/test/test.csv", package = "synapseClient")
  table<-Table(tableSchema=tableSchema, values=csvFilePath)
  downloadLocation<-tempdir()
  retrievedTable<-synStore(table, retrieveData=TRUE, verbose=FALSE, downloadLocation=downloadLocation)
  checkTrue(is(retrievedTable, "TableFilePath"))
  checkTrue(!is.null(propertyValue(retrievedTable@schema, "id")))
  checkTrue(length(retrievedTable@updateEtag)>0)
  # now check that the data frames are the same
  checkEquals(downloadLocation, substring(retrievedTable@filePath, 1, nchar(downloadLocation)))
  retrievedDataFrame<-synapseClient:::loadCSVasDataFrame(retrievedTable@filePath)
  dataFrame<-read.csv(csvFilePath, header=FALSE)
  checkTrue(all(dataFrame==retrievedDataFrame))
  checkTrue(all(tableColumnNames==names(retrievedDataFrame)))
  # make sure the row labels are valid
  synapseClient:::parseRowAndVersion(row.names(retrievedDataFrame))
}

  