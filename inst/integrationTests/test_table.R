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
        dimnames = list(c(1,2), tableColumnNames[c(2,1,3)])))
  } else {
    # the simple version, without rearranging the columns when uploading
    dataFrame <- as.data.frame(matrix(c("a1", "b1", "c1", "a2", "b2", "c2"), nrow = 2, ncol = 3, byrow = TRUE,
        dimnames = list(c(1,2), tableColumnNames)))
  }
  table<-Table(tableSchema=tableSchema, values=dataFrame)
  retrievedTable<-synStore(table, retrieveData=TRUE, verbose=FALSE)
  checkTrue(is(retrievedTable, "TableDataFrame"))
  checkEquals(propertyValue(retrievedTable@schema, "id"), propertyValue(tableSchema, "id"))
  checkTrue(length(retrievedTable@updateEtag)>0)
  # now check that the data frames are the same
  all(dataFrame==retrievedTable@values)
  all(names(dataFrame)==names(retrievedTable@values))
  # make sure the row labels are valid
  synapseClient:::parseRowAndVersion(row.names(retrievedTable@values))
}

integrationTestSynStoreDataFrameNORetrieveData <- function() {
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
        dimnames = list(c(1,2), tableColumnNames[c(2,1,3)])))
  } else {
    # the simple version, without rearranging the columns when uploading
    dataFrame <- as.data.frame(matrix(c("a1", "b1", "c1", "a2", "b2", "c2"), nrow = 2, ncol = 3, byrow = TRUE,
        dimnames = list(c(1,2), tableColumnNames)))
  }
  table<-Table(tableSchema=tableSchema, values=dataFrame)
  rowCount<-synStore(table, verbose=FALSE)
  checkEquals(rowCount, 2)
}

integrationTestSynStoreNumericDataFrame<-function() {
  project<-synapseClient:::.getCache("testProject")
  
  tc1 <- TableColumn(name="sweet", columnType="STRING")
  tc1 <- synStore(tc1)
  tc2 <- TableColumn(name="sweet2", columnType="INTEGER")
  tc2 <- synStore(tc2)
  
  pid<-propertyValue(project, "id")
  tschema <- TableSchema(name = "testDataFrameTable", parent=pid, columns=c(tc1, tc2))
  tschema <- synStore(tschema, createOrUpdate=FALSE)
  
  rowsToUpload<-30
  myTable <- Table(tschema, values=data.frame(sweet=sample(c("one", "two", "three"), size = rowsToUpload, replace = T), sweet2=sample.int(rowsToUpload, replace = T)))
  rowCount <- synStore(myTable)
  # returns the number of rows uploaded
  checkEquals(rowCount, rowsToUpload)
}

integrationTestSynStoreAndRETRIEVENumericDataFrame<-function() {
  project<-synapseClient:::.getCache("testProject")
  
  tc1 <- TableColumn(name="sweet", columnType="STRING")
  tc1 <- synStore(tc1)
  tc2 <- TableColumn(name="sweet2", columnType="INTEGER")
  tc2 <- synStore(tc2)
  
  pid<-propertyValue(project, "id")
  tschema <- TableSchema(name = "testDataFrameTable", parent=pid, columns=c(tc1, tc2))
  tschema <- synStore(tschema, createOrUpdate=FALSE)
  
  rowsToUpload<-30
  dataFrame <- data.frame(sweet=sample(c("one", "two", "three"), size = rowsToUpload, replace = T), sweet2=sample.int(rowsToUpload, replace = T))
  myTable <- Table(tschema, values=dataFrame)
  myTable <- synStore(myTable, retrieveData=T)
  checkTrue(is(myTable, "TableDataFrame"))
  checkEquals(propertyValue(myTable@schema, "id"), propertyValue(tschema, "id"))
  checkTrue(length(myTable@updateEtag)>0)
  # now check that the data frames are the same
  all(dataFrame==myTable@values)
  all(names(dataFrame)==names(myTable@values))
  # make sure the row labels are valid
  synapseClient:::parseRowAndVersion(row.names(myTable@values))
}

integrationTestLargeTable<-function() {
  project<-synapseClient:::.getCache("testProject")
  pid<-propertyValue(project, "id")
  
  tc1 <- TableColumn(name="sweet", columnType="STRING")
  tc1 <- synStore(tc1)
  tc3 <- TableColumn(name="sweet3", columnType="STRING")
  tc3 <- synStore(tc3)
  ts <- TableSchema(name="testLargeTable", parent=pid, columns = c(tc1, tc3))
  ts <- synStore(ts)
  nRows<-10000
  mt <- Table(ts, values=data.frame(sweet=sample(c("one", "two", "three"), size=nRows, replace=T), sweet3=sample(c("four", "five"), size=nRows, replace=T)))
  rowCount <- synStore(mt)
  checkEquals(rowCount, nRows)
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

# TODO test updating a table

  