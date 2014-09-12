# unit tests for supporting functions in Table.R
# 
# Author: brucehoff
###############################################################################

unitTest_asTableColumns<-function() {
  tableColumns<-as.tableColumns(system.file("resources/test/withHeaders.csv", package = "synapseClient"))
  checkEquals(4, length(tableColumns))
  checkTrue(identical(tableColumns[[1]], 
      TableColumn(name="string", columnType="STRING", enumValues=CharacterList("a1", "a2"))))
  checkTrue(identical(tableColumns[[2]], TableColumn(name="numeric", columnType="DOUBLE")))
  checkTrue(identical(tableColumns[[3]], TableColumn(name="integer", columnType="INTEGER")))
  checkTrue(identical(tableColumns[[4]], TableColumn(name="logical", columnType="BOOLEAN")))
  
  # do the same thing, but starting with a data frame, rather than a file
  dataframe<-read.csv(system.file("resources/test/withHeaders.csv", package = "synapseClient"))
  tableColumns<-as.tableColumns(dataframe)
  
  checkEquals(4, length(tableColumns))
  checkTrue(identical(tableColumns[[1]], 
      TableColumn(name="string", columnType="STRING", enumValues=CharacterList("a1", "a2"))))
  checkTrue(identical(tableColumns[[2]], TableColumn(name="numeric", columnType="DOUBLE")))
  checkTrue(identical(tableColumns[[3]], TableColumn(name="integer", columnType="INTEGER")))
  checkTrue(identical(tableColumns[[4]], TableColumn(name="logical", columnType="BOOLEAN")))
}
