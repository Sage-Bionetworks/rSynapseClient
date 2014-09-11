# unit tests for supporting functions in Table.R
# 
# Author: brucehoff
###############################################################################

unitTest_asTableColumns<-function() {
  tableColumns<-as.tableColumns(system.file("resources/test/withHeaders.csv", package = "synapseClient"))
  checkEquals(4, length(tableColumns))
  checkTrue(identical(tableColumns[[1]], 
      TableColumn(name="string", columnType="STRING", enumValues=CharacterList("a1", "a2"))))
  # todo check the other columns
}

