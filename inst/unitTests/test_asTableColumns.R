# unit tests for supporting functions in Table.R
# 
# Author: brucehoff
###############################################################################

test_asTableColumns<-function() {
  tableColumns<-as.tableColumns("withHeaders.csv")
  checkEquals(3, length(tableColumns))
  checkTrue(identical(tableColumns[[1]], TableColumn(name="string", columnType="STRING", enumValues=CharacterList("a1", "a2"))))
  # todo check the other columns
}

