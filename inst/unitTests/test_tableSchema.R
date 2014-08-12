# unit tests for TableSchema
# 
# Author: brucehoff
###############################################################################


test_tableSchema<-function() {
  tableColumns<-list()
  for (i in 1:3) {
    tableColumn<-TableColumn(id=sprintf("%s", i),
      name=sprintf("R_Client_Unit_Test_Column_Name_%d", i), 
      columnType="STRING")
    tableColumns<-append(tableColumns, tableColumn)
  }
  
  
  name<-sprintf("R_Client_Integration_Test_Create_Schema_%s", sample(999999999, 1))
  
  tableSchema<-TableSchema(name, "syn123", tableColumns,  foo="bar", "pi"=3.14)
  for (i in 1:3) {
    checkEquals(tableColumns[[i]]$id, propertyValue(tableSchema, "columnIds")[[i]])
  }
  
}
