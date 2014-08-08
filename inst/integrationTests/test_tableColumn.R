# 
# Author: brucehoff
###############################################################################


integrationTestCreateColumn<-function() {
  name<-sprintf("R_Client_Integration_Test_%s", sample(999999999, 1))
  enumValues<-synapseClient:::CharacterList("foo", "bar", "bas")
   
  tableColumn<-TableColumn(
    name=name, 
    columnType="STRING", 
    defaultValue="foo", 
    maximumSize=as.integer(30), 
    enumValues=enumValues)
  
}
