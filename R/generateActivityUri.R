# 
# A method to generate the URI for doing Activity CRUD
#
.generateActivityUri<-function(id) {
  paste("/activity", id, sep="/")
}