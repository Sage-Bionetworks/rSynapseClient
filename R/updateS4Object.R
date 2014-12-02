# update method for auto-generated S4 objects
# 
# Author: brucehoff
###############################################################################

updateS4Object<-function(object, updateUri) {
  objectAsList<-createListFromS4Object(object)
  listResult<-synRestPUT(updateUri, objectAsList)
  objectResult<-createS4ObjectFromList(listResult, class(object))
  objectResult
}



