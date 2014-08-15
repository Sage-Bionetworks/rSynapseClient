# accessors for Access Control Lists
# 
# Author: brucehoff
###############################################################################


synCreateEntityACL<-function(acl) {
  uri<-sprintf("/entity/%s/acl", acl@id)
  createS4ObjectFromList(synRestPOST(uri, createListFromS4Object(acl)), "AccessControlList")
}

synGetEntityACL<-function(entityId) {
  uri<-sprintf("/entity/%s/acl", entityId)
  createS4ObjectFromList(synRestGET(uri), "AccessControlList")
}

synUpdateEntityACL<-function(acl) {
  uri<-sprintf("/entity/%s/acl", acl@id)
  createS4ObjectFromList(synRestPUT(uri, createListFromS4Object(acl)), "AccessControlList")
}

synDeleteEntityACL<-function(entityId) {
  synRestDELETE(sprintf("/entity/%s/acl", entityId))
}

# TODO write functions for managing Evaluation ACLs

