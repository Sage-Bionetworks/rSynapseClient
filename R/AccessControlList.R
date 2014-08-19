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

showContent<-function(acl) {
  result<-sprintf("Access Control List for %s, created on %s\n", acl$id, acl$creationDate)
  if (isNullSlot(acl@resourceAccess)) {
    result<-paste(result, "\t**no ACL entries**\n", sep="")
  } else {
    for (ra in acl@resourceAccess@content) {
      result<-paste(result,
      sprintf("\tPrincipal ID %s has access type(s): %s\n", 
        ra@principalId, paste(ra@accessType@content, collapse=", "))
      , sep="")
    }
  }
  result
}

setMethod(
  f = "show",
  signature = signature("AccessControlList"),
  definition = function(object) {
    cat(showContent(object))
  }
)


# TODO write functions for managing Evaluation ACLs

