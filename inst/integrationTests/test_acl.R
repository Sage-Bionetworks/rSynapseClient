# Integration Tests for ACL management
# 
# Author: brucehoff
###############################################################################

.setUp <- function() {
  ## create a project to fill with entities
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)
  
}

.tearDown <- function() {
  ## delete the test project
  deleteEntity(synapseClient:::.getCache("testProject"))
}


integrationTestACLRoundtrip <- function() {
  project<-synapseClient:::.getCache("testProject")
  file<-File(name="foo", parentId=propertyValue(project, "id"))
  file<-synStore(file)
  id<-propertyValue(file, "id")
  
  # make an ACL for the file
  acl<-AccessControlList(id=id) # TODO add self into ACL
  acl<-synCreateEntityACL(acl)
  # TODO check content
  
  # retrieve the ACL for the file
  retrieved<-synGetEntityACL(id)
  checkTrue(identical(retrieved, acl))
  
  # change the ACL for the file
  acl<-synUpdateEntityACL(acl)
  
  # delete the ACL for the file
  synDeleteEntityACL(id)
  
  #TODO check that it's inherited again
}