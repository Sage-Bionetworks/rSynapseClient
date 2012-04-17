## Unit tests for generic Synapse Entities
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################
#
#.setUp <- 
#  function()
#{
# 
#  myGetEntity <-
#    function(entity)
#  {
#    list(name="SageBioCurationEula",
#      annotations="/repo/v1/fakeType/3732/annotations",
#      id="-12345",
#      parentId="3730",
#      etag="0",
#      agreement="do you agree?",
#      uri="/repo/v1/fakeType/3732",
#      accessControlList="/repo/v1/fakeType/3732/acl"
#    )
#  }
#  
#  mySynapseEntity <-
#    function(entity)
#  {
#    s4Entity <- new("SynapseEntity")
#    synapseClient:::.populateSlotsFromEntity(s4Entity, entity)
#  }
#  
#  unloadNamespace('synapseClient')
#  assignInNamespace("SynapseEntity", mySynapseEntity, "synapseClient")
#  assignInNamespace(".getEntity", myGetEntity, "synapseClient")
#  attachNamespace("synapseClient")
#}
#
#.tearDown <-
#  function()
#{
#  ## put back the overridden functions and original cache
#  unloadNamespace("synapseClient")
#  attachNamespace("synapseClient")
#  unloadNamespace("synapseClient")
#  attachNamespace("synapseClient")
#}
#
#unitTestGenericEntity <-
#  function()
#{
#  entity <- getEntity("aFakeId")
#  checkEquals("SynapseEntity", as.character(class(entity)))
#}
stop("Please fix these tests")