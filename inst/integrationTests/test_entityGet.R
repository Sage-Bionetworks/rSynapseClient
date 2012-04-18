## Test getEntity method
## 
## Author: Nicole Deflaux <nicole.deflaux@sagebase.org>
###############################################################################
#
#integrationTestGet <- 
#  function() 
#{
#  study <- synapseQuery(query='select * from entity where entity.name == "MSKCC Prostate Cancer"')
# 
#  study <- getEntity(entity=study$entity.id[1])
#  
#  ## The fields returned by this service API may change over time, but
#  ## there are a few we should always expect to receive
#  checkTrue("id" %in% names(properties(study)))
#  checkTrue("name" %in% names(properties(study)))
#  checkTrue("versionLabel" %in% names(properties(study)))
#  checkTrue("uri" %in% names(properties(study)))
#}
#

integrationTestWarnMe <-
  function(){
  warning("need to fix tests for test_entityGet.R")
}