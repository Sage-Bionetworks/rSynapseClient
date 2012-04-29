## Integration tests for synapseGet
##
## Author: Nicole Deflaux <nicole.deflaux@sagebase.org>
#################################################################################

#integrationTestGet <-
#  function()
#{
#  studies <- synapseQuery(query='select * from entity where entity.name == "MSKCC Prostate Cancer"')
#
#  dataset <- synapseClient:::synapseGet(uri=paste('/dataset/', entity$entity.id[1], sep=""))
#
#  ## The fields returned by this service API may change over time, but
#  ## there are a few we should always expect to receive
#  checkTrue("id" %in% names(dataset))
#  checkTrue("name" %in% names(dataset))
#  checkTrue("versionLabel" %in% names(dataset))
#  checkTrue("id" %in% names(dataset))
#  checkTrue("uri" %in% names(dataset))
#}
integrationTestWarnMe <-
  function(){
  warning("need to fix tests for test_synapseGet.R")
}