## Integration tests for synapseGet
##
## Author: Nicole Deflaux <nicole.deflaux@sagebase.org>
#################################################################################

integrationTestGet <-
  function()
{
  datasets <- synapseQuery(query='select * from dataset where dataset.name == "MSKCC Prostate Cancer"')

  dataset <- synapseClient:::synapseGet(uri=paste('/dataset/', datasets$dataset.id[1], sep=""))

  ## The fields returned by this service API may change over time, but
  ## there are a few we should always expect to receive
  checkTrue("id" %in% names(dataset))
  checkTrue("name" %in% names(dataset))
  checkTrue("versionLabel" %in% names(dataset))
  checkTrue("id" %in% names(dataset))
  checkTrue("uri" %in% names(dataset))
}
