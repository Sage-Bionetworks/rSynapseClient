## Test getEntity method
## 
## Author: Nicole Deflaux <nicole.deflaux@sagebase.org>
###############################################################################

integrationTestGet <- 
  function() 
{
  datasets <- synapseQuery(query='select * from dataset where dataset.name == "MSKCC Prostate Cancer"')
 
  dataset <- getEntity(entity=datasets$dataset.id[1])
  
  ## The fields returned by this service API may change over time, but
  ## there are a few we should always expect to receive
  checkTrue("id" %in% names(properties(dataset)))
  checkTrue("name" %in% names(properties(dataset)))
  checkTrue("versionLabel" %in% names(properties(dataset)))
  checkTrue("uri" %in% names(properties(dataset)))
}

