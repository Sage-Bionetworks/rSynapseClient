## Refresh the synapse session token
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
##############################################################################

synapseRefreshSessionToken <- 
  function(sessionToken, host=getEndpointLocationForService("AUTH"))
{
  # constants
  kService <- "/session"
  ## end constants
  
  entity <- list()
  entity$sessionToken <- sessionToken
  
  uri <- kService
  response <- synapsePut(uri=uri, entity=entity, endpoint=synapseServiceEndpoint("AUTH"), anonymous=TRUE)
  .setCache("sessionTimestamp", Sys.time())
}
