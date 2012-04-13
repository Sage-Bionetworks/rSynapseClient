## set/get the Synapse portal endpoint
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapsePortalEndpoint <- 
  function(endpoint)
{
  if (!missing(endpoint)) {
    .setCache("portalEndpoint", endpoint)
  }
  else {
    return(.getCache("portalEndpoint"))
  }
}
