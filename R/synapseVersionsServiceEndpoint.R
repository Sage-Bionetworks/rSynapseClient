## set and get the Synapse Versions service endpoint
## 
## Author: Bruce Hoff
###############################################################################

synapseVersionsServiceEndpoint <- 
  function(endpoint)
{
  if (!missing(endpoint)) {
    .setCache("versionsServiceEndpoint", endpoint)
  } else {
    return(.getCache("versionsServiceEndpoint"))
  }
}

.getVersionsEndpoint <- function() {
  .getCache("versionsServiceEndpoint")	
}

