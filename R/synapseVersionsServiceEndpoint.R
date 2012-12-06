## set and get the Synapse Versions service endpoint
## 
## Author: Bruce Hoff
###############################################################################

synapseVersionsServiceEndpoint <- 
  function(endpoint)
{
  if (!missing(endpoint)) {
    .setCache("versionsserviceEndpoint", endpoint)
    url <- .ParsedUrl(url=endpoint)
    .setCache("versionsserviceEndpointLocation", paste(url@protocol, '://', url@authority, sep=''))
    .setCache("versionsserviceEndpointPrefix", url@path)
  }
  else {
    return(.getCache("versionsserviceEndpoint"))
  }
}

.getVersionsEndpointLocation <- function() {
  .getCache("versionsserviceEndpointLocation")	
}

.getVersionsEndpointPrefix <- function() {
  .getCache("versionsserviceEndpointPrefix")	
}
