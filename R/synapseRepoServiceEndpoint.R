## get/set the Synapse repo service endpoint
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseRepoServiceEndpoint <- 
  function(endpoint)
{
  if (!missing(endpoint)) {
    .setCache("reposerviceEndpoint", endpoint)
    url <- .ParsedUrl(url=endpoint)
    .setCache("reposerviceEndpointLocation", paste(url@protocol, '://', url@authority, sep=''))
	.setCache("reposerviceEndpointProtocol", url@protocol)
	.setCache("reposerviceEndpointHost", url@authority)
	.setCache("reposerviceEndpointPrefix", url@path)
    
    synapseClient:::.setCache("sessionToken", NULL)
    synapseClient:::.setCache("hmacSecretKey", NULL) 
  }
  else {
    return(.getCache("reposerviceEndpoint"))
  }
}

.getRepoEndpointLocation <- function() {
  .getCache("reposerviceEndpointLocation")	
}

.getRepoEndpointPrefix <- function() {
	.getCache("reposerviceEndpointPrefix")	
}

.getRepoEndpointProtocol <- function() {
	.getCache("reposerviceEndpointProtocol")
}

.getRepoEndpointHost <- function() {
	.getCache("reposerviceEndpointHost")	
}
