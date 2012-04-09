## set and get the Synapse auth service endpoint
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseAuthServiceEndpoint <- 
  function(endpoint)
{
  if (!missing(endpoint)) {
    .setCache("authserviceEndpoint", endpoint)
    url <- .ParsedUrl(url=endpoint)
    .setCache("authserviceEndpointLocation", paste(url@protocol, '://', url@authority, sep=''))
    .setCache("authserviceEndpointPrefix", url@path)
    if(.getCache('useJavaClient')){
      .jenv[["syn"]]$setAuthEndpoint(endpoint)
    }
    synapseClient:::.setCache("sessionToken", NULL)
    synapseClient:::.setCache("hmacSecretKey", NULL)
  }
  else {
    return(.getCache("authserviceEndpoint"))
  }
}

.getAuthEndpointLocation <- function() {
  .getCache("authserviceEndpointLocation")	
}

.getAuthEndpointPrefix <- function() {
  .getCache("authserviceEndpointPrefix")	
}
