## set and get the Synapse FILE service endpoint
## 
## Author: <bruce.hoff@sagebase.org>
###############################################################################

synapseFileServiceEndpoint <- 
  function(endpoint)
{
  if (!missing(endpoint)) {
    .setCache("fileserviceEndpoint", endpoint)
    url <- .ParsedUrl(url=endpoint)
    .setCache("fileserviceEndpointLocation", paste(url@protocol, '://', url@authority, sep=''))
    .setCache("fileserviceEndpointPrefix", url@path)
  }
  else {
    return(.getCache("fileserviceEndpoint"))
  }
}

.getFileEndpointLocation <- function() {
  .getCache("fileserviceEndpointLocation")	
}

.getFileEndpointPrefix <- function() {
  .getCache("fileserviceEndpointPrefix")	
}
