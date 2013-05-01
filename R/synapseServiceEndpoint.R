## get/set the Synapse repo service endpoint
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseServiceEndpoint <- 
  function(service, endpoint)
{
  cacheKey<-serviceEndpointCacheKey(service)
  if (!missing(endpoint)) {
    endpointObject<-parseEndpoint(endpoint)
    endpointObject$service<-service # i.e. along with the endpoint, we store what kind of service it's for
    .setCache(cacheKey, endpointObject)
  } else {
    .getCache(cacheKey)
  }
}

# TODO perhaps these public functions should be combined into one which sets all the endpoints at once
synapseRepoServiceEndpoint<-function(endpoint) {synapseServiceEndpoint("REPO", endpoint)}
synapseAuthServiceEndpoint<-function(endpoint) {synapseServiceEndpoint("AUTH", endpoint)}
synapseFileServiceEndpoint<-function(endpoint) {synapseServiceEndpoint("FILE", endpoint)}
synapsePortalEndpoint<-function(endpoint) {synapseServiceEndpoint("PORTAL", endpoint)}

getEndpointForService<-function(service) {synapseServiceEndpoint(service)$endpoint}

getEndpointPrefixForService<-function(service) {synapseServiceEndpoint(service)$endpointPrefix}

getEndpointLocationForService<-function(service) {synapseServiceEndpoint(service)$endpointLocation}

serviceEndpointCacheKey<-function(service) {sprintf("%sEndpoint", service)}

parseEndpoint<-function(endpoint) {
  ans<-list()
  ans$endpoint<-endpoint
  url <- .ParsedUrl(url=endpoint)
  ans$endpointLocation <- paste(url@protocol, '://', url@authority, sep='')
  ans$endpointProtocol <- url@protocol
  ans$endpointHost <- url@authority
  ans$endpointPrefix <- url@path
  ans
}

