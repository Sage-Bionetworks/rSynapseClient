# synapseRequestFollowingAllRedirects
#
# calls getURL in RCurl letting RCurl follow redirects
# looks up synapse endpoint to construct URL from the given URI
# 
# Author: brucehoff
###############################################################


synapseRequestFollowingAllRedirects<-function(
  uri, # omitting the endpoint
  endpoint=synapseServiceEndpoint("REPO"), # one of REPO, AUTH, FILE
  postfields = NULL, # the request body
  customrequest, # the request method
  httpheader, # the headers
  curl, # the curl handle
  debugfunction = NULL,
  .opts) {
  
  endpoint<-resolvePermanentRedirects(endpoint)
    
  followRedirOpts<-.opts
  followRedirOpts$followlocation<-T # DO include 'followlocation'
  followRedirOpts$header<-TRUE
  
  url <- paste(endpoint$endpoint, uri, sep="")
  
  result<-getURLWithRetries(url,
    postfields, # the request body
    customrequest, # the request method
    httpheader, # the headers
    curl, # the curl handle
    debugfunction,
    opts=followRedirOpts)
 
  result$response
}

