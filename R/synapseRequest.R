# synapseRequest
#
# calls getURL in RCurl letting RCurl follow redirects
# looks up synapse endpoint to construct URL from the given URI
# 
# Author: brucehoff
###############################################################


synapseRequest<-function(
  uri, # omitting the endpoint
  endpoint=synapseServiceEndpoint("REPO"), # one of REPO, AUTH, FILE
  postfields = NULL, # the request body
  customrequest, # the request method
  httpheader, # the headers
  curl, # the curl handle
  debugfunction = NULL,
  .opts) {
  
  endpoint<-resolvePermanentRedirects(endpoint)
    
  opts<-.opts
  opts$header<-TRUE
  
  url <- paste(endpoint$endpoint, uri, sep="")
  
  result<-getURLWithRetries(url,
    postfields, # the request body
    customrequest, # the request method
    httpheader, # the headers
    curl, # the curl handle
    debugfunction,
    opts=opts)
 
  result$response
}

