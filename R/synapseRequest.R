# synapseRequest
#
# calls getURL in RCurl after resolving permanent redirects
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
  .opts,
  logErrorsToSynapse=TRUE,
  extraRetryStatusCodes=NULL) {
  
  endpoint<-resolvePermanentRedirects(endpoint, logErrorsToSynapse=logErrorsToSynapse)
    
  url <- paste(endpoint$endpoint, uri, sep="")
  
  result<-getURLWithRetries(url,
    postfields, # the request body
    customrequest, # the request method
    httpheader, # the headers
    curl, # the curl handle
    debugfunction,
    opts=.opts,
	logErrorsToSynapse,
	extraRetryStatusCodes=extraRetryStatusCodes)
 
  list(headers=result$parsedHeaders$headers, body=result$body)
}

