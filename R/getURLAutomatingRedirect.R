# getURLAutomatingRedirect
# calls getURL in RCurl letting RCurl follow redirects
# 
# Author: brucehoff
#######################################################


getURLAutomatingRedirect<-function(
  uri, # omitting the endpoint
  isRepoRequest=TRUE, # if FALSE then it's an auth request
  postfields = NULL, # the request body
  customrequest, # the request method
  httpheader, # the headers
  curl, # the curl handle
  debugfunction = NULL,
  .opts) {
  followRedirOpts<-.opts
  followRedirOpts$followlocation<-T # DO include 'followlocation'
  followRedirOpts$header<-TRUE
  
  if (isRepoRequest) {
    url <- paste(synapseRepoServiceEndpoint(), uri, sep="")
  } else {
    url <- paste(synapseAuthServiceEndpoint(), uri, sep="")
  }
  
  result<-getURLWithRetries(url,
    postfields, # the request body
    customrequest, # the request method
    httpheader, # the headers
    curl, # the curl handle
    debugfunction,
    opts=followRedirOpts)
 
  result$response$body
}

