# getURLFollowingRedirect
# wraps getURL in RCurl adding logic for following redirects
# Note: getURL also has redirect following logic, but doesn't support
# the option to suppress mapping 'POST' to 'GET' when redirecting a request
# Therefore we implement the redirect logic ourselves.
# 
# Author: brucehoff
###############################################################################


getURLFollowingRedirect<-function(
  uri, # omitting the endpoint
  isRepoRequest=TRUE, # if FALSE then it's an auth request
  postfields = NULL, # the request body
  customrequest, # the request method
  httpheader, # the headers
  curl, # the curl handle
  debugfunction = NULL,
  .opts
) {
  noRedirOpts<-.opts
  noRedirOpts$followlocation<-NULL # do NOT include 'followlocation'
  noRedirOpts$header<-TRUE
  MAX_REDIRECTS <- 3
  for (redirs in 1:MAX_REDIRECTS) { 
    if (isRepoRequest) {
      url <- paste(synapseRepoServiceEndpoint(), uri, sep="")
    } else {
      url <- paste(synapseAuthServiceEndpoint(), uri, sep="")
    }
    rawResponse<-getURL(url, 
      postfields=postfields, 
      customrequest=customrequest, 
      .opts=noRedirOpts, 
      httpheader=httpheader, 
      curl=curl, 
      debugfunction=debugfunction)
    
    response<-parseHttpResponse(rawResponse)
    httpStatus<-getCurlInfo(curl)$response.code
    if (httpStatus!=response$statusCode) stop(sprintf("%d!=%d", httpStatus, response$statusCode))
    # TODO what about other 3XX statuses?
    if (httpStatus==301) {
      redirectLocation<-response$headers[["Location"]]
      if (is.null(redirectLocation)) stop("received redirect status but no redirect location")
      redirectEndpoint<-parseEndpoint(redirectLocation, uri)
      if (isAuthenticationRequest) {
        synapseAuthServiceEndpoint(redirectEndpoint)
      } else {
        synapseRepoServiceEndpoint(redirectEndpoint)
      }
    } else {
      return(response$body)
    }
  }
  
  
}
