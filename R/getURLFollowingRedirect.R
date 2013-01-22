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
  .opts, 
  maxTries # the number of tries when timeout or 503 is encountered.  1=no retries
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
    
    result<-getURLWithRetries(url,
      postfields, # the request body
      customrequest, # the request method
      httpheader, # the headers
      curl, # the curl handle
      debugfunction,
      opts=noRedirOpts, 
      maxTries)
    
    if (result$httpStatus==301) {
      redirectLocation<-result$response$headers[["Location"]]
      if (is.null(redirectLocation)) stop("received redirect status but no redirect location")
      # uri should be at the end of the redirect location
      uriStart <- regexpr(uri, redirectLocation, fixed=T)
      if (uriStart<0) stop(sprintf("%s does not appear in %s", uri, redirectLocation))
      if (uriStart+nchar(uri)!=1+nchar(redirectLocation)) stop(sprintf("%s does not come at the end of %s", uri, redirectLocation))
      redirectEndpoint<-substr(redirectLocation, 1, uriStart-1)
      if (!isRepoRequest) {
        synapseAuthServiceEndpoint(redirectEndpoint)
      } else {
        synapseRepoServiceEndpoint(redirectEndpoint)
      }
    } else {
      return(result$response$body)
    }
  }
  stop(sprintf("Exceeded max redirects (%d)", MAX_REDIRECTS))
}

