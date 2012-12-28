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
    if (is.null(postfields)) {
      rawResponse<-getURL(url, 
        customrequest=customrequest, 
        .opts=noRedirOpts, 
        httpheader=httpheader, 
        curl=curl, 
        debugfunction=debugfunction)
    } else {
      rawResponse<-getURL(url, 
        postfields=postfields, 
        customrequest=customrequest, 
        .opts=noRedirOpts, 
        httpheader=httpheader, 
        curl=curl, 
        debugfunction=debugfunction)
    }
    
    response<-parseHttpResponse(rawResponse)
    httpStatus<-getCurlInfo(curl)$response.code

    # TODO what about other 3XX statuses?
    if (httpStatus==301) {
      redirectLocation<-response$headers[["Location"]]
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
      return(response$body)
    }
  }
  
  
}
