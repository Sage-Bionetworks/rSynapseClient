# synapseGetFollowingPermanentRedirects
#
# wraps getURL in RCurl adding logic for following permanent redirects
#
# Note: getURL also has redirect following logic, but doesn't 
# return the new location to which the client is redirected.
# Therefore we implement the redirect logic ourselves, capturing
# the location and updating the 'repo' or 'auth' endpoint accordingly.
#
# Note:  This method ONLY executes GET requests, not PUT, POST, or DELETE
# since following other redirects is not kosher:
# From
# http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
# Sect. 10.3.2:  "If the 301 status code is received in response to a request 
# other than GET or HEAD, the user agent MUST NOT automatically redirect the 
# request unless it can be confirmed by the user, since this might change the 
# conditions under which the request was issued."
# 
# Author: brucehoff
###############################################################################


synapseGetFollowingPermanentRedirects<-function(
  uri, # omitting the endpoint
  endpoint=synapseServiceEndpoint(service="REPO"), # REPO, AUTH, or FILE
  httpheader, # the headers
  curl, # the curl handle
  debugfunction = NULL,
  .opts) {
  customrequest<-"GET"
  noRedirOpts<-.opts
  noRedirOpts$followlocation<-NULL # do NOT include 'followlocation'
  noRedirOpts$header<-TRUE
  MAX_REDIRECTS<-.getCache("webRequestMaxRedirects")
  if (is.null(MAX_REDIRECTS) || MAX_REDIRECTS<1) stop(sprintf("Illegal value for MAX_REDIRECTS %d.", MAX_REDIRECTS))
  
  # initialize the global cache entry for the service
  if (is.null(endpoint$service)) stop("Service field is required.")
  synapseServiceEndpoint(endpoint$service, endpoint$endpoint)
  
  for (redirs in 1:MAX_REDIRECTS) { 
    
    url<-paste(endpoint$endpoint, uri, sep="")
    
    result<-getURLWithRetries(url,
      NULL, # the request body
      customrequest, # the request method
      httpheader, # the headers
      curl, # the curl handle
      debugfunction,
      opts=noRedirOpts)
    
    if (result$httpStatus==301) {
      redirectLocation<-result$response$headers[["Location"]]
      if (is.null(redirectLocation)) stop("received redirect status but no redirect location")
      # uri should be at the end of the redirect location
      uriStart <- regexpr(uri, redirectLocation, fixed=T)
      if (uriStart<0) stop(sprintf("%s does not appear in %s", uri, redirectLocation))
      if (uriStart+nchar(uri)!=1+nchar(redirectLocation)) stop(sprintf("%s does not come at the end of %s", uri, redirectLocation))
      redirectEndpoint<-substr(redirectLocation, 1, uriStart-1)
      synapseServiceEndpoint(endpoint$service, redirectEndpoint)
      endpoint<-synapseServiceEndpoint(endpoint$service)
    } else {
      return(result)
    }
  }
  stop(sprintf("Exceeded max redirects (%d)", MAX_REDIRECTS))
}

