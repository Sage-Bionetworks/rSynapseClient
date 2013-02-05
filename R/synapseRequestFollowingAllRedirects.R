# synapseRequestFollowingAllRedirects
#
# calls getURL in RCurl letting RCurl follow redirects
# looks up synapse endpoint to construct URL from the given URI
# 
# Author: brucehoff
###############################################################


synapseRequestFollowingAllRedirects<-function(
  uri, # omitting the endpoint
  service="REPO", # one of REPO, AUTH, FILE
  postfields = NULL, # the request body
  customrequest, # the request method
  httpheader, # the headers
  curl, # the curl handle
  debugfunction = NULL,
  .opts) {
  
  # need to make sure that permanent redirects have been resolved:
  redirectResolvedKey <- sprintf("permanent.redirects.resolved.%s", service)
  redirectResolvedStatus <- .getCache(redirectResolvedKey)
  if (is.null(redirectResolvedStatus)) {
    # this will follow redirects and update the service endpoint globally
    redirectResult<-synapseGetFollowingPermanentRedirects(
      uri="/nonexistentservice", # non-existent service, just to trigger the redirect
      service=service,
      httpheader=httpheader,
      curl=curl,
      debugfunction = debugfunction,
      .opts=.opts)  
     # we expect a 404 error since we called a non-existent service
     if (redirectResult$httpStatus!=404) stop(sprintf("Expected HTTP Status 404 but found %s", redirectResult$httpStatus))
    .setCache(redirectResolvedKey, TRUE)
  }
    
  followRedirOpts<-.opts
  followRedirOpts$followlocation<-T # DO include 'followlocation'
  followRedirOpts$header<-TRUE
  
  if (service=="REPO") {
    url <- paste(synapseRepoServiceEndpoint(), uri, sep="")
  } else if (service=="AUTH") {
    url <- paste(synapseAuthServiceEndpoint(), uri, sep="")
  } else if (service=="FILE") {
    url <- paste(synapseFileServiceEndpoint(), uri, sep="")
  } else {
    stop(sprintf("Unexpected service: %s.", service))
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

