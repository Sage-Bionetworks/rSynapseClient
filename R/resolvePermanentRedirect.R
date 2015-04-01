##
## resolvePermanentRedirects
##
## resolve the permanent redirects for a service endpoint
##
##
####################################################################

resolvePermanentRedirects<-function(endpoint) {
  
  # we need to related permanent redirects to some service (e.g. REPO, AUTH, FILE)
  # if no service is specified then we use the 'host' portion of the endpoint as a proxy
  if (is.null(endpoint$service)) {
    endpoint$service<-endpoint$endpointHost
  }
  
  # need to make sure that permanent redirects have been resolved:
  redirectResolvedKey <- sprintf("permanent.redirects.resolved.%s", endpoint$service)
  redirectResolvedStatus <- .getCache(redirectResolvedKey)
  if (is.null(redirectResolvedStatus)) {
    # this will follow redirects and update the service endpoint globally
    redirectResult<-synapseGetFollowingPermanentRedirects(
      uri="/nonexistentservice", # non-existent service, just to trigger the redirect
      endpoint=endpoint,
      httpheader=.getCache("curlHeader"),
      curl=getCurlHandle(),
      debugfunction = NULL,
      .opts=.getCache("curlOpts"))  
    # we expect a 404 error since we called a non-existent service
    # might also get a 401 error if authentication is required
    status<-redirectResult$parsedHeaders$statusCode
    if (status<400 || status>=500) stop(sprintf("Expected HTTP Status 4xx but found %s", status))
    .setCache(redirectResolvedKey, TRUE)
  }
  
  # return the up-to-date endpoint
  synapseServiceEndpoint(endpoint$service)
}