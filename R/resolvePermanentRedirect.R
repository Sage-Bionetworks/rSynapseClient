##
## resolvePermanentRedirects
##
## resolve the permanent redirects for a service endpoint
##
## 'service' is REPO, AUTH, or FILE
##
####################################################################

resolvePermanentRedirects<-function(service) {
  
  # need to make sure that permanent redirects have been resolved:
  redirectResolvedKey <- sprintf("permanent.redirects.resolved.%s", service)
  redirectResolvedStatus <- .getCache(redirectResolvedKey)
  if (is.null(redirectResolvedStatus)) {
    # this will follow redirects and update the service endpoint globally
    redirectResult<-synapseGetFollowingPermanentRedirects(
      uri="/nonexistentservice", # non-existent service, just to trigger the redirect
      service=service,
      httpheader=.getCache("curlHeader"),
      curl=getCurlHandle(),
      debugfunction = NULL,
      .opts=.getCache("curlOpts"))  
    # we expect a 404 error since we called a non-existent service
    # might also get a 401 error if authentication is required
    status<-redirectResult$httpStatus
    if (status<400 || status>=500) stop(sprintf("Expected HTTP Status 4xx but found %s", redirectResult$httpStatus))
    .setCache(redirectResolvedKey, TRUE)
  }
  
}