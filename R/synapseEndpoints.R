# 
# Author: brucehoff
###############################################################################


synSetEndpoints<-function(repoEndpoint, authEndpoint, fileEndpoint, portalEndpoint) {
  if (missing(repoEndpoint) &&
      missing(authEndpoint) &&
      missing(fileEndpoint) &&
      missing(portalEndpoint)) {
    synapseResetEndpoints()
  }
  if (!missing(repoEndpoint)) synapseRepoServiceEndpoint(repoEndpoint)
  if (!missing(authEndpoint)) synapseAuthServiceEndpoint(authEndpoint)
  if (!missing(fileEndpoint)) synapseFileServiceEndpoint(fileEndpoint)
  if (!missing(portalEndpoint)) synapsePortalEndpoint(portalEndpoint)
}

synGetEndpoints<-function() {
  list(repo=synapseRepoServiceEndpoint()$endpoint, 
    auth=synapseAuthServiceEndpoint()$endpoint, 
    file=synapseFileServiceEndpoint()$endpoint, 
    portal=synapsePortalEndpoint()$endpoint)
}
