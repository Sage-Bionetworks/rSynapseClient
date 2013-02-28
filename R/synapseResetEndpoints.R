## reset synapse endpoints to their default values
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseResetEndpoints <-
  function()
{
  synapseAuthServiceEndpoint("https://auth-prod.prod.sagebase.org/auth/v1")
  synapseRepoServiceEndpoint("https://repo-prod.prod.sagebase.org/repo/v1")
  synapseFileServiceEndpoint("https://file-prod.prod.sagebase.org/file/v1")
  synapseVersionsServiceEndpoint("http://versions.synapse.sagebase.org/synapseRClient")
  synapsePortalEndpoint("http://synapse.sagebase.org")
}
