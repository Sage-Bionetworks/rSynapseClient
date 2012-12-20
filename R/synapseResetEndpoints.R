## reset synapse endpoints to their default values
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseResetEndpoints <-
  function()
{
  synapseAuthServiceEndpoint("https://auth-prod.sagebase.org/auth/v1")
  synapseRepoServiceEndpoint("https://repo-prod.sagebase.org/repo/v1")
  synapseVersionsServiceEndpoint("http://versions.synapse.sagebase.org/synapseRClient")
  synapsePortalEndpoint("http://synapse.sagebase.org")
}
