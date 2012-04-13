## reset synapse endpoints to their default values
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseResetEndpoints <-
  function()
{
  synapseAuthServiceEndpoint("https://auth-alpha.sagebase.org/auth/v1")
  synapseRepoServiceEndpoint("https://repo-alpha.sagebase.org/repo/v1")
  synapsePortalEndpoint("http://synapse.sagebase.org")
}
