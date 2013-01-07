## Send a post request to Synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapsePost <- 
  function(uri,
    entity, 
    isRepoRequest=TRUE, 
    curlHandle=getCurlHandle(), 
    anonymous = FALSE, 
   opts = .getCache("curlOpts"),
   maxTries=10 # the number of tries when timeout or 503 is encountered.  1=no retries
)
{
  ## constants
  kMethod <- "POST"
  ## end constants
  
  .synapsePostPut(uri = uri, 
    entity = entity, 
    isRepoRequest = isRepoRequest,
    requestMethod = kMethod,  
    curlHandle = curlHandle, 
    anonymous = anonymous, 
    opts = opts,
    maxTries=maxTries
  )
}