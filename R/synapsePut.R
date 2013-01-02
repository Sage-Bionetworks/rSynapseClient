## Send a put request to Synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapsePut <- 
  function(uri, entity, isRepoRequest=TRUE, curlHandle=getCurlHandle(), anonymous = FALSE, 
    opts = .getCache("curlOpts"))
{
  ## constants
  kMethod <- "PUT"
  ## end constants
  
  .synapsePostPut(uri = uri, 
    entity = entity, 
    isRepoRequest = isRepoRequest,
    requestMethod = kMethod,
    curlHandle = curlHandle, 
    anonymous = anonymous,  
    opts = opts
  )
}