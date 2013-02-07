## Send a put request to Synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapsePut <- 
  function(
    uri, 
    entity, 
    service="REPO", 
    curlHandle=getCurlHandle(), 
    anonymous = FALSE, 
    opts = .getCache("curlOpts")
)
{
  ## constants
  kMethod <- "PUT"
  ## end constants
  
  .synapsePostPut(uri = uri, 
    entity = entity, 
    service = service,
    requestMethod = kMethod,
    curlHandle = curlHandle, 
    anonymous = anonymous,  
    opts = opts
  )
}