## Send a get request to synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseGet <- 
  function(uri, 
    endpoint=synapseServiceEndpoint("REPO"), 
    curlHandle=getCurlHandle(), 
    anonymous = .getCache("anonymous"), 
    opts = .getCache("curlOpts"), 
    entity = NULL, 
    checkHttpStatus=T
)
{
  ## constants
  kMethod <- "GET"
  ## end constants
  
  .synapseGetDelete(uri = uri, 
    endpoint = endpoint,
    requestMethod = kMethod,  
    curlHandle = curlHandle, 
    anonymous = anonymous, 
    opts = opts,
    entity = entity,
    checkHttpStatus=checkHttpStatus
  )
}

