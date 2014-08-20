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
    checkHttpStatus=checkHttpStatus
  )
}

