## Delete a synapse entity
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseDelete <- 
  function(
    uri, 
    endpoint=synapseServiceEndpoint("REPO"), 
    entity, 
    curlHandle=getCurlHandle(), 
    anonymous=FALSE, 
    opts = .getCache("curlOpts"),
    checkHttpStatus=T
)
{
  ## constants
  kMethod <- "DELETE"
  ## end constants
  
  if(!missing(entity)){
    .synapsePostPut(uri = uri, 
      entity = entity,
      endpoint = endpoint,
      requestMethod = kMethod, 
      curlHandle = curlHandle, 
      anonymous = anonymous,  
      opts = opts,
      checkHttpStatus=checkHttpStatus
    )
  }else{
    .synapseGetDelete(uri = uri, 
      requestMethod = kMethod,
      endpoint = endpoint,
      curlHandle = curlHandle, 
      anonymous = anonymous, 
      opts = opts,
      checkHttpStatus=checkHttpStatus
    )
  }
}