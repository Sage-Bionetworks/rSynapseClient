## Delete a synapse entity
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseDelete <- 
  function(
    uri, 
    service="REPO", 
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
      service = service,
      requestMethod = kMethod, 
      curlHandle = curlHandle, 
      anonymous = anonymous,  
      opts = opts,
      httpStatus
    )
  }else{
    .synapseGetDelete(uri = uri, 
      requestMethod = kMethod,
      service = service,
      curlHandle = curlHandle, 
      anonymous = anonymous, 
      opts = opts,
      checkHttpStatus=checkHttpStatus
    )
  }
}