## Delete a synapse entity
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseDelete <- 
  function(uri, isRepoRequest=TRUE, entity, curlHandle=getCurlHandle(), anonymous=FALSE, 
    opts = .getCache("curlOpts"))
{
  ## constants
  kMethod <- "DELETE"
  ## end constants
  
  if(!missing(entity)){
    .synapsePostPut(uri = uri, 
      entity = entity,
      isRepoRequest = isRepoRequest,
      requestMethod = kMethod, 
      curlHandle = curlHandle, 
      anonymous = anonymous,  
      opts = opts		
    )
  }else{
    .synapseGetDelete(uri = uri, 
      requestMethod = kMethod,
      isRepoRequest = isRepoRequest,
      curlHandle = curlHandle, 
      anonymous = anonymous, 
      opts = opts
    )
  }
}