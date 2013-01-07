## Delete a synapse entity
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseDelete <- 
  function(
    uri, 
    isRepoRequest=TRUE, 
    entity, 
    curlHandle=getCurlHandle(), 
    anonymous=FALSE, 
    opts = .getCache("curlOpts"),
    maxTries = 10 # the number of tries when timeout or 503 is encountered.  1=no retries
)
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
      opts = opts,
      maxTries=maxTries
    )
  }else{
    .synapseGetDelete(uri = uri, 
      requestMethod = kMethod,
      isRepoRequest = isRepoRequest,
      curlHandle = curlHandle, 
      anonymous = anonymous, 
      opts = opts,
      maxTries=maxTries
    )
  }
}