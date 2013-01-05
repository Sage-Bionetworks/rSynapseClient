## Send a get request to synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseGet <- 
  function(uri, 
    isRepoRequest=TRUE, 
    curlHandle=getCurlHandle(), 
    anonymous = .getCache("anonymous"), 
    opts = .getCache("curlOpts"), 
    entity = NULL, 
    checkHttpStatus=T,
    maxTries=10 # the number of tries when timeout or 503 is encountered.  1=no retries
)
{
  ## constants
  kMethod <- "GET"
  ## end constants
  
  .synapseGetDelete(uri = uri, 
    isRepoRequest = isRepoRequest,
    requestMethod = kMethod,  
    curlHandle = curlHandle, 
    anonymous = anonymous, 
    opts = opts,
    entity = entity,
    checkHttpStatus=checkHttpStatus, 
    maxTries=maxTries
  )
}

