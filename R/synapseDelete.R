## Delete a synapse entity
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseDelete <- 
  function(
    uri, 
    endpoint=synapseServiceEndpoint("REPO"), 
    curlHandle=getCurlHandle(), 
    anonymous=FALSE, 
    opts = .getCache("curlOpts"),
    checkHttpStatus=T
)
{
  ## constants
  kMethod <- "DELETE"
  ## end constants
  
  if(!missing(entity))
    stop("Delete request should not have a request body.")

  .synapseGetDelete(uri = uri, 
      requestMethod = kMethod,
      endpoint = endpoint,
      curlHandle = curlHandle, 
      anonymous = anonymous, 
      opts = opts,
      checkHttpStatus=checkHttpStatus
    )

}