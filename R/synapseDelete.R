## Send a Delete request to the Repository Services
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

  .synapseGetDelete(uri = uri, 
      requestMethod = kMethod,
      endpoint = endpoint,
      curlHandle = curlHandle, 
      anonymous = anonymous, 
      opts = opts,
      checkHttpStatus=checkHttpStatus
    )

}