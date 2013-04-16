## Send a post request to Synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapsePost <- 
  function(uri,
    entity, 
    service="REPO", 
    curlHandle=getCurlHandle(), 
    anonymous = FALSE, 
   opts = .getCache("curlOpts"),
   checkHttpStatus=T
)
{
  ## constants
  kMethod <- "POST"
  ## end constants
  
  .synapsePostPut(uri = uri, 
    entity = entity, 
    service = service,
    requestMethod = kMethod,  
    curlHandle = curlHandle, 
    anonymous = anonymous, 
    opts = opts,
    checkHttpStatus=checkHttpStatus
  )
}