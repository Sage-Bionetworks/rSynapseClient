##
## synapseDownloadFromService
##
## for a wiki page attachment, downloadUri would be:
## /{ownerObjectType}/{ownerObjectId}/wiki/{wikiId}/attachment?fileName={attachmentFileName}
## for a File entity the downloadUri would be:
## 		/entity/{enityId}/file
## or if a version is specified:
## 		/entity/{entityId}/version/{versionNumber}/file
##
## returns the destfile
##
##
## author:  bruce.hoff@sagebase.org
##

## this is the analog of 'synapseDownloadFile', switching to the Repo-file service
# NOTE:  downloadUri must contain redirect=FALSE request parameter
synapseDownloadFromServiceToDestination<-function(
  downloadUri, 
  endpointName="REPO", 
  destfile=tempfile(), 
  curlHandle = getCurlHandle(), 
  extraRetryStatusCodes=NULL) {
  # check own version, stopping if blacklisted
  checkBlackList()
  
  redirectUrl<-synRestGET(downloadUri, synapseServiceEndpoint(endpointName))
  
  synapseDownloadFileToDestination
  
  webRequestResult<-webRequestWithRetries(
    fcn=function(curlHandle) {
		synapseDownloadHttpFileToDestination(
				url=redirectUrl, destfile=destfile, curlHandle=curlHandle, opts=.getCache("curlOpts"))
    },
    curlHandle=curlHandle,
    extraRetryStatusCodes=extraRetryStatusCodes
  )
  
  destfile <- webRequestResult$result
  
  .checkCurlResponse(object=curlHandle)
  
  destfile
}