##
## downloadFromService
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

## Download a file from a path provided via redirect from a Synapse service
# This function assumes that there will be a redirect to the actual download URL.
# We capture the URL then pass it to the next function which directs it to the right
# handler for the protocol it has (e.g. https vs. sftp).
# NOTE:  downloadUri must contain redirect=FALSE request parameter
downloadFromService<-function(
  downloadUri, 
  endpointName="REPO", 
  destdir=tempdir(), 
  curlHandle = getCurlHandle(), 
  extraRetryStatusCodes=NULL) {
  # check own version, stopping if blacklisted
  checkBlackList()
  
  opts = .getCache("curlOpts")
  # we don't want to follow redirects, we just want to return the redirectURL
  opts$followlocation<-NULL
  redirectUrl<-synapseGet(downloadUri, 
				  endpoint=synapseServiceEndpoint(endpointName),   
				  opts = opts)
  
  result<-protocolSpecificFileDownload(url=redirectUrl, destdir=destdir, 
		  curlHandle = curlHandle, extraRetryStatusCodes=extraRetryStatusCodes)
  
  .checkCurlResponse(object=curlHandle)
  
  result
}