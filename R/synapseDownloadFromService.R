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
synapseDownloadFromServiceToDestination<-function(
  downloadUri, 
  endpointName="REPO", 
  destfile=tempfile(), 
  curlHandle = getCurlHandle(), 
  opts = .getCache("curlOpts"),
  extraRetryStatusCodes=NULL) {
  # check own version, stopping if blacklisted
  checkBlackList()
  
  # make sure permanent redirects have been resolved
  endpoint<-synapseServiceEndpoint(endpointName)
  resolvePermanentRedirects(endpoint)
  
  # the url for the repo service
  downloadUrl<-sprintf("%s%s", endpoint$endpoint, downloadUri)
  paramIndex<-regexpr("?", downloadUri, fixed=T)
  if(paramIndex>0) {
    downloadUriWithoutParams<-substr(downloadUri, 1, paramIndex-nchar("?"))
  } else {
    downloadUriWithoutParams<-downloadUri
  }
  
  # we start with the common header info, as used for other requests
  header <- .getCache("curlHeader")
  
  # must remove the common Content-Type header, as it will break file download from S3
  if (any(names(header)=='Content-Type')) {
    header <- header[-1*which(names(header)=='Content-Type')]
  }
  
  # we add in the authentication info
  header <- .stuffHeaderHmac(header, sprintf("%s%s", getEndpointPrefixForService(endpointName), downloadUriWithoutParams))
  
  # we start with the common request options, then add the headers
  opts$httpheader <- header
  
  webRequestResult<-webRequestWithRetries(
    fcn=function(curlHandle) {
      .curlWriterDownload(url=downloadUrl, destfile=destfile, curlHandle=curlHandle, opts=opts)
    },
    curlHandle=curlHandle,
    extraRetryStatusCodes=extraRetryStatusCodes
  )
  
  destfile <- webRequestResult$result
  
  .checkCurlResponse(object=curlHandle)
  
  destfile
}