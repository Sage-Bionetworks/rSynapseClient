##
## synapseDownloadFromRepoService
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
synapseDownloadFromRepoService<-
  function (downloadUri, curlHandle = getCurlHandle(), cacheDir = synapseCacheDir(), opts = .getCache("curlOpts"), versionId = NULL)
{
  if (is.null(cacheDir)) stop(paste("cacheDir is required. synapseCacheDir() returns ", synapseCacheDir()))
  
  ## Download the file to the cache
  destfile <- .generateCacheDestFile(downloadUri, versionId)
  
  synapseDownloadFromRepoServiceToDestination(downloadUri=downloadUri, destfile=destfile, curlHandle=curlHandle, opts=opts)
}


synapseDownloadFromRepoServiceToDestination<-function(downloadUri, destfile=tempfile(), curlHandle = getCurlHandle(), opts = .getCache("curlOpts")) {
  # check own version, stopping if blacklisted
  checkBlackList()
  
  # make sure permanent redirects have been resolved
  resolvePermanentRedirects(synapseRepoServiceEndpoint())
  
  # the url for the repo service
  downloadUrl<-sprintf("%s%s", synapseRepoServiceEndpoint()$endpoint, downloadUri)
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
  header <- switch(authMode(),
    auth = .stuffHeaderAuth(header),
    hmac = .stuffHeaderHmac(header, sprintf("%s%s", getEndpointPrefixForService("REPO"), downloadUriWithoutParams)),
    stop("Unknown auth mode: %s. Could not build header", authMode())
  )		
  
  # we start with the common request options, then add the headers
  opts$httpheader <- header
  
  destfile <- .curlWriterDownload(url=downloadUrl, destfile=destfile, curlHandle=curlHandle, opts=opts)
  .checkCurlResponse(curlHandle)
  
  destfile
}