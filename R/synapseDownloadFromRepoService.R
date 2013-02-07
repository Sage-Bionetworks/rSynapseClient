##
## synapseDownloadFromRepoService
##
## for a wiki page attachment, downloadUri would be:
## /{ownerObjectType}/{ownerObjectId}/wiki/{wikiId}/attachment?fileName={attachmentFileName}
##
## returns the destfile
##
##
## author:  bruce.hoff@sagebase.org
##

synapseDownloadFromRepoService<-function(downloadUri, destfile=tempfile()) {
  # check own version, stopping if blacklisted
  checkBlackList()
  
  # make sure permanent redirects have been resolved
  resolvePermanentRedirects("REPO")
  
  # the url for the repo service
  downloadUrl<-sprintf("%s%s", synapseRepoServiceEndpoint(), downloadUri)
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
    hmac = .stuffHeaderHmac(header, sprintf("%s%s", .getRepoEndpointPrefix(), downloadUriWithoutParams)),
    stop("Unknown auth mode: %s. Could not build header", authMode())
  )		
  
  # we start with the common request options...
  opts <- .getCache("curlOpts")
  # ...and add the headers
  opts$httpheader <- header
  
  curlHandle<-getCurlHandle()
  destfile <- .curlWriterDownload(url=downloadUrl, destfile=destfile, curlHandle=curlHandle, opts=opts)
  .checkCurlResponse(curlHandle)
  
  destfile
}