## Download a file from Synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

legalFilePath<-function(filePath) {
	gsub("[()`'<>\"|?*]", "_", filePath)
}

# Note: This will be deleted along with the code for Locationable
synapseDownloadFile  <- 
  function (url, checksum, curlHandle = getCurlHandle(), cacheDir = synapseCacheDir(), opts = .getCache("curlOpts"), versionId = NULL)
{
	if (is.null(cacheDir)) stop(paste("cacheDir is required. synapseCacheDir() returns ", synapseCacheDir()))
	
  ## Download the file to the cache
  destfile <- .generateCacheDestFile(url, versionId)
  
  ## temporary hack for github url that does not contain file extension
  parsedUrl <- .ParsedUrl(url)
  if( parsedUrl@host=="github.com" ){
    splits <- strsplit(parsedUrl@pathPrefix, "/")
    if( splits[[1]][length(splits[[1]])] == "zipball" )
      destfile <- paste(destfile, ".zip", sep="")
    if( splits[[1]][length(splits[[1]])] == "tarball" )
      destfile <- paste(destfile, ".tar", sep="")
  }
  
  protocolSpecificFileDownload(url=url, destdir=dirname(destfile), opts=opts)$downloadedFile
}

# download file from source which may involve one of a variety of protocols
protocolSpecificFileDownload  <- 
  function (url, destdir, curlHandle = getCurlHandle(), opts = .getCache("curlOpts"), extraRetryStatusCodes)
{
  parsedUrl<-.ParsedUrl(url)
  protocol<-tolower(parsedUrl@protocol)
  if (protocol=="http" || protocol=="https" || protocol=="file" || protocol=="ftp") {
    result<-downloadHttpFile(url, destdir, curlHandle, opts, extraRetryStatusCodes)
  } else if (protocol=="sftp") {
    result<-downloadSftpFile(url, destdir)
  } else {
    stop(sprintf("Unsupported protocol %s", protocol))
  }
  result
}

# download file from source which is HTTP/HTTPS
downloadHttpFile  <- 
    function (url, destdir, curlHandle = getCurlHandle(), opts = .getCache("curlOpts"), extraRetryStatusCodes)
  {
  	if (!file.exists(destdir)){
   	 dir.create(destdir, recursive=TRUE)
  	}
  
	wrwrResult<-webRequestWithRetries(
		fcn=function(curlHandle) {
			.curlWriterDownload(url=url, destdir=destdir, opts = opts, curlHandle = curlHandle)
		},
		curlHandle=curlHandle,
		extraRetryStatusCode=NULL
	)
	
	wrwrResult$result
}

downloadSftpFile  <- 
  function (url, destdir)
{
  if (!(RsftpPackageIsAvailable() && require("Rsftp"))) 
    stop("File is hosted on SFTP server but Rsftp package not installed/available.  Please install Rsftp and try again.")
  parsedUrl<-.ParsedUrl(url)
  credentials<-getCredentialsForHost(parsedUrl)
  urlDecodedPath<-URLdecode(parsedUrl@path)
  filePath<-tempfile(tmpdir=destdir)
  success<-sftpDownload(parsedUrl@host, credentials$username, credentials$password, urlDecodedPath, filePath)
  if (!success) {
    message<-sprintf("Failed to download %s from %s", urlDecodedPath, parsedUrl@host)
    logErrorToSynapse(label=sprintf("sftp get %s", parsedUrl@host), message=)
    stop(message)
  }
  list(downloadedFile=filePath, fileName=URLdecode(parsedUrl@file))
}

