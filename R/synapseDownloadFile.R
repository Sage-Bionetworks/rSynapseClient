## Download a file from Synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

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

