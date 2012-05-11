## Test handling of md5sum checks after download
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.setUp <- 
    function()
{
  badFilePath <- tempfile()
  goodFilePath <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file = goodFilePath)
  write.table(d, file = badFilePath)
  
  ## override synapseDownloadFile
  myCurlWriterDownload <-
      function(url, destfile=tempfile(), curlHandle = getCurlHandle(), writeFunction=synapseClient:::.getCache('curlWriter'), opts = synapseClient:::.getCache("curlOpts"))
  {
    if(url == synapseClient:::.getCache("goodURL")){
      filePath <- synapseClient:::.getCache("goodFilePath")
    }else if(url == synapseClient:::.getCache("badURL")){
      filePath <- synapseClient:::.getCache("badFilePath")
    }else{
      stop("invalid URL")
    }
    file.copy(filePath,destfile)
    destfile	
  }
  suppressWarnings(unloadNamespace('synapseClient'))
  assignInNamespace(".curlWriterDownload", myCurlWriterDownload, "synapseClient")
  attachNamespace("synapseClient")
  synapseClient:::.setCache("goodFilePath", goodFilePath)
  synapseClient:::.setCache("badFilePath", badFilePath)
  synapseClient:::.setCache("checksum", as.character(tools::md5sum(goodFilePath)))
  synapseClient:::.setCache("goodURL", "http://fakeUrl.goodChecksum")
  synapseClient:::.setCache("badURL", "http://fakeUrl.badChecksum")
  
  
  synapseCacheDir(file.path(tempdir(), ".synapseCache"))
  
}

.tearDown <-
    function()
{
  ## put back the overridden functions and original cache
  suppressWarnings(unloadNamespace("synapseClient"))
  attachNamespace("synapseClient")
  suppressWarnings(unloadNamespace("synapseClient"))
  attachNamespace("synapseClient")
}

unitTestChecksumMatches <-
    function()
{
  checkTrue(!is.null(synapseCacheDir()))
  file <- synapseClient:::synapseDownloadFile(synapseClient:::.getCache("goodURL"),synapseClient:::.getCache("checksum"))
  checkEquals(as.character(tools::md5sum(file)), synapseClient:::.getCache("checksum"))
}

unitTestLegalFileName <-
    function() {
  checkEquals("im_a soemthing _lam_ scientist :\\ and I use _.zip", synapseClient:::legalFilePath("im'a soemthing (lam) scientist :\\ and I use *.zip"))
  checkEquals("__________", synapseClient:::legalFilePath("()`'<>\"|?*"))
}

unitTestSynapseDownloadToLegalFile <- function() {
  checkTrue(!is.null(synapseCacheDir()))
  url <-synapseClient:::.getCache("goodURL")
  filePath <-paste(tempdir(), "foo'bar|.txt")
  legalFileName <- synapseClient:::synapseDownloadToLegalFile(url, filePath)
  checkEquals(paste(tempdir(), "foo_bar_.txt"), legalFileName)
  
}

unitTestBadChecksum <-
    function()
{
  file <- synapseClient:::.curlWriterDownload(synapseClient:::.getCache('badURL'))
  checkEquals(as.character(tools::md5sum(file)), as.character(tools::md5sum(synapseClient:::.getCache("badFilePath"))))
  checkTrue(as.character(tools::md5sum(synapseClient:::.getCache("goodFilePath"))) != as.character(tools::md5sum(synapseClient:::.getCache("badFilePath"))))
  checkException(synapseClient:::synapseDownloadFile(synapseClient:::.getCache("badURL"), synapseClient:::.getCache("checksum")))
}
