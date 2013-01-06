# Teest expontential backoff/retry
# 
# Author: brucehoff
###############################################################################

.setUp <-
  function()
{
  synapseClient:::.setCache("httpRequestCount", 0)
  synapseClient:::.setCache("httpStatus", 200)
  ## this function will 'time out' the first time but pass the second time
  myGetUrl <- function(url, 
    customrequest, 
    httpheader, 
    curl, 
    debugfunction,
    .opts 
    ) { 
    if (regexpr("/version", url, fixed=T)>=0) {
      synapseClient:::.setCache("httpStatus", 200)
      return("HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"version\":\"foo\"}")
    }
    httpRequestCount <-synapseClient:::.getCache("httpRequestCount")
    synapseClient:::.setCache("httpRequestCount", httpRequestCount+1)
    if (httpRequestCount<1) { # first time, it fails
      synapseClient:::.setCache("httpStatus", 503)
      return("HTTP/1.1 503 Service Unavailable\r\nContent-Type: application/json\r\n\r\n")
    } else {
      synapseClient:::.setCache("httpStatus", 200)
      return("HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"foo\":\"bar\"}")
    }
  }
  attr(myGetUrl, "origDef") <- synapseClient:::.getURLIntern
  assignInNamespace(".getURLIntern", myGetUrl, "synapseClient")
  
  myGetCurlInfo<-function(curlHandle=NULL) {
    list(response.code=synapseClient:::.getCache("httpStatus"))
  }
  attr(myGetCurlInfo, "origDef") <- synapseClient:::.getCurlInfo
  assignInNamespace(".getCurlInfo", myGetCurlInfo, "synapseClient")
}

.tearDown <-
  function()
{
  assignInNamespace(".getURLIntern", attr(synapseClient:::.getURLIntern, "origDef"), "synapseClient")
  assignInNamespace(".getCurlInfo", attr(synapseClient:::.getCurlInfo, "origDef"), "synapseClient")
  unloadNamespace('synapseClient')
  library(synapseClient)
}




unitTestExponentialBackoffShouldFail <- 
  function()
{
  opts<-synapseClient:::.getCache("curlOpts")
  opts$timeout.ms<-100
  
  # this will get a 503, and an empty response
  shouldBeEmpty<-synapseClient:::synapseGet("/query?query=select+id+from+entity+limit==500", 
      anonymous=T, opts=opts, checkHttpStatus=FALSE, maxTries=1)
  checkEquals("", shouldBeEmpty)
  checkEquals(503, synapseClient:::.getCurlInfo()$response.code)
}

unitTestExponentialBackoffShouldComplete <- 
  function()
{
  opts<-synapseClient:::.getCache("curlOpts")
  opts$timeout.ms<-100
  
  # this will complete
  result<-synapseClient:::synapseGet("/query?query=select+id+from+entity+limit==500", anonymous=T, opts=opts, maxTries=3)
  checkEquals(list(foo="bar"), result)
  checkEquals(200, synapseClient:::.getCurlInfo()$response.code)
}
