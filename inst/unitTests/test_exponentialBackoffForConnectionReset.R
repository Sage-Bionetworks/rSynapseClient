# Teest expontential backoff/retry
# 
# Author: brucehoff
###############################################################################

.setUp <-
  function()
{
  synapseClient:::.setCache("exponentialBackoffWillFail", TRUE)
  ## this function will 'time out' the first time but pass the second time
  myGetUrl <- function(url, 
    customrequest, 
    .opts, 
    httpheader, 
    curl, 
    debugfunction) { 
    if (regexpr("/version", url, fixed=T)>=0) return("HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"version\":\"foo\"}")
    willFail <-synapseClient:::.getCache("exponentialBackoffWillFail")
    if (willFail) {
      synapseClient:::.setCache("exponentialBackoffWillFail", FALSE)
      stop("Error in function (type, msg, asError = TRUE)  : Connection reset by peer") 
    } else {
      return("HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"foo\":\"bar\"}")
    }
  }
  attr(myGetUrl, "origDef") <- synapseClient:::.getURLIntern
  assignInNamespace(".getURLIntern", myGetUrl, "synapseClient")
}

.tearDown <-
  function()
{
  assignInNamespace(".getURLIntern", attr(synapseClient:::.getURLIntern, "origDef"), "synapseClient")
  unloadNamespace('synapseClient')
  library(synapseClient)
}




unitTestExponentialBackoffShouldTimeout <- 
  function()
{
  opts<-synapseClient:::.getCache("curlOpts")
  opts$timeout.ms<-100
  
  # this will time out
  shouldBeError<-try(synapseClient:::synapseGet("/query?query=select+id+from+entity+limit==500", 
      anonymous=T, opts=opts, maxTries=1), silent=T)
  checkEquals("try-error", class(shouldBeError))
   
}

unitTestExponentialBackoffShouldComplete <- 
  function()
{
  opts<-synapseClient:::.getCache("curlOpts")
  opts$timeout.ms<-100
  
  # this will complete
  synapseClient:::synapseGet("/query?query=select+id+from+entity+limit==500", 
    anonymous=T, opts=opts, maxTries=3)
  
}