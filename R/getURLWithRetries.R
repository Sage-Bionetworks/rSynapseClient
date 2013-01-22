
getURLWithRetries<-function(url,
  postfields = NULL, # the request body
  customrequest="GET", # the request method
  httpheader=NULL, # the headers
  curl=getCurlHandle(), # the curl handle
  debugfunction = NULL,
  opts, 
  maxTries=10 # the number of tries when timeout or 503 is encountered.  1=no retries
) {
  # for exponential retries, used for 503 errors and timeouts (see SYNR-296)
  INITIAL_BACKOFF_SECONDS <- 1
  BACKOFF_MULTIPLIER <- 2 # i.e. the back off time is (initial)*[multiplier^(# retries)]
  if (maxTries<1) stop(sprintf("Illegal parameter maxTries %d.", maxTries))
  
  optsWithHeader<-opts
  optsWithHeader$header<-TRUE
  
  
  backoff<-INITIAL_BACKOFF_SECONDS
  for (retry in 1:maxTries) {
    if (is.null(postfields)) {
      rawResponse<-try(.getURLIntern(url, 
          customrequest=customrequest, 
          httpheader=httpheader, 
          curl=curl, 
          debugfunction=debugfunction,
          .opts=optsWithHeader
        ), silent=T)
    } else {
      rawResponse<-try(.getURLIntern(url, 
          postfields=postfields, 
          customrequest=customrequest, 
          httpheader=httpheader, 
          curl=curl, 
          debugfunction=debugfunction,
         .opts=optsWithHeader
        ), silent=T)
    }
    
    if (class(rawResponse)=="try-error") {
      if ((regexpr("connect() timed out", rawResponse[[1]], fixed=T)[1]>=0) || 
        (regexpr("Connection reset by peer", rawResponse[[1]], fixed=T)[1]>=0)) {
        # then it's a time out
        Sys.sleep(backoff)
        backoff <- backoff * BACKOFF_MULTIPLIER
      } else {
        # then it's some other error
        stop(rawResponse[[1]])
      }
    } else {
      httpStatus<-.getCurlInfo(curl)$response.code
      if (httpStatus==503) {
        # then we retry
        Sys.sleep(backoff)
        backoff <- backoff * BACKOFF_MULTIPLIER
      } else {
        break
      }
    }
  }
  if (class(rawResponse)=="try-error") {
    # then we gave up on the exponential retry
    stop(rawResponse[[1]])
  }
  
  # note, if we continue to get 503s after max exponential retries, then we just return the 503 result

  #message(rawResponse)
  response<-parseHttpResponse(rawResponse)
  
  list(response=response, httpStatus=httpStatus)
}


# this is added for unit testing purposes, providing a function to override
.getURLIntern<-function(url, 
  postfields,
  customrequest, 
  httpheader, 
  curl, 
  debugfunction,
  .opts
) {
  if (missing(postfields)) {
    getURL(url=url, 
      customrequest=customrequest, 
      httpheader=httpheader, 
      curl=curl, 
      debugfunction=debugfunction,
      .opts=.opts
    )
  } else {
    getURL(url=url, 
      postfields=postfields, 
      customrequest=customrequest, 
      httpheader=httpheader, 
      curl=curl, 
      debugfunction=debugfunction,
      .opts=.opts
    )
  }
}

# this is added for unit testing purposes, providing a function to override
.getCurlInfo<-function(curlHandle) {
  getCurlInfo(curlHandle)
}


