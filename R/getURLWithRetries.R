
getURLWithRetries<-function(url,
  postfields = NULL, # the request body
  customrequest="GET", # the request method
  httpheader=NULL, # the headers
  curl=getCurlHandle(), # the curl handle
  debugfunction = NULL,
  opts
) {
  
  optsWithHeader<-opts
  optsWithHeader$header<-TRUE
  
  result<-webRequestWithRetries(
      fcn=function(curlHandle) {
        if (is.null(postfields)) {
          .getURLIntern(url, 
              customrequest=customrequest, 
              httpheader=httpheader, 
              curl=curlHandle, 
              debugfunction=debugfunction,
              .opts=optsWithHeader
            )
        } else {
          .getURLIntern(url, 
              postfields=postfields, 
              customrequest=customrequest, 
              httpheader=httpheader, 
              curl=curlHandle, 
              debugfunction=debugfunction,
              .opts=optsWithHeader
            )
        }
      },
      curlHandle=curl,
      extraRetryStatusCode=NULL
  )
  response<-parseHttpResponse(result$result)
  list(response=response, httpStatus=result$httpStatus)
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


