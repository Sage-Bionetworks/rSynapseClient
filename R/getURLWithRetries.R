
getURLWithRetries<-function(url,
  postfields = NULL, # the request body
  customrequest="GET", # the request method
  httpheader=NULL, # the headers
  curl=getCurlHandle(), # the curl handle
  debugfunction = NULL,
  opts
) {
  
  result<-webRequestWithRetries(
      fcn=function(curlHandle) {
        if (is.null(postfields)) {
          .getURLIntern(url, 
              customrequest=customrequest, 
              httpheader=httpheader, 
              curl=curlHandle, 
              debugfunction=debugfunction
            )
        } else {
          .getURLIntern(url, 
              postfields=postfields, 
              customrequest=customrequest, 
              httpheader=httpheader, 
              curl=curlHandle, 
              debugfunction=debugfunction
            )
        }
      },
      curlHandle=curl,
      extraRetryStatusCode=NULL
  )
  parsedHeaders<-parseHttpHeaders(result$headers)
  list(response=result$body, httpStatus=parsedHeaders$statusCode)
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
  h = basicTextGatherer()
	
  if (missing(postfields)) {
	  body<-getURL(url=url, 
      customrequest=customrequest, 
      httpheader=httpheader, 
      curl=curl, 
      debugfunction=debugfunction,
	  headerfunction=h$update,
      .opts=.opts
    )
	list(headers)
  } else {
	  body<-getURL(url=url, 
      postfields=postfields, 
      customrequest=customrequest, 
      httpheader=httpheader, 
      curl=curl, 
      debugfunction=debugfunction,
	  headerfunction=h$update,
      .opts=.opts
    )
  }
  list(headers=h$value(), body=body)
}

# this is added for unit testing purposes, providing a function to override
.getCurlInfo<-function(curlHandle) {
  getCurlInfo(curlHandle)
}


