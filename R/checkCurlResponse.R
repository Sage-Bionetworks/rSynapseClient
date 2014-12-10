## Check the response of the curl request. throw an error if an http error
## code is returned.
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.checkCurlResponse <- function(object, response, call.=FALSE, logErrorToSynapse=FALSE){
  if(class(object) != "CURLHandle") stop("invalid curl handle")
  info <- .getCurlInfo(object)
  if(info$response.code != 0 & (info$response.code < 200 || info$response.code >= 300)) {
    url<-info$effective.url
    host<-""
    if (!is.null(url) && nchar(url)>0) {
      parsedUrl<-.ParsedUrl(url)
      host<-parsedUrl@host
    }
    label<-paste(info$response.code, host)
    message <- paste("HTTP Error:", info$response.code, "for request", url)
    if (!missing(response)) {
      if (logErrorToSynapse) logErrorToSynapse(label, paste(message, response, sep = '\n'))
      stop(paste(message, response, sep = '\n'), call.=call.)
    } else {
      if (logErrorToSynapse) logErrorToSynapse(label, message=message)
      stop(message, call.=call.)
    }
  }
}

# gets the status code for the given CurlHandle,
getStatusCode<-function(object) {
  if(class(object) != "CURLHandle") stop("invalid curl handle")
  info <- .getCurlInfo(object)
  info$response.code
}
