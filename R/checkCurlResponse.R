## Check the response of the curl request. throw an error if an http error
## code is returned.
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.checkCurlResponse <- function(object, response, call.=FALSE){
  if(class(object) != "CURLHandle") stop("invalid curl handle")
  info <- .getCurlInfo(object)
  if(info$response.code != 0 & (info$response.code < 200 || info$response.code >= 300)){
    message <- paste("HTTP Error:", info$response.code, "for request", info$effective.url)
    if (!missing(response)) {
      logErrorToSynapse(info$response.code, paste(message, response, sep = '\n'))
      stop(paste(message, response, sep = '\n'), call.=call.)
    } else {
      logErrorToSynapse(info$response.code, message=message)
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
