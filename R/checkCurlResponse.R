## Check the response of the curl request. throw an error if an http error
## code is returned.
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.checkCurlResponse <- function(object, response, call.=FALSE){
  if(class(object) != "CURLHandle") stop("invalid curl handle")
  info <- getCurlInfo(object)
  if(info$response.code != 0 & (info$response.code < 200 || info$response.code >= 300)){
    message <- paste("HTTP Error:", info$response.code, "for request", info$effective.url)
    if(!missing(response)) {
      if(grepl('The AWS Access Key Id you provided does not exist in our records.', response)) {
        # The propagation delay for new IAM users is anywhere from 5 to 60 seconds
        stop(paste("Try your request again, but if it doesn't work within 2 minutes, contact the Synapse team for help.", 
            message, response, sep = '\n'), call.=call.)
      }
      else {
        stop(paste(message, response, sep = '\n'), call.=call.)
      }
    } else{
      stop(message, call.=call.)
    }
  }
}
