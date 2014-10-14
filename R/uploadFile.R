# Uploads a file to Synapse, taking into account the uploadDestinations
# associated with the parent project.
# 
# Author: brucehoff
###############################################################################


uploadFileToEntity<-function(filepath, containerEntityId, uploadDestination, curlHandle=getCurlHandle(), contentType=NULL) {
  # get the uploadDestinations for the containerEntity
  uploadDestinationResponse<-synRestGET(sprintf("/uploadDestinations/%s", containerEntityId))
  uploadDestintations<-createTypedListFromList(uploadDestinationResponse, UploadDestinationList) # TODO auto-generate UploadDestinationList
  if (length(uploadDestintations)==0) stop(sprintf("Entity %s has no upload destinations to choose from.", containerEntityId))
  if (missing(uploadDestination)) {
    uploadDestination<-uploadDestinations[[1]]
  } else {
    foundit<-false
    for (i in 1:length(uploadDestintations)) {
      if (identical(uploadDestination, uploadDestinations[[i]])) {
        foundit<-true
        break
      }
    }
    if (!foundit) stop("Chosen upload destination is not allowed for this project.")
  }
  if (!is.null(uploadDestination@banner)) message(uploadDestination@banner)
  if (is(uploadDestination, S3UploadDestination)) {
    chunkedFileUpload(filepath, curlHandle, contentType)
  } else if (is(uploadDestination, ExternalUploadDestination)) {
    if (uploadDestination@uploadType=="S3") {
      stop("Upload to specified S3 destination is not yet supported.")
    } else if (uploadDestination@uploadType=="SFTP") {
      sftpUpload(filePath, uploadDestination@url, curlHandle=getCurlHandle())
    } else if (uploadDestination@uploadType=="HTTPS") {
      stop("Upload to specified HTTPS destination is not yet supported.")
    }
  } else {
    stop(sprintf("Unrecognized UploadDestination type %s", class(uploadDestination)))
  }
}
