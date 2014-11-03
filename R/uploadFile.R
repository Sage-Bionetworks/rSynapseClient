# Uploads a file to Synapse, taking into account the uploadDestinations
# associated with the parent project.
# 
# Author: brucehoff
###############################################################################

getCredentialsForHost<-function(hostNameWithProtocol) {
  username<-NULL
  password<-NULL
  config <- try(ConfigParser())
  if (class(config) != "try-error") {
    usernameKey<-sprintf("%s:username", hostNameWithProtocol)
    if (all(Config.hasOption(config, "authentication", usernameKey))) {
      username <- Config.getOption(config, "authentication", usernameKey)
    }
    passwordKey<-sprintf("%s:password", hostNameWithProtocol)
    if (all(Config.hasOption(config, "authentication", passwordKey))) {
      password <- Config.getOption(config, "authentication", passwordKey)
    }
  }
  # TODO if not in config file, prompt user
  return(list(username=username, password=password))
}

uploadFileToEntity<-function(filepath, containerEntityId, uploadDestination, curlHandle=getCurlHandle(), contentType=NULL) {
  # get the uploadDestinations for the containerEntity
  uploadDestinationResponse<-synRestGET(sprintf("/uploadDestinations/%s", containerEntityId))
  uploadDestintations<-createTypedListFromList(uploadDestinationResponse, UploadDestinationList) # TODO auto-generate UploadDestinationList
  if (length(uploadDestintations)==0) stop(sprintf("Entity %s has no upload destinations to choose from.", containerEntityId))
  if (missing(uploadDestination)) {
    uploadDestination<-uploadDestinations[[1]]
  }
  if (!is.null(uploadDestination@banner)) message(uploadDestination@banner)
  if (is(uploadDestination, S3UploadDestination)) {
    chunkedFileUpload(filepath, curlHandle, contentType)
  } else if (is(uploadDestination, ExternalUploadDestination)) {
    if (uploadDestination@uploadType=="S3") {
      stop("Upload to specified S3 destination is not yet supported.")
    } else if (uploadDestination@uploadType=="SFTP") {
      if (!(RsshPackageIsAvailable() && require("Rssh"))) 
        stop("Upload target is SFTP but Rssh package not installed/available.  Please install Rssh and try again.")
      parsed<-.ParsedUrl(uploadDestination@url)
      credentials<-getCredentialsForHost()
      fileName<-basename(filepath)
      createMissingDirectories(parsedUrl@host, credentials$username, credentials$password, parserUrl@path)
      remotePathAndFile<-file.path(parserUrl@path, fileName)
      sftpUpload(parsedUrl@host, credentials$username, credentials$password, remotePathAndFile, filepath)
    } else if (uploadDestination@uploadType=="HTTPS") {
      stop("Upload to specified HTTPS destination is not yet supported.")
    }
  } else {
    stop(sprintf("Unrecognized UploadDestination type %s", class(uploadDestination)))
  }
}

RsshPackageIsAvailable<-function() {
  any(.packages(all.available=T)=="Rssh")
}

createMissingDirectories<-function(host, username, password) {
  directories<-strsplit(path,"/")[[1]]
  if (length(directories)==0) {
    return
  }
  cumulativeDirectory<-""
  for (dir in directories) {
    cumulativeDirectory<-paste(cumulativeDirectory, dir, sep="/") # TODO Not quite right
    if (length(cumulativeDirectory)>0) {
      if (!sftpDirectoryExists(host, username, password, cumulativeDirectory)) {
        sftpMakeDirectory(host, username, password, cumulativeDirectory)
      } 
    }
  }
}
