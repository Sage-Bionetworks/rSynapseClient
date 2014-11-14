# Uploads a file to Synapse, taking into account the uploadDestinations
# associated with the parent project.
# 
# Author: brucehoff
###############################################################################

uploadFileToEntity<-function(filePath, uploadDestination, curlHandle=getCurlHandle(), contentType=NULL) {
  if (!is.null(uploadDestination@banner)) message(uploadDestination@banner)
  if (is(uploadDestination, "S3UploadDestination")) {
    chunkedUploadFile(filepath=filePath, curlHandle=curlHandle, contentType=contentType)
  } else if (is(uploadDestination, "ExternalUploadDestination")) {
    if (uploadDestination@uploadType=="S3") {
      stop("Upload to specified S3 destination is not yet supported.")
    } else if (uploadDestination@uploadType=="SFTP") {
      if (!(RsshPackageIsAvailable() && require("Rssh"))) 
        stop("Upload target is SFTP but Rssh package not installed/available.  Please install Rssh and try again.")
      urlDecodedDestination<-URLdecode(uploadDestination@url)
      parsedUrl<-.ParsedUrl(urlDecodedDestination)
      credentials<-getCredentialsForHost(parsedUrl)
      fileName<-basename(filePath)
      destinationPath<-parsedUrl@path
      # path from url starts with "/" but this isn't meant to mean to go to the root of the remote file system
      if (substring(destinationPath,1,1)=="/") destinationPath<-substring(destinationPath, 2)
      createMissingDirectories(parsedUrl@host, credentials$username, credentials$password, destinationPath)
      remotePathAndFile<-file.path(destinationPath, fileName)
      success<-sftpUpload(parsedUrl@host, credentials$username, credentials$password, remotePathAndFile, filePath)
      if (!success) stop(sprintf("Failed to upload %s to %s", filePath, parsedUrl@host))
      # TODO make sure the following URL is URL-encoded
      synapseLinkExternalFile(URLencode(paste(urlDecodedDestination, fileName, sep="/")), fileName, contentType)
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

getCredentialsForHost<-function(parsedUrl) {
  hostNameWithProtocol<-sprintf("%s://%s", parsedUrl@protocol, parsedUrl@host)
  username<-NULL
  password<-NULL
  config <- try(ConfigParser())
  if (class(config) != "try-error") {
    if (all(Config.hasOption(config, hostNameWithProtocol, "username"))) {
      username <- Config.getOption(config, hostNameWithProtocol, "username")
    }
    if (all(Config.hasOption(config, hostNameWithProtocol, "password"))) {
      password <- Config.getOption(config, hostNameWithProtocol, "password")
    }
  }
  if (is.null(username)) {
    username <- .getUsername(sprintf("Username for %s: ", parsedUrl@host))
  }
  if (is.null(password)) {
    password <- .getPassword(sprintf("Password for %s:  ", parsedUrl@host))
  }
  return(list(username=username, password=password))
}

createMissingDirectories<-function(host, username, password, path) {
  for (dir in getDirectorySequence(path)) {
    if (!sftpDirectoryExists(host, username, password, dir)) {
        success<-sftpMakeDirectory(host, username, password, dir)
        if (!success) stop(sprintf("Failed to create %s on %s", dir, host))
    } 
  }
}

# e.g. for "foo/bar", returns c("foo", "foo/bar")
getDirectorySequence<-function(path) {
  if (length(path)==0 || nchar(path)==0) return("")
  if (path=="/") return("/")
  directories<-strsplit(path,"/")[[1]]
  result<-c()
  for (dir in directories) {
      if (length(result)==0) {
        result<-dir
      } else {
        result<-append(result, paste(result[length(result)], dir, sep="/"))
      }
  }
  result[which(nchar(result)>0)]
}
