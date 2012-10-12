setMethod(
  f = "AttachmentOwner",
  signature = "character",
  definition = function(path, archiveFileName){
    own <- new("AttachmentOwner")
    own@fileCache <- getFileCache(path)
    if(!missing(archiveFileName)){
      own@fileCache$setArchiveFileName(archiveFileName)
    }
    own
  }
)

setMethod(
  f = "AttachmentOwner",
  signature = "missing",
  definition = function(){
    own <- new("AttachmentOwner")
    own@fileCache <- getFileCache(tempfile())
    own
  }
)
