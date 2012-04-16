# TODO: Add comment
# 
# Author: furia
###############################################################################

setMethod(
  f = "initialize",
  signature = "ArchiveOwner",
  definition = function(.Object){
    .Object@fileCache <- new("FileCache", ...)
    .Object
  }
)

setMethod(
  f = "addFile",
  signature = signature("ArchiveOwner", "ANY", "ANY"),
  definition = function(entity,file, path){
    addFile(entity@fileCache, file, path)
  }
)