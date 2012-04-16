# TODO: Add comment
# 
# Author: mfuria
###############################################################################

setMethod(
  f = "addFile",
  signature = signature("SynapseLocationOwner", "ANY", "ANY"),
  definition = function(entity, file, path){
    addFile(entity@fileCache, file, path)
  }
)

setMethod(
    f = "moveFile",
    signature = signature("SynapseLocationOwner", "ANY", "ANY"),
    definition = function(entity, src, dest){
      moveFile(entity@fileCache, src, dest)
    }
)

setMethod(
    f = "deleteFile",
    signature = signature("SynapseLocationOwner", "ANY"),
    definition = function(entity, file){
      deleteFile(entity@fileCache, file)
    }
)

names.SynapseLocationOwner <-
    function(x)
{
  c("objects", "cacheDir", "files")
}


setMethod(
    f = "[",
    signature = "SynapseLocationOwner",
    definition = function(x, i, j, ...){
      if(length(as.character(as.list(substitute(list(...)))[-1L])) > 0L || !missing(j))
        stop("incorrect number of subscripts")
      if(is.numeric(i)){
        if(any(i > length(names(x))))
          stop("subscript out of bounds")
        i <- names(x)[i]
      }else if(is.character(i)){
        if(!all(i %in% names(x)))
          stop("undefined objects selected")
      }else{
        stop(sprintf("invalid subscript type '%s'", class(i)))
      }
      retVal <- lapply(i, function(i){
            if(i=="objects"){
              objects <- getObjects(slot(x, i))
            }
            slot(x@objects, i)
          }
      )
      names(retVal) <- i
      retVal
    }
)

setMethod(
    f = "[[",
    signature = "SynapseLocationOwner",
    definition = function(x, i, j, ...){
      if(length(as.character(as.list(substitute(list(...)))[-1L])) > 0L || !missing(j))
        stop("incorrect number of subscripts")
      if(length(i) > 1)
        stop("subscript out of bounds")
      x[i][[1]]
    }
)

setMethod(
    f = "$",
    signature = "SynapseLocationOwner",
    definition = function(x, name){
      x[[name]]
    }
)


