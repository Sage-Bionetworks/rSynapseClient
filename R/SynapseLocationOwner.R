# TODO: Add comment
# 
# Author: mfuria
###############################################################################

setMethod(
  f = "initialize",
  signature = "SynapseLocationOwner",
  definition = function(.Object){
    .Object@archOwn <- new("ArchiveOwner")
    .Object
  }
)

setMethod(
  f = "addFile",
  signature = signature("SynapseLocationOwner", "character", "character"),
  definition = function(entity, file, path){
    entity@archOwn <- addFile(entity@archOwn, file, path)
    invisible(entity)
  }
)

setMethod(
    f = "addFile",
    signature = signature("SynapseLocationOwner", "character", "missing"),
    definition = function(entity, file){
      entity@archOwn <- addFile(entity@archOwn, file)
      invisible(entity)
    }
)

setMethod(
    f = "moveFile",
    signature = signature("SynapseLocationOwner", "character", "character"),
    definition = function(entity, src, dest){
      entity@archOwn <- moveFile(entity@archOwn, src, dest)
      invisible(entity)
    }
)

setMethod(
    f = "deleteFile",
    signature = signature("SynapseLocationOwner", "character"),
    definition = function(entity, file){
      entity@archOwn <- deleteFile(entity@archOwn, file)
      invisible(entity)
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
              retVal <- x@archOwn@objects
            }else if(i == "cacheDir"){
              retVal <- cacheDir(x@archOwn)
            }else if(i == "files"){
              retVal <- files(x@archOwn)
            }else{
              retVal <- NULL
            }
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

setMethod(
  f = "show",
  signature = "SynapseLocationOwner",
  definition = function(object){
    cat('An object of class "', class(object), '"\n', sep="")
    
    cat("Synapse Entity Name : ", properties(object)$name, "\n", sep="")
    cat("Synapse Entity Id   : ", properties(object)$id, "\n", sep="")
    
    if (!is.null(properties(object)$parentId))
      cat("Parent Id           : ", properties(object)$parentId, "\n", sep="")
    if (!is.null(properties(object)$type))
      cat("Type                : ", properties(object)$type, "\n", sep="")
    if (!is.null(properties(object)$versionNumber)) {
      cat("Version Number      : ", properties(object)$versionNumber, "\n", sep="")
      cat("Version Label       : ", properties(object)$versionLabel, "\n", sep="")
    }
    
    obj.msg <- summarizeObjects(object)
    if(!is.null(obj.msg)){
      cat("\n", obj.msg$count,":\n", sep="")
      cat(obj.msg$objects, sep="\n")
    }
    
    files.msg <- summarizeCacheFiles(object)
    if(!is.null(files.msg))
      cat("\n", files.msg$count, "\n", sep="")
    if(!is.null(propertyValue(object,"id"))){
      cat("\nFor complete list of annotations, please use the annotations() function.\n")
      cat(sprintf("To view this Entity on the Synapse website use the 'onWeb()' function\nor paste this url into your browser: %s\n", object@synapseWebUrl))
    }
  }
)

setMethod(
  f = "summarizeObjects",
  signature = "SynapseLocationOwner",
  definition = function(entity){
    msg <- NULL
    if(length(entity$objects  ) > 0){
      msg$count <- sprintf("loaded object(s)")
      objects <- objects(entity$objects)
      classes <- unlist(lapply(objects, function(object){paste(class(entity$objects[[object]]), collapse=":")}))
      msg$objects <- sprintf('[%d] "%s" (%s)', 1:length(objects), objects, classes)
    }
    msg
  }
)

setMethod(
  f = "summarizeCacheFiles",
  signature = "SynapseLocationOwner",
  definition = function(entity){
    ## if Cached Files exist, print them out
    msg <- NULL
    if(length(entity$cacheDir) != 0){
      msg$count <- sprintf('%d File(s) cached in "%s"', length(entity$files), entity$cacheDir)
      if(length(entity$files) > 0)
        msg$files <- sprintf('[%d] "%s"',1:length(entity$files), entity$files)
    }
    msg
  }
)

setMethod(
  f = "loadObjectsFromFiles",
  signature = "SynapseLocationOwner",
  definition = function(owner){
    owner@archOwn <- loadObjectsFromFiles(owner@archOwn)
    invisible(owner)
  }
)



