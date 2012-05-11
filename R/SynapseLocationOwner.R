# TODO: Add comment
# 
# Author: mfuria
###############################################################################

setMethod(
  f = "initialize",
  signature = "SynapseLocationOwner",
  definition = function(.Object){
    .Object@archOwn <- new("ArchiveOwner")
    setPackageName(env=.Object)
    .Object
  }
)


setMethod(
    f = "loadEntity",
    signature = "SynapseLocationOwner",
    definition = function(entity){
      ##if(length(entity$files) == 0)
      entity <- downloadEntity(entity)
      
      entity@archOwn <- loadObjectsFromFiles(entity@archOwn)
      
      
#    if(is.null(annotValue(entity, "format"))){
#      ##setPackageName(sprintf("entity%s", propertyValue(entity, "id")), env = entity@location@objects)
#      return(entity)
#    }
#    entity@location@objects <- switch(annotValue(entity, "format"),
#      rbin = .loadRbinaryFiles(file.path(entity@location@cacheDir,entity@location@files)),
#      sageBioCurated = .loadSageBioPacket(entity),
#      entity@location@objects
#    )
#    setPackageName(sprintf("entity%s", propertyValue(entity, "id")), env = entity@location@objects)
      entity
    }
)


setMethod(
  f = "storeEntity",
  signature = "SynapseLocationOwner",
  definition = function(entity){
    if(is.null(propertyValue(entity, "id"))){
      entity <- createEntity(entity)
    }else{
      entity <- updateEntity(entity)
    }
    if(length(entity$files) > 0L){
      ## create the archive on disk (which will persist file metaData to disk)
      createArchive(entity@archOwn)
      
      ## upload the archive  file (storeFile also updates the entity)
      entity <- storeFile(entity ,file.path(entity@archOwn@fileCache$getCacheRoot(), entity@archOwn@fileCache$getArchiveFile()))
    }
    entity
  }
)

setMethod(
  f = "storeEntityFiles",
  signature = "SynapseLocationOwner",
  definition = function(entity){
    storeEntity(entity)
  }
)


setMethod(
  f = "createEntity",
  signature = "SynapseLocationOwner",
  definition = function(entity){
    cfun <- getMethod("createEntity", "SynapseEntity")
    ee <- cfun(entity)
    ee@archOwn <- entity@archOwn
    ee
  }
)


setMethod(
  f = "storeFile",
  signature = signature("SynapseLocationOwner", "character"),
  definition = function(entity, filePath) {
    
    if(!file.exists(filePath))
      stop('archive file does not exist')
    
    ## make sure that the entity is up to date
    if(is.null(propertyValue(entity, "id"))){
      ## Create the LocationOwner in Synapse
      entity <- createEntity(entity)
    } else {
      ## Update the LocationOwner in Synapse just in case any other fields were changed
      ## TODO is this needed?
      entity <- updateEntity(entity)
    } 
    
    entity <- tryCatch(
      synapseClient:::.performRUpload(entity, filePath),
      error = function(e){
        warning(sprintf("failed to upload data file, please try again: %s", e))
        return(entity)
      }
    )
    
    ## move the data file from where it is to the local cache directory
    if(is.null(propertyValue(entity, 'locations')[[1]]['path']))
      stop("NULL URL")
    parsedUrl <- synapseClient:::.ParsedUrl(propertyValue(entity, 'locations')[[1]]['path'])
    destdir <- file.path(synapseCacheDir(), gsub("^/", "", parsedUrl@pathPrefix))
    destdir <- path.expand(destdir)
    
    ## set the cachRoot to the new location this method should do the right 
    ## thing. don't move if src and dest are the same. make sure there are
    ## no straggler files left behind, clean up temp directories, etc.
    entity@archOwn <- setCacheRoot(entity@archOwn, destdir, clean = TRUE)
  
    ## unpack the archive into it's new root directory.
    entity@archOwn <- unpackArchive(entity@archOwn)
    invisible(entity)
  }
)

setMethod(
  f = "getEntity",
  signature = "SynapseLocationOwner",
  definition = function(entity){
    gfun <- getMethod("getEntity", "SynapseEntity")
    ee <- gfun(entity)
    ee@archOwn <- entity@archOwn
    ee
  }
)

setMethod(
    f = "updateEntity",
    signature = "SynapseLocationOwner",
    definition = function(entity){
      ufun <- getMethod("updateEntity", "SynapseEntity")
      updatedEntity <- ufun(entity)
      slot(updatedEntity, "archOwn") <- entity@archOwn
      updatedEntity
    }
)

setMethod(
  f = "downloadEntity",
  signature = "SynapseLocationOwner",
  definition = function(entity){
    ## check whether user has signed agreement
    ## euals are broken. ignore for now
#    if(!hasSignedEula(entity)){
#      if(!.promptSignEula())
#        stop(sprintf("Visit https://synapse.sagebase.org to sign the EULA for entity %s", propertyValue(entity, "id")))
#      if(!.promptEulaAgreement(entity))
#        stop("You must sign the EULA to download this dataset. Visit http://synapse.sagebase.org for more information.")
#      .signEula(entity)
#    }
    
    ## download the archive from S3
    ## Note that we just use the first location, to future-proof this we would use the location preferred
    ## by the user, but we're gonna redo this in java so no point in implementing that here right now
#    dfun <- getMethod("downloadEntity", "SynapseEntity")
#    ee <- dfun(entity)
#    ee@archOwn <- entity@archOwn
  
    if(is.null(propertyValue(entity, "locations")[[1]][['path']]))
      return(entity)
    
    ## download the file
    url <- propertyValue(entity, "locations")[[1]][['path']]
    
    parsedUrl <- synapseClient:::.ParsedUrl(url)
    destfile <- file.path(synapseCacheDir(), gsub("^/", "", parsedUrl@path))
    destfile <- path.expand(destfile)
    cacheRoot <- gsub(basename(destfile), "", destfile, fixed=TRUE)
    entity@archOwn <- setCacheRoot(entity@archOwn, cacheRoot, clean=TRUE)

    
    ## passing the md5 sum causes this funciton to only download the file
    ## if the cached copy does not match that md5 sum
    if(file.exists(destfile)){
      archiveFile = synapseDownloadFile(url, propertyValue(entity, "md5"))
    }else{
      archiveFile = synapseDownloadFile(url)
    }
    archiveFile <- normalizePath(archiveFile)
    
    
    fc <- getFileCache(archiveFile)
    entity@archOwn@fileCache <- fc
    ## unpack the archive
    entity@archOwn <- unpackArchive(entity@archOwn)
    
    entity
  }
)

setMethod(
    f = "deleteEntity",
    signature = "SynapseLocationOwner",
    definition = function(entity){
      entity@archOwn@fileCache$delete()
      dfun <- getMethod("deleteEntity", "SynapseEntity")
      entity <- dfun(entity)
      invisible(entity) 
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
  c("objects", "cacheDir", "files", names.SynapseEntity(x))
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
              retVal <- files(x)
            }else if(i %in% names.SynapseEntity(x)){
              fun <- getMethod("[", "SynapseEntity")
              retVal <- fun(x, i)
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
  f = "files",
  signature = "SynapseLocationOwner",
  definition = function(object){
    files(object@archOwn)
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

setMethod(
  f = "attach",
  signature = signature(what = "SynapseLocationOwner"),
  definition = function (what, pos = 2, name = getPackageName(what), warn.conflicts = TRUE){
    attach(what@archOwn, pos= pos, name = name, warn.conflicts = warn.conflicts)
  }
)

setMethod(
  f = "detach",
  signature = signature(name = "SynapseLocationOwner"),
  definition = function (name)
  {
    detach(name@archOwn)
  }
)

setMethod(
  f = "setPackageName",
  signature = signature(env = "SynapseLocationOwner"),
  definition = function(pkg, env)
  {
    if(missing(pkg))
      pkg <- basename(tempfile(pattern=as.character(class(env))))
    setPackageName(pkg = pkg, env = env@archOwn)
  }
)

setMethod(
  f = "getPackageName",
  signature = signature(where = "SynapseLocationOwner"),
  definition = function (where, create = TRUE)
  {
    getPackageName(where = where@archOwn, create = create)
  }
)


setReplaceMethod("$", 
  signature = "SynapseLocationOwner",
  definition = function(x, name, value) {
     if(!(name %in% names.SynapseEntity(x)))
      stop("invalid element")
    fun <- getMethod("$<-", "SynapseEntity")
    fun(x, name, value)
  }
)

