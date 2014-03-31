#
# Author: mfuria
###############################################################################

##
## This class exists to prevent entities from owning R binaries
##
setClass(
  Class = "LocationableWithoutBinaries",
  contains = c("Entity"),
  representation = representation(
    archOwn = "ArchiveOwner"
  ),
  prototype = prototype(
    properties = synapseClient:::SynapseProperties(synapseClient:::getEffectivePropertyTypes("org.sagebionetworks.repo.model.Locationable"))
  )
)

defineEntityConstructors("org.sagebionetworks.repo.model.Locationable", "LocationableWithoutBinaries", package="synapseClient")

##
## All Locationable entities that are allowed to own R binaries will inherit from this class
##
setClass(
    Class = "Locationable",
    contains = c("LocationableWithoutBinaries"),
    representation(
      objOwn = "CachingObjectOwner"
    )
)


##
## Locationable contructor
##
setMethod(
  f = "Locationable",
  signature = "list",
  definition = function(entity){
    ee <- new("Locationable")
    ee@properties <- entity
    ee
  }
)


setMethod(
  f = "attach",
  signature = signature(what = "LocationableWithoutBinaries"),
  definition = function (what, pos = 2, name = getPackageName(what), warn.conflicts = TRUE){
    attach(what@archOwn, pos= pos, name = name, warn.conflicts = warn.conflicts)
  }
)

setMethod(
  f = "detach",
  signature = signature(name = "LocationableWithoutBinaries"),
  definition = function (name)
  {
    detach(name@archOwn)
  }
)

setMethod(
  f = "attach",
  signature = signature(what = "Locationable"),
  definition = function (what, pos = 2, name = getPackageName(what@objOwn), warn.conflicts = TRUE){
    attach(what@objOwn, pos = pos, name = name, warn.conflicts = warn.conflicts)

    afun <- getMethod('attach', 'LocationableWithoutBinaries')
    afun(what, pos = pos, warn.conflicts = warn.conflicts)
  }
)

setMethod(
  f = "detach",
  signature = signature(name = "Locationable"),
  definition = function (name)
  {
    detach(name@objOwn)
    detach(name@archOwn)
  }
)

setMethod(
	f = "loadEntity",
	signature = signature("Locationable","missing"),
	definition = function(entity){

    lfun <- getMethod("loadEntity", signature("LocationableWithoutBinaries", "missing"))
    entity <- lfun(entity)
    entity@objOwn$objects@fileCache <- entity@archOwn@fileCache
		entity@objOwn <- loadObjectsFromFiles(entity@objOwn, clearEnvironment=TRUE)

		entity
	}
)

#######
## loadEntity methods
#######


##
## Load Locationable entity for the latests version. This method is for
## locationable entities that don't own binaries
##
setMethod(
    f = "loadEntity",
    signature = signature("LocationableWithoutBinaries", "missing"),
    definition = function(entity){
      if(!is.null(entity$properties$id))
        entity <- downloadEntity(entity)
      entity@archOwn <- loadObjectsFromFiles(entity@archOwn, clearEnvironment=TRUE)
      setFetchMethod(entity, "load")
      entity
    }
)

##
## Load a specific version of aLocationable entity for the latests version. This method is for
## locationable entities that don't own binaries. Specify version using a character
##
setMethod(
    f = "loadEntity",
    signature = signature("LocationableWithoutBinaries", "character"),
    definition = function(entity, versionId){
      ##if(length(entity$files) == 0)
      entity <- downloadEntity(entity, versionId)
      entity@archOwn <- loadObjectsFromFiles(entity@archOwn, clearEnvironment=TRUE)
      setFetchMethod(entity, "load")
      entity
    }
)


##
## Load a specific version of aLocationable entity for the latests version. This method is for
## locationable entities that don't own binaries. Specify version using a number
##
setMethod(
  f = "loadEntity",
  signature = signature("LocationableWithoutBinaries", "numeric"),
  definition = function(entity, versionId){
    loadEntity(entity, as.character(versionId))
  }
)



##
## Load the entity
##
setMethod(
  f = "loadEntity",
  signature = signature("Locationable","character"),
  definition = function(entity, versionId){

    lfun <- getMethod("loadEntity", signature("LocationableWithoutBinaries", "character"))
    entity <- lfun(entity, versionId)
    entity@objOwn$objects@fileCache <- entity@archOwn@fileCache
    entity@objOwn <- loadObjectsFromFiles(entity@objOwn, clearEnvironment=TRUE)

    entity
  }
)

setMethod(
  f = "loadEntity",
  signature = signature("Locationable","numeric"),
  definition = function(entity, versionId){
    loadEntity(entity, as.character(versionId))
  }
)

#####
## end load entity methods
#####

##
## Store the archive file for a locationable entity
##
setMethod(
  f = "storeFile",
  signature = signature("LocationableWithoutBinaries", "character"),
  definition = function(entity, filePath) {

    if(!file.exists(filePath))
      stop('archive file does not exist')

    ## make sure that the entity is up to date
    if(is.null(propertyValue(entity, "id"))){
      ## Create the Locationable in Synapse
      entity <- createEntity(entity)
      filePath <- file.path(entity@archOwn@fileCache$getCacheRoot(), basename(filePath))
    }

    entity <- tryCatch(
      .performRUpload(entity, filePath),
      error = function(e){
        warning(sprintf("failed to upload data file, please try again: %s", e))
        return(entity)
      }
    )

    ## move the data file from where it is to the local cache directory
    url <- entity$properties$locations[[1]][['path']]
    if(is.null(url))
      stop("NULL URL")
    destdir <- .generateCacheDestDir(url, entity$properties$versionNumber)

    ## set the cachRoot to the new location this method should do the right
    ## thing. don't move if src and dest are the same. make sure there are
    ## no straggler files left behind, clean up temp directories, etc.
    entity@archOwn <- setCacheRoot(entity@archOwn, destdir, clean = TRUE)

    ## make sure the fileCache gets added to the FileCacheFactory
    ##entity@archOwn <- ArchiveOwner(destdir)
    if(inherits(entity, "Locationable"))
      setFileCache(entity@objOwn, entity@archOwn@fileCache)

    ## unpack the archive into it's new root directory.
    entity@archOwn <- unpackArchive(entity@archOwn)
    invisible(entity)
  }
)


##
## Store a locationable entity. This method is a general-purpose method that must do the "right thing"
## based on the context. if the entity was retrieved using "getEntity", the method should store just
## the annotations and properties. If the was retrieved using "loadEntity", the method should store
## both the properties/annotations as well as the archive file. If the archive file hasn't changed
## since it was retrieved, it should not be uploaded.
##
setMethod(
  f = "storeEntity",
  signature = "LocationableWithoutBinaries",
  definition = function(entity){
    if(is.null(propertyValue(entity, "id"))){
      entity <- createEntity(entity)
    }else{
      method <- synapseClient:::getFetchMethod(entity)
      # I think this is redundant, as 'updateEntity' will be called below
      # so we fix by suppressing the extra call if it's to come shortly
      if (is.null(getFetchMethod(entity)) || getFetchMethod(entity) != "load"){
        entity <- updateEntity(entity)
      }
      if(!is.null(method))
        synapseClient:::setFetchMethod(entity, method)
    }

    if(!is.null(getFetchMethod(entity)) && getFetchMethod(entity) == "load"){
      ## create the archive on disk (which will persist file metaData to disk)
      file <- createArchive(entity@archOwn)

      if(!is.null(file)){
        ## upload the archive  file (storeFile also updates the entity)
        file <- file.path(entity@archOwn@fileCache$getCacheRoot(), file)

        ## note, this calls 'updateEntity' as a side effect
        entity <- storeFile(entity, file)
      }else{
        if(!is.null(entity$properties$locations)){
          entity <- deleteProperty(entity, "locations") # this may be the source of the problem described in SYNR-363
        }
        entity <- updateEntity(entity)
      }
    }
    entity
  }
)

##
## Method provided for backward compatibility
##
setMethod(
  f = "storeEntityFiles",
  signature = "LocationableWithoutBinaries",
  definition = function(entity){
    storeEntity(entity)
  }
)



setMethod(
  f = "getEntity",
  signature = signature("LocationableWithoutBinaries", "missing"),
  definition = function(entity){
    gfun <- getMethod("getEntity", signature("Entity", "missing"))
    ee <- gfun(entity)
    ee@archOwn <- entity@archOwn
    ee
  }
)

setMethod(
  f = "getEntity",
  signature = signature("Locationable", "missing"),
  definition = function(entity){
    gfun <- getMethod("getEntity", signature("LocationableWithoutBinaries", "missing"))
    ee <- gfun(entity)
    ee@objOwn <- entity@objOwn
    ee
  }
)

setMethod(
  f = "storeEntityObjects",
  signature = "Locationable",
  definition = function(entity){
    storeEntity(entity)
  }
)

setMethod(
    f = "updateEntity",
    signature = "LocationableWithoutBinaries",
    definition = function(entity){
      ufun <- getMethod("updateEntity", "Entity")
      updatedEntity <- ufun(entity)
      slot(updatedEntity, "archOwn") <- entity@archOwn
      updatedEntity
    }
)


setMethod(
  f = "updateEntity",
  signature = "Locationable",
  definition = function(entity){
    ufun <- getMethod("updateEntity", "LocationableWithoutBinaries")
    updatedEntity <- ufun(entity)
    updatedEntity@objOwn <- entity@objOwn
    updatedEntity
  }
)


#####
## Create Entity methods
#####

setMethod(
  f = "createEntity",
  signature = "LocationableWithoutBinaries",
  definition = function(entity){ 
    entity@archOwn@fileCache <- getFileCache(getFileCacheName(entity@archOwn@fileCache))
    archOwn <- entity@archOwn
    cfun <- getMethod("createEntity", "Entity")
    entity <- cfun(entity)
    entity@archOwn <- setCacheRoot(archOwn, entity@archOwn@fileCache$getCacheRoot(), TRUE )
    entity
  }
)

setMethod(
  f = "createEntity",
  signature = "Locationable",
  definition = function(entity){
    cfun <- getMethod("createEntity", "LocationableWithoutBinaries")
    ee <- cfun(entity)
    ee@objOwn <- entity@objOwn
    ee
  }
)


#####
## end Create Entity methods
#####


setMethod(
  f = "downloadEntity",
  signature = signature("LocationableWithoutBinaries", "missing"),
  definition = function(entity){
    if(length(entity$properties$id) > 0)
      entity <- downloadEntity(entity, entity$properties$versionNumber)
    entity
  }
)

setMethod(
  f = "downloadEntity",
  signature = signature("LocationableWithoutBinaries", "numeric"),
  definition = function(entity, versionId){
    downloadEntity(entity, as.character(versionId))
  }
)

setMethod(
  f = "downloadEntity",
  signature = signature("LocationableWithoutBinaries", "character"),
  definition = function(entity, versionId){
    if(versionId != as.character(entity$properties$versionNumber))
      entity <- getEntity(entity, versionId)

    if(is.null(propertyValue(entity, "locations")[[1]][['path']])) {
      unfulfilledAccessRequirements<-synapseGet(sprintf("/entity/%s/accessRequirementUnfulfilled", propertyValue(entity, "id")))
      if (unfulfilledAccessRequirements$totalNumberOfResults>0) { # if there is a file but I lack access due to a restriction...
        # ...an error message is displayed which include the 'onweb' command to open the entity page, 
        # where a user can address the access requirements.
        message <- sprintf("Please visit the web page for this entity (onWeb(\"%s\")) to review and fulfill its download requirement(s).",
          propertyValue(entity, "id"))
        stop(message)
      }
      return(entity)
    }

    ## download the file
    url <- entity$properties$locations[[1]][['path']]
    destfile <- .generateCacheDestFile(url, entity$properties$versionNumber)
    if(entity@archOwn@fileCache$archiveFile != basename(destfile)){
      entity@archOwn <- ArchiveOwner(dirname(destfile), basename(destfile))
    } else {
      entity@archOwn <- ArchiveOwner(dirname(destfile))
    }


    ## passing the md5 sum causes this funciton to only download the file
    ## if the cached copy does not match that md5 sum
    archiveFile <- synapseClient:::synapseDownloadFile(url, propertyValue(entity, "md5"), versionId=entity$properties$versionNumber)

    archiveFile <- normalizePath(archiveFile)
    if(entity@archOwn@fileCache$archiveFile != basename(destfile)){
      entity@archOwn <- ArchiveOwner(dirname(destfile), basename(destfile))
    } else {
      entity@archOwn <- ArchiveOwner(dirname(destfile))
    }
    ## unpack the archive
    entity@archOwn <- unpackArchive(entity@archOwn)

    entity
  }
)


setMethod(
  f = "downloadEntity",
  signature = signature("Locationable", "missing"),
  definition = function(entity){
    ## call the superclass method
    dlfun <- getMethod("downloadEntity", signature=signature("LocationableWithoutBinaries", "missing"))
    ee <- dlfun(entity)
    ee@objOwn <- entity@objOwn

    ## now set the archive for the caching object owner to be the same
    ## as the one for the archive owner
    oo <- entity@objOwn
    setFileCache(oo, entity@archOwn@fileCache)
    ee@objOwn <- oo

    ee
  }
)


setMethod(
    f = "deleteEntity",
    signature = "LocationableWithoutBinaries",
    definition = function(entity){
      entity@archOwn@fileCache$delete()
      dfun <- getMethod("deleteEntity", "Entity")
      entity <- dfun(entity)
      invisible(entity)
    }
)


#####
## Manage the files contained in a locationable entity's archive
#####

setMethod(
  f = "addFile",
  signature = signature("LocationableWithoutBinaries", "character", "character"),
  definition = function(entity, file, path){
    entity@archOwn <- addFile(entity@archOwn, file, path)
    invisible(entity)
  }
)

setMethod(
    f = "addFile",
    signature = signature("LocationableWithoutBinaries", "character", "missing"),
    definition = function(entity, file){
      entity@archOwn <- addFile(entity@archOwn, file)
      invisible(entity)
    }
)

setMethod(
    f = "moveFile",
    signature = signature("LocationableWithoutBinaries", "character", "character"),
    definition = function(entity, src, dest){
      entity@archOwn <- moveFile(entity@archOwn, src, dest)
      invisible(entity)
    }
)

setMethod(
    f = "deleteFile",
    signature = signature("LocationableWithoutBinaries", "character"),
    definition = function(entity, file){
      entity@archOwn <- deleteFile(entity@archOwn, file)
      invisible(entity)
    }
)

#####
## end file management methods
#####


#####
## manage archive files containing binary objects
#####

setMethod(
  f = "addObject",
  signature = signature("Locationable", "ANY", "character", "missing"),
  definition = function(owner, object, name){
    owner@objOwn <- addObject(owner@objOwn, object, name)
    invisible(owner)
  }
)

setMethod(
  f = "addObject",
  signature = signature("Locationable", "ANY", "missing", "missing"),
  definition = function(owner, object){
    name = deparse(substitute(object, env=parent.frame()))
    name <- gsub("\\\"", "", name)
    addObject(owner, object, name)
  }
)

setMethod(
  f = "addObject",
  signature = signature("Locationable", "list", "missing", "logical"),
  definition = function(owner, object, unlist){
    if(unlist){
      if(any(names(object) == ""))
        stop("All list elements must be named when unlisting")
      lapply(names(object), function(n){
          owner <<- addObject(owner, object[[n]], n)
        })
    }else{
      owner <- addObject(owner, object)
    }
    invisible(owner)
  }
)

setMethod(
  f = "deleteObject",
  signature = signature("Locationable", "character"),
  definition = function(owner, which){
    owner@objOwn <- deleteObject(owner@objOwn, which)
    invisible(owner)
  }
)

setMethod(
  f = "renameObject",
  signature = signature("Locationable", "character", "character"),
  definition = function(owner, which, name){
    owner@objOwn <- renameObject(owner@objOwn, which, name)
    invisible(owner)
  }
)

setMethod(
  f = "getObject",
  signature = signature("Locationable", "character"),
  definition = function(owner, which){
    getObject(owner@objOwn, which)
  }
)

#####
## end object manaagement methods
#####


setMethod(
  f = "files",
  signature = "LocationableWithoutBinaries",
  definition = function(object){
    files(object@archOwn)
  }
)

setMethod(
  f = "files",
  signature = "Locationable",
  definition = function(object){
    setdiff(files(object@archOwn), files(object@objOwn))
  }
)

setMethod(
  f = "cacheDir",
  signature = "Locationable",
  definition = function(object){
    cacheDir(object@objOwn)
  }
)

objects.Locationable <-
  function(x)
{
  objects(x@objOwn)
}


setMethod(
    f = "[",
    signature = "LocationableWithoutBinaries",
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
            }else if(i %in% names.Entity(x)){
              class(x) <- "Entity"
              x[[i]]
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
  f = "[",
  signature = "Locationable",
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
        switch(i,
          objects = .doGetObjects(x),
          cacheDir = cacheDir(x@archOwn),
          files = files(x),
          fileObjects = x@archOwn@objects,
          binObjects = x@objOwn$objects[],
          if(i %in% names.Entity(x)){
              class(x) <- "Entity"
              return(x[[i]])
            }else{
              return(NULL)
            }
        )
      }
    )
    names(retVal) <- i
    retVal
  }
)

setMethod(
    f = "[[",
    signature = "LocationableWithoutBinaries",
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
    signature = "LocationableWithoutBinaries",
    definition = function(x, name){
      x[[name]]
    }
)

setMethod(
  f = ".doGetObjects",
  signature = "Locationable",
  definition = function(x){
    oo <- union(objects(x@archOwn@objects, all.names=T), objects(x@objOwn$objects, all.names=T))
    retVal <- lapply(oo, function(oName){
        if(oName %in% objects(x@archOwn@objects, all.names=T))
          return(x@archOwn@objects[[oName]])
        return(x@objOwn$objects[[oName]])
      })
    names(retVal) <- oo
    retVal
  }
)


##
## initialize methods for both Locationable classes
##
setMethod(
  f = "initialize",
  signature = "LocationableWithoutBinaries",
  definition = function(.Object){
    .Object@archOwn <- new("ArchiveOwner")
    .Object@archOwn@fileCache <- getFileCache(.Object@archOwn@fileCache$getCacheRoot())
    setPackageName(env=.Object)
    .Object
  }
)

setMethod(
  f = "initialize",
  signature = "Locationable",
  definition = function(.Object){
    ## call super-class initialize method
    ifun <- getMethod("initialize", "LocationableWithoutBinaries")
    .Object <- ifun(.Object)

    .Object@objOwn <- new("CachingObjectOwner")
    .Object@objOwn$objects@fileCache <- .Object@archOwn@fileCache
    .Object
  }
)


names.LocationableWithoutBinaries <-
    function(x)
{
  c(names.Entity(x), "objects", "cacheDir", "files")
}


names.Locationable <-
  function(x)
{
  c(names.LocationableWithoutBinaries(x), "fileObjects", "binObjects")
}


setMethod(
  f = "summarizeObjects",
  signature = "LocationableWithoutBinaries",
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
  f = "summarizeObjects",
  signature = "Locationable",
  definition = function(entity){
    msg <- NULL
    if(length(names(entity$objects)) > 0){
      msg$count <- sprintf("loaded object(s)")
      objects <- names(entity$objects)
      classes <- unlist(lapply(objects, function(object){paste(class(entity$objects[[object]]), collapse=":")}))
      msg$objects <- sprintf('[%d] "%s" (%s)', 1:length(objects), objects, classes)
    }
    msg
  }
)

setMethod(
  f = "summarizeCacheFiles",
  signature = "LocationableWithoutBinaries",
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


objects.Locationable <-
  function(name)
{
  union(objects(name@archOwn@objects, all.names=T), objects(name@objOwn$objects, all.names=T))
}



setMethod(
  f = "loadObjectsFromFiles",
  signature = "LocationableWithoutBinaries",
  definition = function(owner){
    owner@archOwn <- loadObjectsFromFiles(owner@archOwn, clearEnvironment=TRUE)
    invisible(owner)
  }
)

setMethod(
  f = "loadObjectsFromFiles",
  signature = "Locationable",
  definition = function(owner){
    ## call the superclass method
    lfun <- getMethod("loadObjectsFromFiles", signature("LocationableWithoutBinaries"))
    owner <- lfun(owner)
    owner@objOwn <- loadObjectsFromFiles(owner@objOwn, clearEnvironment=TRUE)
    invisible(owner)
  }
)

setReplaceMethod("$",
  signature = "Locationable",
  definition = function(x, name, value) {
     if(name == "objects"){
       stop("not yet supported, use the addObject method for adding objects", call.=FALSE)
    }else if(name %in% names.Locationable(x)){
      slot(x, name) <- value
    }else{
      stop("invalid element")
    }
    x
  }
)

setMethod(
  f = "show",
  signature = "LocationableWithoutBinaries",
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
  f = "setPackageName",
  signature = signature(env = "LocationableWithoutBinaries"),
  definition = function(pkg, env)
  {
    if(missing(pkg))
      pkg <- basename(tempfile(pattern=as.character(class(env))))
    setPackageName(pkg = pkg, env = env@archOwn)
  }
)

setMethod(
  f = "getPackageName",
  signature = signature(where = "LocationableWithoutBinaries"),
  definition = function (where, create = TRUE)
  {
    getPackageName(where = where@archOwn, create = create)
  }
)


setReplaceMethod("$",
  signature = "LocationableWithoutBinaries",
  definition = function(x, name, value) {
     if(!(name %in% names.Entity(x)))
      stop("invalid element")
    fun <- getMethod("$<-", "Entity")
    fun(x, name, value)
  }
)

setMethod(
  f = "setFetchMethod",
  signature = signature("LocationableWithoutBinaries", "character", "FileCacheFactory"),
  definition = function(object, method, factory){
    setFetchMethod(object@archOwn, method, factory)
  }
)


setMethod(
  f = "setFetchMethod",
  signature = signature("LocationableWithoutBinaries", "character", "missing"),
  definition = function(object, method){
    setFetchMethod(object@archOwn, method)
  }
)

setMethod(
  f = "getFetchMethod",
  signature = signature("LocationableWithoutBinaries", "FileCacheFactory"),
  definition = function(object, factory){
    getFetchMethod(object@archOwn, factory)
  }
)

setMethod(
  f = "getFetchMethod",
  signature = signature("LocationableWithoutBinaries", "missing"),
  definition = function(object){
    getFetchMethod(object@archOwn)
  }
)




