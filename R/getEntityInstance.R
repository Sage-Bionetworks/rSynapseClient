# TODO: Add comment
#
# Author: furia
###############################################################################

setMethod(
  f = "getEntityInstance",
  signature = signature("list"),
  definition = function(entity)
  {
    class <- getClassFromSynapseEntityType(entity$entityType)

    ## synapseEntity is the default
    if(is.null(class))
      class <- "Entity"

    if(class == "Entity"){
      if(!is.null(entity$locations) && length(entity$locations) > 0)
        class <- "Locationable"
    }

    ## call the appropriate constructor and pass the list
    ## representation of the entity
    fun <- getMethod(class, signature = "list", where="synapseClient")
    ee <- fun(entity)
    ee@synapseWebUrl <- .buildSynapseUrl(propertyValue(ee, "id"))

    if(inherits(ee, "Locationable")){
      url <- ee$properties$locations[[1]][['path']]
      if(!is.null(url)){
        ## instantiate the ArchiveOwner
        destfile <- .generateCacheDestFile(url, ee$properties$versionNumber)
        cacheRoot <- dirname(destfile)
      }else if(!is.null(ee$properties$id)){
        ## use an entity-specifict temp dir
        cacheRoot <- file.path(tempdir(), ee$properties$id)
      }else{
        ## use a temp dir
        cacheRoot <- tempdir()
      }
      if(is.null(cacheRoot))
        stop("null cache root")

      if(!file.exists(cacheRoot))
        dir.create(cacheRoot, recursive=TRUE)

      cacheRoot <- gsub("[\\/]+", "/", normalizePath(cacheRoot))

      if(cacheRoot %in% synapseClient:::availFileCaches()){
          ee@archOwn@fileCache <- getFileCache(cacheRoot)
      } else{
          setCacheRoot(ee@archOwn, cacheRoot, clean = FALSE)
          lapply(dir(ee$cacheDir), function(f){addFile(ee, file.path(ee$cacheDir,f))})
          setFetchMethod(ee, "get")
      }
    }
    ee
  }
)


setMethod(
  f = "initializeEntity",
  signature = "Entity",
  definition = function(entity){
    entity
  }
)

setMethod(
  f = "initializeEntity",
  signature = "LocationableWithoutBinaries",
  definition = function(entity){
    ifun <- getMethod("initializeEntity", "Entity")
    entity <- ifun(entity)

    ## get the cache url for this entity
    url <- entity$properties$locations[[1]][['path']]
    if(is.null(url))
      return(entity)
    destdir <- .generateCacheDestDir(url, entity$properties$versionNumber)
    if(!file.exists(destdir))
      dir.create(destdir, recursive=T)
    destdir <- normalizePath(path.expand(destdir))

    ## instantiate the file cache an put the reference in
    ## the archOwner
    fc <- getFileCache(destdir)
    entity@archOwn@fileCache <- fc
    entity
  }
)

setMethod(
  f = "initializeEntity",
  signature = "Locationable",
  definition = function(entity){
    ifun <- getMethod("initializeEntity", "LocationableWithoutBinaries")
    entity <- ifun(entity)

    ## instantiate the file cache an put the reference in
    ## the archOwner
    entity@objOwn$fileCache <- entity@archOwn@fileCache
    entity
  }
)
