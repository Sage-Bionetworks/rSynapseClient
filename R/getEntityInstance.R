#
#
# Author: furia
###############################################################################

getFactoryForConcreteType<-function(concreteType) {
  factoryName<-NULL
  if (concreteType=="org.sagebionetworks.repo.model.FileEntity") factoryName<-"createFileFromProperties"
  if (concreteType=="org.sagebionetworks.repo.model.table.TableEntity") factoryName<-"createTableSchemaFromProperties"
  # by default the factory is a class constructor
  if (is.null(factoryName)) factoryName<-getClassNameFromSchemaName(concreteType)
  getMethod(factoryName, signature = "list", where="synapseClient")
}

setMethod(
  f = "getEntityInstance",
  signature = signature("list"),
  definition = function(entity)
  {
    if (is.null(entity$concreteType)) {
      if (!is.null(entity$locations) && length(entity$locations) > 0) {
        factory <- getMethod("Locationable", signature = "list", where="synapseClient")
      } else {
        stop("Entity metadata is missing 'concreteType'.");
      }
    } else {
      factory <- getFactoryForConcreteType(entity$concreteType)
    }
    
    ## call the appropriate constructor and pass the list
    ## representation of the entity
    ee <- factory(entity)
    ee@synapseWebUrl <- .buildSynapseUrl(propertyValue(ee, "id"))

    if(inherits(ee, "Locationable")) {
      location <- ee$properties$locations[[1]][['path']]
      if(!is.null(location)){
        ## instantiate the ArchiveOwner
        destfile <- .generateCacheDestFile(location, ee$properties$versionNumber)
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

      if(cacheRoot %in% availFileCaches()){
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

