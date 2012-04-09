## get an entity from Synapse and download it's files
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

setMethod(
  f = "downloadEntity",
  signature = "LocationOwner",
  definition = function(entity){
    ## check whether user has signed agreement
    if(!hasSignedEula(entity)){
      if(!.promptSignEula())
        stop(sprintf("Visit https://synapse.sagebase.org to sign the EULA for entity %s", propertyValue(entity, "id")))
      if(!.promptEulaAgreement(entity))
        stop("You must sign the EULA to download this dataset. Visit http://synapse.sagebase.org for more information.")
      .signEula(entity)
    }
    entity@location <- .cacheEntity(entity)
    entity
  }
)

setMethod(
  f = "downloadEntity",
  signature = "Code",
  definition = function(entity){
    entity@location <- .cacheEntity(entity)
    entity
  }
)

setMethod(
  f = "downloadEntity",
  signature = "GithubCode",
  definition = function(entity){
    loc <- .cacheEntity(entity)
    class(loc) <- "ReadOnlyCachedLocation"
    entity@location <- loc
    entity
  }
)

setMethod(
  f = "downloadEntity",
  signature = "SynapseEntity",
  definition = function(entity){
    getEntity(entity)
  }
)
setMethod(
  f = "downloadEntity",
  signature = "character",
  definition = function(entity){
    downloadEntity(getEntity(entity))
  }
)
setMethod(
  f = "downloadEntity",
  signature = "numeric",
  definition = function(entity){
    downloadEntity(as.character(entity))
  }
)
setMethod(
  f = "downloadEntity",
  signature = "list",
  definition = function(entity){
    downloadEntity(getEntity(entity))
  }
)

setMethod(
  f = ".cacheEntity",
  signature = "LocationOwner",
  definition = function(entity) {
    
    ## Get the download locations for this entity
    locations <- propertyValue(entity, "locations")
    if (is.null(locations)) {
      entity <- getEntity(propertyValue(entity, "id"))
      locations <- propertyValue(entity, "locations")
      if (is.null(locations)) 
        return(new("CachedLocation"))
      
    }
    
    ## Note that we just use the first location, to future-proof this we would use the location preferred
    ## by the user, but we're gonna redo this in java so no point in implementing that here right now
    destfile = synapseDownloadFile(url = locations[[1]]['path'], checksum = propertyValue(entity, "md5"))
    
    ## Locations are no longer entities in synapse, but they still exist here in the R client
    location <- Location(list(path=locations[[1]]['path'], type=locations[[1]]['type']))
    
    location <- CachedLocation(location, .unpack(filename = destfile))
    
    ## preserved the objects environment of the original
    location@objects <- entity@location@objects
    location
  }
)
