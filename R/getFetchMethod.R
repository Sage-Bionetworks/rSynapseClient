###
## Retrieve a character string indicating which fetch method (getEnitty or loadEntity)
## was used to retrieve the entity tied to the given fileCache object
## this is important when calling storeEntity. If getEntity was used, updateEntity should be called
## if loadEntity was used, storeEntity should upload the files associated with the fileCache
###
setMethod(
  f = "getFetchMethod",
  signature = signature("character", "missing"),
  definition = function(object){
  	factory<- new("FileCacheFactory")
    getFetchMethod(object, factory)
  }
)
