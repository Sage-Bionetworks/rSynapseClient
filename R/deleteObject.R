# delete object from LocationOwner entity
# 
# Author: Matt Furia
###############################################################################

setGeneric(
		name = "deleteObject",
		def = function(entity, which){
			standardGeneric("deleteObject")
		}
)

setMethod(
                f = "deleteObject",
                signature = signature("LocationOwner", "character"),
                definition = function(entity, which){
                        entity@location <- deleteObject(entity@location, which)
			invisible(entity)
                }
)

setMethod(
		f = "deleteObject",
		signature = signature("CachedLocation", "character"),
		definition = function(entity, which){
			rm(list=which, envir=entity@objects)
			tryCatch(
					.deleteCacheFile(entity, which),
					error = function(e){
						warning(sprintf("Unable to delete cache file associated with %s\n%s", which, e))
					},
					warning = function(e){
						warning(sprintf("Unable to delete cache file associated with %s\n%s", which, e))
					}
			)
			invisible(entity)
		}
)
