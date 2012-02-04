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

setMethod(
  f = "deleteObject",
  signature = signature("GithubCode", "ANY"),
  definition = function(entity, which){
    stop("Cannot delete an object for class GithubCode - already synced with a github tag.")
  }
)
