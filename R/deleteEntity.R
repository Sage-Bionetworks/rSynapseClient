setMethod(
        f = "deleteEntity",
        signature = "SynapseEntity",
        definition = function(entity){
                envir <- parent.frame(2)
                inherits <- FALSE
                name <- deparse(substitute(entity, env=parent.frame()))
                synapseClient:::synapseDelete(uri=propertyValue(entity, "uri"))
                if(any(grepl(name,ls(envir=envir))))
			remove(list = name, envir=envir, inherits=inherits)
		entity <- deleteProperty(entity, "id")
                entity <- deleteProperty(entity, "accessControlList")
		entity <- deleteProperty(entity, "uri")
		entity <- deleteProperty(entity, "annotations")
		entity <- deleteProperty(entity, "etag")
		invisible(entity)
        }
)

setMethod(
	f = "deleteEntity",
	signature = "LocationOwner",
	definition = function(entity){
		unlink(entity$cacheDir, recursive = TRUE, force = TRUE)
		oldClass <- class(entity)
		class(entity) <- "SynapseEntity"
		entity <- deleteEntity(entity)
		class(entity) <- oldClass
		invisible(entity)	
	}
)

setMethod(
		f = "deleteEntity",
		signature = "numeric",
		definition = function(entity){
			deleteEntity(as.character(entity))
		}
)

setMethod(
		f = "deleteEntity",
		signature = "character",
		definition = function(entity){
			entity <- getEntity(entity)
			deleteEntity(entity)
		}
)

