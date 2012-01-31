# TODO: Add comment
# 
# Author: Matt Furia
###############################################################################

setGeneric(
		name = "storeEntity",
		def = function(entity){
			standardGeneric("storeEntity")
		}
)

setMethod(
		f = "storeEntity",
		signature = "SynapseEntity",
		definition = function(entity){
			if(is.null(propertyValue(entity, "id"))){
				entity <- createEntity(entity)
			}else{
				entity <- updateEntity(entity)
			}
			entity
		}
)

setMethod(
		f = "storeEntity",
		signature = "Layer",
		definition = function(entity){
			if(length(list.files(file.path(entity$cacheDir, .getCache("rObjCacheDir")), all.files = TRUE)) > 0 ){
				storeEntityObjects(entity)
			} else if((length(entity$files) == 1) 
					&& (tolower(class(entity)) != "code") 
					&& (("M" == propertyValue(entity, "type")) || grepl(".*\\.zip$", entity$files[1], perl=TRUE))) {
				# Special case for media layers, don't zip them
				# Special case for zip files, they are already zipped so don't zip them
				storeFile(entity, entity$files)
			}
			else {
				storeEntityFiles(entity)
			}
		}
)

setMethod(
		f = "storeEntity",
		signature = "Code",
		definition = function(entity){
			if((length(entity$files) == 1) 
					|| grepl(".*\\.zip$", entity$files[1], perl=TRUE)) {
				# Special case for single code file, don't zip it
				# Special case for zip files, they are already zipped so don't zip them
				storeFile(entity, entity$files)
			}
			else {
				storeEntityFiles(entity)
			}
		}
)
