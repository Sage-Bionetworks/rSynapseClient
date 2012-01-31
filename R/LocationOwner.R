# TODO: Add comment
# 
# Author: furia
###############################################################################

setMethod(
		f = "refreshEntity",
		signature = "LocationOwner",
		definition = function(entity){
			refreshedEntity <- getEntity(entity)
			refreshedEntity@location <- entity@location
			refreshedEntity@objects <- entity@objects
			refreshedEntity
		}
)

setMethod(
		f = "show",
		signature = "LocationOwner",
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
			
			files.msg <- summarizeCacheFiles(object@location)
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
		signature = "LocationOwner",
		definition = function(entity){
			msg <- NULL
			if(length(objects(entity@objects)) > 0){
				msg$count <- sprintf("loaded object(s)")
				objects <- objects(entity@objects)
				classes <- unlist(lapply(objects, function(object){class(entity@objects[[object]])}))
				
				msg$objects <- sprintf('[%d] "%s" (%s)', 1:length(objects), objects, classes)
			}
			msg
		}
)


setMethod(
		f = "[",
		signature = "LocationOwner",
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
							envir <- slot(x,i)
							objects <- lapply(objects(envir), function(key) get(key,envir=envir))
							names(objects) <- objects(envir)
							return(objects)
						}
						slot(x@location, i)
					}
			)
			names(retVal) <- i
			retVal
		}
)

setMethod(
		f = "[[",
		signature = "LocationOwner",
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
		signature = "LocationOwner",
		definition = function(x, name){
			x[[name]]
		}
)

#setMethod(
#		f = "$<-",
#		signature = "LocationOwner",
#		definition = function(x, name, value){
#			cat(name,"\n")
#		}
#)


setMethod(
		f = "names",
		signature = "LocationOwner",
		definition = function(x){
			c("objects", "cacheDir", "files")
		}
)

setMethod(
		f = "initialize",
		signature = "LocationOwner",
		definition = function(.Object, properties=NULL){
			.Object@objects <- new.env(parent=emptyenv())
			if(!is.null(properties))
				.Object@properties <- properties
			.Object@location = new(Class="CachedLocation")
			.Object
		}
)

