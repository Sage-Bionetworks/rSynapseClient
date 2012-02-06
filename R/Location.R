kPropertiesSlotName = "properties"

setMethod(
		f = "Location",
		signature = "list",
		definition = function(entity){
			location <- new("Location", properties = entity)		
		}
)

setMethod(
		f = "CachedLocation",
		signature = signature("Location", "character"),
		definition = function(location, files){
			class(location) <- "CachedLocation"
			location@objects <- new.env()
			location@cacheDir <- attr(files, "rootDir")
			files <- gsub(attr(files,"rootDir"), "", as.character(files), fixed=TRUE)
			## Don't populate files from the .R_OBJECTS dir
			regexp <- gsub("[\\.]", "\\\\.", sprintf("^/?%s", .getCache("rObjCacheDir")))
			indx <- grep(regexp, files)
			if(length(indx) != 0)
				files <- files[-indx]
			location@files <- character()
			if(length(files) != 0)
				location@files <- gsub("^[\\\\/]+","", files)
			location
		}
)

setMethod(
		f = "show",
		signature = "CachedLocation",
		definition = function(object){
			cat('An object of class "', class(object), '"\n', sep="")
			
			msg <- summarizeCacheFiles(object)
			if(!is.null(msg)){
				cat("\n", msg$count, " :\n", sep="")
				cat(msg$files, sep="\n")
			}
		}
)

setMethod(
		f = "summarizeCacheFiles",
		signature = "CachedLocation",
		definition = function(entity){
			## if Cached Files exist, print them out
			msg <- NULL
			if(length(entity@cacheDir) != 0){
				msg$count <- sprintf('%d File(s) cached in "%s"', length(entity@files), entity@cacheDir)
				if(length(entity@files) > 0)
					msg$files <- sprintf('[%d] "%s"',1:length(entity@files), entity@files)
			}
			msg
		}
)

setMethod(
		f = "initialize",
		signature = "CachedLocation",
		definition = function(.Object, ...){
			.Object@cacheDir <- tempfile(pattern="cacheDir")
			.Object@cacheDir <- gsub("[\\]+", "/", .Object@cacheDir)
			.Object@objects <- new.env()
			if(!file.exists(.Object@cacheDir))
				dir.create(.Object@cacheDir, recursive=TRUE)
			.Object
		}
)

setMethod(
                f = "initialize",
                signature = "ReadOnlyCachedLocation",
                definition = function(.Object, ...){
                        .Object@cacheDir <- tempfile(pattern="cacheDir")
                        .Object@cacheDir <- gsub("[\\]+", "/", .Object@cacheDir)
                        .Object@objects <- new.env()
                        if(!file.exists(.Object@cacheDir))
                                dir.create(.Object@cacheDir, recursive=TRUE)
                        .Object
                }
)

#####
## get the list of properties for the object
#####
setMethod(
                f = "properties",
                signature = signature("Location"),
                definition = function(object){
                        slot(object, kPropertiesSlotName)
                }
)

#####
## set the properties list for the object
#####
setMethod(
                f = "properties<-",
                signature = signature("Location", "list"),
                definition = function(object, value){
                        object@properties <- value
                        return(object)
                }
)

#####
## Get the property names
#####
setMethod(
                f = "propertyNames",
                signature = signature("Location"),
                definition = function(object){
                        return(names(slot(object, kPropertyFieldName)))
                }
)

#####
## Get a property value by name
#####
setMethod(
                f = "propertyValue",
                signature = signature("Location", "character"),
                definition = function(object, which){
                        properties(object)[[which]]
                }
)


#####
## set a property value
#####
setMethod(
                f = "propertyValue<-",
                signature = signature("Location", "character"),
                definition = function(object, which, value){
                        properties(object)[[which]] <- value
                        object
                }
)

#####
## Get the property values
#####
setMethod(
                f = "propertyValues",
                signature = signature("Location"),
                definition = function(object){
                        lapply(propertyNames(object),FUN=propertyValue, object=object)
                }
)

#####
## Set multiple property values
#####
setMethod(
                f = "propertyValues<-",
                signature = signature("Location", "list"),
                definition = function(object, value){
                        if(any(names(value) == ""))
                                stop("All entity members must be named")
                        for(name in names(value))
                                propertyValue(object, name) <- value[[name]]
                        return(object)
                }
)

#####
## Delete a property
#####
setMethod(
                f = "deleteProperty",
                signature = signature("Location", "character"),
                definition = function(object, which){
                        if(!all(which %in% propertyNames(object))){
                                indx <- which(!(which %in% propertyNames(object)))
                                warning(paste(propertyNames(object)[indx], sep="", collapse=","), "were not found in the object, so were not deleted.")
                        }
			object@properties <- object@properties[setdiff(propertyNames(object), which)]
                        return(object)
                }
)


