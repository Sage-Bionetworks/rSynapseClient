# TODO: Add comment
# 
# Author: furia
###############################################################################

setMethod(
    f = "properties",
    signature = "SimplePropertyOwner",
    definition = function(object){
      object@properties
    }
)

setMethod(
    f = "properties<-",
    signature = "SimplePropertyOwner",
    definition = function(object, value){
      object@properties <- value
      object
    }

)

setMethod(
  f = "propertyNames",
  signature = "SimplePropertyOwner",
  definition = function(object){
    names(object@properties)
  }
)

setMethod(
  f = "propertyValues",
  signature = "SimplePropertyOwner",
  definition = function(object){
    values <- lapply(propertyNames(object@properties), function(n){
        getProperty(object,n)
      }
    )
    unlist(values)
  }
)

#####
## Set multiple property values
#####
setMethod(
		f = "propertyValues<-",
		signature = signature("SynapseEntity", "list"),
		definition = function(object, value){
			if(any(names(value) == "") && length(value) > 0)
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
		signature = signature("SynapseEntity", "character"),
		definition = function(object, which){
			if(!all(which %in% propertyNames(object))){
				indx <- which(!(which %in% propertyNames(object)))
				warning(paste(propertyNames(object)[indx], sep="", collapse=","), "were not found in the object, so were not deleted.")
			}
			object@properties <- object@properties[setdiff(propertyNames(object), which)]
			return(object)
		}
)

#####
## set a property value
#####
setMethod(
		f = "propertyValue<-",
		signature = signature("SynapseEntity", "character"),
		definition = function(object, which, value){
			properties(object)[[which]] <- as.character(value)
			object
		}
)

setMethod(
    f = "propertyValue",
    signature = signature("SynapseEntity", "character"),
    definition = function(object, which){
      properties(object)[[which]]
    }
)


