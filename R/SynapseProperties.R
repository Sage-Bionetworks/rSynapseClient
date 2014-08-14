##
## NOTE: SynapseProperties constructor that would normally be defined here is instead defined
## in AllClasses.R
##

setMethod(
  f = "deleteProperty",
  signature = signature("SynapseProperties", "character"),
  definition = function(object, which){
    object@properties <- deleteProperty(object@properties, which)
    object
  }
)

setMethod(
  f = "SynapseProperties",
  signature = "missing",
  definition = function(){
    new("SynapseProperties")
  }
)

setMethod(
  f = "properties",
  signature = "SynapseProperties",
  definition = function(object){
    ## include all valid properties, including the
    ## NULL equivilant for ones that haven't been set
    val <- lapply(propertyNames(object), function(name){
        propertyValue(object, name)
      }
    )
    names(val) <- propertyNames(object)
    val
  }
)

setMethod(
	f = "propertyNames",
	signature = "SynapseProperties",
	definition = function(object){
    ## return the names of all valid properties
    if(is.null(object@typeMap))
      return(propertyNames(object@properties))
		names(object@typeMap)
	}
)

setMethod(
	f = "propertyValues",
	signature = "SynapseProperties",
	definition = function(object){
		unlist(lapply(propertyNames(object), function(name) propertyValue(object, name)))
	}
)

setMethod(
	f = "propertyValue",
	signature = "SynapseProperties",
	definition = function(object, which){
		getProperty(object@properties, which)
	}
)

setMethod(
	f = "propertyValue<-",
	signature = signature("SynapseProperties", "character"),
	definition = function(object, which, value){
		if(!is.null(object@typeMap) & !all(which %in% names(object@typeMap)))
			stop("invalid property specified")

		## coerce the value to the correct type
    if(!is.null(object@typeMap) & !is.null(value)){
		  type <- object@typeMap[[which]]
		  value <- do.call(sprintf("as.%s", type), list(value))
    }

		object@properties <- setProperty(object@properties, which, value)
		object
	}
)

setMethod(
  f = "propertyValue<-",
  signature = signature("SynapseProperties", "character", "list"),
  definition = function(object, which, value){
    if(!is.null(object@typeMap) & !all(which %in% names(object@typeMap)))
      stop("invalid property specified")

    object@properties <- setProperty(object@properties, which, value)
    object
  }
)

setMethod(
  f = "[",
  signature = "SynapseProperties",
  definition = function(x, i, j, ...){
    if(length(as.character(as.list(substitute(list(...)))[-1L])) > 0L || !missing(j))
      stop("incorrect number of subscripts")
    if(is.numeric(i)){
      if(any(i > length(propertyNames(x))))
        stop("subscript out of bounds")
      i <- propertyNames(x)[i]
    }
    properties(x)[i]
  }
)

setMethod(
  f = "[[",
  signature = "SynapseProperties",
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
  signature = "SynapseProperties",
  definition = function(x, name){
    x[[name]]
  }
)

setReplaceMethod(
  f = "[[", 
  signature = signature(
    x = "SynapseProperties",
    i = "character"
  ),
  definition = function(x, i, value) {
    if(length(i) > 1)
      stop("subscript out of bounds")
  	if(!is.null(x@typeMap) & !all(i %in% names(x@typeMap)))
			stop("invalid property specified")
    propertyValue(x, i) <- value
    x
  }
)

setReplaceMethod(
  f = "[", 
  signature = signature(
    x = "SynapseProperties",
    i = "character"
  ),
  definition = function(x, i, value) {
    if(length(i) < length(value))
      stop("more elements supplied than there are to replace")
    if(length(i) %% length(value) != 0L)
      stop("number of items to replace is not a multiple of replacement length")

    if(!is.null(x@typeMap) & !all(i %in% names(x@typeMap)))
      stop("invalid property specified")

    if(length(i) != length(value))
      value <- rep(value, length(i)/length(value))

    lapply(1:length(i), function(indx) propertyValue(x, i[indx]) <<- value[indx])
    x
  }
)

setReplaceMethod(
  f = "$", 
  signature = "SynapseProperties",
  definition = function(x, name, value) {
    x[[name]] <- value
    x
  }
)

as.list.SynapseProperties <-
	function(x, ...)
{
  ## This method should only return property values
  ## that are actually set, which is the behavior of
  ## the TypedPropertyStore class of which SynapseProperties
  ## is composed. This is in contrast to 
  ## the properties() function and show method that
  ## display all valid properties, even if they're not
  ## set. The reason for this behavior is that as.list
  ## is used when converting the SynapseProperties
  ## object into JSON. In that case, only set properties
  ## should be included
	val <- lapply(propertyNames(x@properties), function(n) propertyValue(x, n))
	names(val) <- propertyNames(x@properties)
	val
}

names.SynapseProperties <-
	function(x)
{
  ## return the names of all valid
  ## properties
	propertyNames(x)
}

setMethod(
	f = "show",
	signature = "SynapseProperties",
	definition = function(object){
		show(properties(object))
	}
)

setMethod("identical",
  signature=signature("SynapseProperties", "SynapseProperties"),
  definition = function(x, y, num.eq=TRUE, single.NA = TRUE, attrib.as.set = TRUE,
    ignore.bytecode = TRUE) {
    
    slotNames<-slotNames(x)
    if (!identical(slotNames, slotNames(y), single.NA, attrib.as.set, ignore.bytecode)) return(FALSE)
    for (name in slotNames) {
      if(!identical(slot(x, name), slot(y, name), single.NA, 
          attrib.as.set, ignore.bytecode)) return(FALSE)
    }
    TRUE
  }
)


