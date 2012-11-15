setMethod(
	f = "SynapseProperties",
	signature = "list",
	definition = function(typeMap){
		if(any(names(typeMap) == ""))
			stop("all properties must be named")
		if(any(table(names(typeMap)) > 1L))
			stop("each name must appear only once")
		if(!all(typeMap %in% TYPEMAP))
			stop("invalid data type specified")

		object <- new("SynapseProperties")
		object@typeMap <- typeMap
		object
	}
)

setMethod(
	f = "propertyNames",
	signature = "SynapseProperties",
	definition = function(object){
		names(object@typeMap)
	}
)

setMethod(
	f = "propertyValues",
	signature = "SynapseProperties",
	definition = function(object){
		propertyValues(object@properties)
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
	signature = "SynapseProperties",
	definition = function(object, which, value){
		if(!all(which %in% names(object@typeMap)))
			stop("invalid ptoperty specified")

		## coerce the value to the correct type
		type <- object@typeMap[[which]]
		value <- do.call(sprintf("as.%s", type), list(value))

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
    retVal <- lapply(i, function(i){
        propertyValue(x, i)
      }
    )
    names(retVal) <- i
    retVal
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
  	if(!all(i %in% names(x@typeMap)))
			stop("invalid ptoperty specified")
    propertyValue(x, i) <- value
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
	ll <- lapply(propertyNames(x), function(n) propertyValue(x, n))
	names(ll) <- propertyNames(x)
	ll
}

names.SynapseProperties <-
	function(x)
{
	propertyNames(x)
}

setMethod(
	f = "show",
	signature = "SynapseProperties",
	definition = function(object){
		show(as.list(object))
	}
)
