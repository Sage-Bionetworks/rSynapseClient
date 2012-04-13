# TODO: Add comment
# 
# Author: furia
###############################################################################

setMethod(
  f = "[",
  signature = "ReadOnlyObjectOwner",
  definition = function(x, i, j, ...){
    if(missing(i) || is.null(i))
      i <- names(x) 
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
        x$field(i)
      }
    )
    names(retVal) <- i
    retVal
  }
)

setMethod(
  f = "[[",
  signature = "ReadOnlyObjectOwner",
  definition = function(x, i, j, ...){
    if(length(as.character(as.list(substitute(list(...)))[-1L])) > 0L || !missing(j))
      stop("incorrect number of subscripts")
    if(length(i) > 1)
      stop("subscript out of bounds")
    x[i][[1]]
  }
)

setMethod(
  f = "names",
  signature = "ReadOnlyObjectOwner",
  definition = function(x){
    names(x$.refClassDef@fieldClasses)
  }
)