# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
    f = "[",
    signature = "EnhancedEnvironment",
    definition = function(x, i, j, ...){
      if(length(as.character(as.list(substitute(list(...)))[-1L])) > 0L || !missing(j))
        stop("incorrect number of subscripts")
      if(missing(i))
        return(x[names(x)])
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
            get(i, envir = x@env)
          }
      )
      names(retVal) <- i
      retVal
    }
)

setMethod(
    f = "[[",
    signature = "EnhancedEnvironment",
    definition = function(x, i, j, ...){
      if(length(as.character(as.list(substitute(list(...)))[-1L])) > 0L || !missing(j))
        stop("incorrect number of subscripts")
      if(length(i) > 1)
        stop("subscript out of bounds")
      x[i][[1]]
    }
)

setReplaceMethod("[[", 
    signature = signature(
        x = "EnhancedEnvironment",
        i = "character"
    )
    ,
    function(x, i, value) {
      assign(i, value, envir = x@env)
      x
    }
)

setMethod(
    f = "initialize",
    signature = "EnhancedEnvironment",
    definition = function(.Object){
      .Object@env = new.env()
      .Object
    }
)

setMethod(
    f = "$",
    signature = "EnhancedEnvironment",
    definition = function(x, name){
      x[[name]]
    }
)

setReplaceMethod("$", 
    signature = "EnhancedEnvironment",
    function(x, name, value) {
      x[[name]] <- value
      x
    }
)

setMethod(
    f = "as.environment",
    signature = "EnhancedEnvironment",
    definition = function(x){
      x@env
    }
)

setMethod(
    f = "show",
    signature = "EnhancedEnvironment",
    definition = function(object){
      if(length(object) == 0){
        show(objects(as.environment(object)))
      }else{
        
        for(i in 1:length(object)){
          cat(sprintf("[%i] %s (%s)\n", i, names(object[i]), class(object[[i]])))
        }
      }
    }
)

setMethod(
    f = "length",
    signature = "EnhancedEnvironment",
    definition = function(x){
      length(names(x))
    }
)

names.EnhancedEnvironment <-
    function(x)
{
  objects(x@env, all.names = TRUE)
}



