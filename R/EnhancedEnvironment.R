# TODO: Add comment
# 
# Author: furia
###############################################################################

##
## Initialize the EnhancedEnvironment by creating a new environment
##
setMethod(
  f = "initialize",
  signature = "EnhancedEnvironment",
  definition = function(.Object){
    .Object@env = new.env()
    setPackageName.EnhancedEnvironment(env = .Object)
    .Object
  }
)


##
## Allows caller to assign access elements in the environment using a bracket accessor.
## works for objects starting with a period as well.
##
setMethod(
    f = "[",
    signature = "EnhancedEnvironment",
    definition = function(x, i, j, ...){
      if(length(as.character(as.list(substitute(list(...)))[-1L])) > 0L || !missing(j))
        stop("incorrect number of subscripts")
      if(missing(i))
        return(x[names(x)])
      if(is.numeric(i)){
        i <- names(x)[i]
      }else if (is.character(i)){
        i <- names(x)[match(i, names(x))]
      }else{
        stop(sprintf("invalid subscript type '%s'", class(i)))
      }
      retVal <- lapply(i, function(i){
            if(is.na(i)) return(NULL)
            get(i, envir = as.environment(x))
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
      if(missing(i))
        stop("invalid subscript type 'symbol'")
      if(is.numeric(i)){
        if(length(i) > 1L)
          stop("attempt to select less than one element")
        if(i == 0L)
          stop("attempt to select less than one element")
        if(i > length(names(x)))
          stop("subscript out of bounds")
        i <- (1:length(names(x)))[i]
      }
      if(length(i) > 1)
        stop("attempt to select more than one element")

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

##
## dollar sign accessor for retrieving a single named object
##
setMethod(
  f = "$",
  signature = "EnhancedEnvironment",
  definition = function(x, name){
    x[[name]]
  }
)

##
## dollar sign accessor's replacement method for adding a single named object
##
setReplaceMethod("$", 
  signature = "EnhancedEnvironment",
  function(x, name, value) {
    x[[name]] <- value
    x
  }
)

##
## coerce to environment by returning the enclosed environment class
##
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
          cat(sprintf("[%i] %s (%s)\n", i, names(object[i]), paste(class(object[[i]]), collapse=",")))
        }
      }
    }
)

##
## Delete the object(s) from the environment
##
setMethod(
  f = "deleteObject",
  signature = signature("EnhancedEnvironment", "character"),
  definition = function(owner, which){
    rm(list=which, envir=as.environment(owner))
    invisible(owner)
  }
)


setMethod(
  f = "addObject",
  signature = signature("EnhancedEnvironment", "ANY", "missing", "missing"),
  definition = function(owner, object){
    name = deparse(substitute(object, env=parent.frame()))
    name <- gsub("\\\"", "", name)
    owner[[name]] <- object
    invisible(owner)
  }
)

setMethod(
  f = "addObject",
  signature = signature("EnhancedEnvironment", "list", "missing", "logical"),
  definition = function(owner, object, unlist){
    if(!unlist){
      name = deparse(substitute(object, env=parent.frame()))
      name <- gsub("\\\"", "", name)
      owner[[name]] <- object
      return(owner)
    }
    if(any(names(object) == ""))
      stop("All elements of the list must have names when unlisting")
    lapply(names(object), function(n){
          owner <<- addObject(owner, object[[n]], n)
        })
    invisible(owner)
  }
)

##
## Get object(s) from the environment
##
setMethod(
  f = "getObject",
  signature = signature("EnhancedEnvironment", "character"),
  definition = function(owner, which){
    nms <- names(owner)
    indx <- which(which %in% nms)
    if(length(indx) == 0)
      return(NULL)
    objs <- lapply(which[indx], function(n) owner[[n]])
    
    if(length(objs) > 1)
      return(unlist(objs))
    objs[[1]]
  }
)

##
## rename object(s) in the environment
##
setMethod(
  f = "renameObject",
  signature = signature("EnhancedEnvironment", "character", "character"),
  definition = function(owner, which, name){
    if(length(which) != length(name))
      stop("Must supply the same number of names as objects")
    
    if(!all(which %in% names(owner)))
      stop("Invalid objects provided")
    
    ## make a copy of the objects that will be moved and delete them from
    ## the entity
    ## TODO : make this more performant by only making copies of objects
    ## when absolutely necessary
    tmpEnv <- new.env()
    lapply(which, FUN = function(key){
        assign(key, getObject(owner, key), envir = tmpEnv)
        owner <- deleteObject(owner, key)
      }
    )
    
    lapply(1:length(which), FUN=function(i){
        owner <- addObject(owner, get(which[i], envir=tmpEnv), name[i])
      }
    )
    rm(tmpEnv)
    invisible(owner)
  }
)

##
## Return the names of the objects held in the environment, including names
## starting with a period
##
names.EnhancedEnvironment <-
    function(x)
{
  objects(x, all.names=TRUE)
}

##
## List the objects held in the environment. By default, this excluded objects
## starting with a period
##
objects.EnhancedEnvironment <-
  function(name, all.names = FALSE, pattern)
{
  objects(envir = as.environment(name), all.names = all.names, pattern = pattern) 
} 

##
## Coerce EnhancedEnvironment to an "environment"
##
as.environment.EnhancedEnvironment <-
  function(x)
{
  x@env  
}

##
## Return a count of the objects in the environment, including ones starting with
## a period.
##
length.EnhancedEnvironment <-
  function(x)
{
  length(names(x))
}

##
## Attach the enhanced environment to the search path
##
setMethod(
    f = "attach",
    signature = signature(what = "EnhancedEnvironment"),
    definition = function (what, pos = 2, name = getPackageName(what), warn.conflicts = TRUE)
    {
      attach(as.environment(what), pos = pos, name = name, warn.conflicts = warn.conflicts)
    }
)

##
## Detach the enhanced environment from the search path
##
setMethod(
  f = "detach",
  signature = signature(name = "EnhancedEnvironment"),
  definition = function (name){
    detach(name=getPackageName(name), character.only = TRUE)
  }
)

##
## Function for setting the package name of an EnhancedEnvironment
##
setMethod(
  f = "setPackageName"    
)
setPackageName.EnhancedEnvironment <-
    function(pkg = basename(tempfile(pattern=as.character(class(env)))), env)
{
  setPackageName(pkg=pkg, env=as.environment(env))
}

##
## Function for getting the package name of an EnhancedEnvironment
##
getPackageName.EnhancedEnvironment <-
    function (where, create = TRUE)
{
  getPackageName(where = as.environment(where), create = create)
}



