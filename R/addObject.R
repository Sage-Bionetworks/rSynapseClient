## Add objects to an entity
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

setMethod(
  f = "addObject",
  signature = signature("Layer", "ANY", "missing", "missing"),
  definition = function(entity, object){
    name = deparse(substitute(object, env=parent.frame()))
    name <- gsub("\\\"", "", name)
    addObject(entity, object, name)
  }
)

setMethod(
  f = "addObject",
  signature = signature("Layer", "ANY", "character", "missing"),
  definition = function(entity, object, name){
    entity@location <- addObject(entity@location, object, name)
    invisible(entity)
  }
)

setMethod(
  f = "addObject",
  signature = signature("CachedLocation", "ANY", "character", "missing"),
  definition = function(entity, object, name){
    assign(name, object, envir = entity@objects)
    tryCatch(
      .cacheObject(entity, name),
      error = function(e){
        deleteObject(entity, name)
        stop(e)
      }
    )
    invisible(entity)
  }
)

setMethod(
  f = "addObject",
  signature = signature("Layer", "list", "character", "missing"),
  definition = function(entity, object, name){
    entity@location <- addObject(entity@location, object, name)
    invisible(entity)
  }
)

setMethod(
  f = "addObject",
  signature = signature("CachedLocation", "list", "character", "missing"),
  definition = function(entity, object, name){
    assign(name, object, envir = entity@objects)
    tryCatch(
      .cacheObject(entity, name),
      error = function(e){
        deleteObject(entity, name)
        stop(e)
      }
    )
    invisible(entity)
  }
)

setMethod(
  f = "addObject",
  signature = signature("Layer", "list", "missing", "missing"),
  definition = function(entity, object){
    entity@location <- addObject(entity@location, object)
    invisible(entity)
  }
)

setMethod(
  f = "addObject",
  signature = signature("CachedLocation", "list", "missing", "missing"),
  definition = function(entity, object){
    if(any(names(object) == "") || is.null(names(object)))
      stop("all elements of the list must be named")
    lapply(names(object), FUN=function(nm){
        addObject(entity, object[[nm]], nm)
      }
    )
    invisible(entity)
  }
)

setMethod(
  f = "addObject",
  signature = signature("Layer", "list", "missing", "logical"),
  definition = function(entity, object, unlist){
    if(unlist){
      entity@location <- addObject(entity@location, object)
      return(entity)
    }
    name = deparse(substitute(object, env=parent.frame()))
    name <- gsub("\\\"", "", name)
    entity@location <- addObject(entity@location, object, name, unlist)
    invisible(entity)
  }
)

setMethod(
  f = "addObject",
  signature = signature("CachedLocation", "list", "missing", "logical"),
  definition = function(entity, object, unlist){
    if(unlist)
      return(addObject(entity, object))
    name = deparse(substitute(object, env=parent.frame()))
    name <- gsub("\\\"", "", name)
    addObject(entity, object, name, unlist)
  }
)

setMethod(
  f = "addObject",
  signature = signature("Layer", "list", "character", "logical"),
  definition = function(entity, object, name, unlist){
    entity@location <- addObject(entity@location, object, name, unlist)	
    invisible(entity)
  }
)

setMethod(
  f = "addObject",
  signature = signature("CachedLocation", "list", "character", "logical"),
  definition = function(entity, object, name, unlist){
    if(unlist)
      stop("cannot specify the object name when unlisting")
    assign(name, object, envir = entity@objects)
    tryCatch(
      .cacheObject(entity, name),
      error = function(e){
        deleteObject(entity, name)
        stop(e)
      }
    )
    invisible(entity)
  }
)

setMethod(
  f = "addObject",
  signature = signature("Layer", "data.frame", "missing", "missing"),
  definition = function(entity, object){
    name = deparse(substitute(object, env=parent.frame()))
    name <- gsub("\\\"", "", name)
    entity@location <- addObject(entity@location, object, name)
    invisible(entity)
  }
)

setMethod(
  f = "addObject",
  signature = signature("Layer", "data.frame", "character", "missing"),
  definition = function(entity, object, name){
    entity@location <- addObject(entity@location, object, name)
    invisible(entity)
  }
)

setMethod(
  f = "addObject",
  signature = signature("CachedLocation", "data.frame", "character", "missing"),
  definition = function(entity, object, name){
    assign(name, object, envir = entity@objects)
    tryCatch(
      .cacheObject(entity, name),
      error = function(e){
        deleteObject(entity, name)
        stop(e)
      }
    )
    invisible(entity)
  }
)

setMethod(
  f = "addObject",
  signature = signature("ReadOnlyCachedLocation", "ANY", "ANY", "ANY"),
  definition = function(entity, object, name, unlist){
    stop("Unable to add object. this entity is read-only.")
  }
)
