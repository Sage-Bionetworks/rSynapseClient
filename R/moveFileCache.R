setMethod(
  f = "moveFileCache",
  signature = signature("character", "character", "FileCacheFactory"),
  definition = function(from, to, factory){
    from <- gsub("[\\/]+", "/", normalizePath(from, mustWork=FALSE))
    to <- gsub("[\\/]+", "/", normalizePath(to, mustWork=FALSE))
    method <- getFetchMethod(from, factory)
    if(to %in% setdiff(availFileCaches(factory=factory), availFileCaches(method, factory)))
      stop("could not move file cache")
    assign(to, get(from, envir=slot(factory, method)) ,envir=slot(factory, method))
    rm(list=from, envir=slot(factory, method))
  }
)

setMethod(
  f = "moveFileCache",
  signature = signature("character", "character", "missing"),
  definition = function(from, to, factory){
		factory <- new("FileCacheFactory")
		moveFileCache(from, to, factory)
  }
)