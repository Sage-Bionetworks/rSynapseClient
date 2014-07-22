## Synapse Code entity constructors
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

setClass(
  Class="Code",
  contains = "VIRTUAL"
)

edit.Code <- 
  function(name, which, ...)
{
  args <- as.list(substitute(list(...)))[-1L]
  if("file" %in% names(args))
    stop("file argument is currently not supported")
  if(missing(which)){
    if(length(name$files) == 0){
      filename <- tempfile(fileext=".R")
      file.create(filename)
      name <- addFile(name, filename, "/code.R")
      warning("Adding new file to entity named code.R")
    }
    which <- 1:length(name$files)
  }
  if(!(is.numeric(which)))
    stop("argument 'which' must be numeric")
  
  which <- as.integer(which)
  if(any(which) > length(name$files))
    stop("Invalid file specified")

  file <- file.path(name$cacheDir, name$files)[which]
  
  file.edit(file, ...)
  invisible(name)
}

setMethod(
  f = "loadEntity",
  signature = signature("Code", "missing"),
  definition = function(entity){
    if(!is.null(propertyValue(entity, "id"))){
      entity <- downloadEntity(entity)
    }
    indx <- grep("\\.r$", tolower(entity$files))
    tryCatch(
      lapply(entity$files[indx],
        function(f){
          f <- file.path(entity$cacheDir, f)
          sys.source(f, env = as.environment(as.environment(entity@archOwn@objects)))
        }
      ),
      error = function(e){
        warning(e)
      }
    )
    # finally, we load any R binaries for this object
    entity@objOwn$objects@fileCache <- entity@archOwn@fileCache
    # note, we tell 'loadObjectsFromFile' NOT to clear the environment before loading!
    entity@objOwn <- loadObjectsFromFiles(entity@objOwn, clearEnvironment=F)
    entity
  }
)

setMethod(
  f = "addGithubTag",
  signature = signature("Code", "character"),
  definition = function(entity, url){
    
    ## CHECK TO MAKE SURE THERE IS NO LOCATION FOR THIS CODE OBJECT YET
    if(!is.null(propertyValue(entity, "locations"))){
      stop("This Code Entity already has a location.")
    }
    
    class(entity) <- "GithubCode"
    
    destfile <- .curlWriterDownload(url=url)
    
    if(!exists("destfile")){
      stop(paste("Downloaded file", destfile, "does not exists"))
    }
    
    md5 <- as.character(tools::md5sum(destfile))
    file.remove(destfile)
    
    ## SET LOCATIONS AND MD5 PROPERTIES
    propertyValue(entity, "locations") <- list(list(path=url, type="external"))
    propertyValue(entity, "md5") <- md5
    annotValue(entity, "githubRepo") <- dirname(dirname(url))
    annotValue(entity, "githubRepoTag") <- basename(url)
    
    entity
  }
)
