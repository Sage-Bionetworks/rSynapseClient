## Synapse Code entity constructors
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################


setMethod(
  f = "Code",
  signature = "list",
  definition = function(entity){
    ee <- new("Code")
    ee@properties <- entity
    ee
  }
)

setMethod(
  f = "Code",
  signature = "missing",
  definition = function(entity){
    Code(list())
  }
)
#
#edit.Code <- 
#  function(name, which, ...)
#{
#  args <- as.list(substitute(list(...)))[-1L]
#  if("file" %in% names(args))
#    stop("file argument is currently not supported")
#  if(missing(which)){
#    if(length(name$files) == 0){
#      filename <- tempfile(fileext=".R")
#      file.create(filename)
#      name <- addFile(name, filename, "/code.R")
#      warning("Adding new file to entity named code.R")
#    }
#    which <- 1:length(name$files)
#  }
#  if(!(is.numeric(which)))
#    stop("argument 'which' must be numeric")
#  if(any(which) > length(name$files))
#    stop("Invalid file specified")
#  
#  if(!("file" %in% names(args))){
#    tmpFile <- tempfile()
#    file.create(tmpFile)
#    file <- "code.R"
#    name <- addFile(name, tmpFile, file)
#  }else{
#    name <- addFile(name, file)
#  }
#  
#  file.edit(file.path(name$cacheDir, file), ...)
#  invisible(name)
#}

#setMethod(
#  f = "loadEntity",
#  signature = "Code",
#  definition = function(entity){
#    if(!is.null(propertyValue(entity, "id"))){
#      entity <- downloadEntity(entity)
#    }
#    indx <- grep("\\.r$", tolower(entity$files))
#    if(!is.null(propertyValue(entity, "id"))){
#      setPackageName(sprintf("entity%s", propertyValue(entity, "id")), env = as.environment(entity@archOwn@objects))
#    }else{
#      setPackageName(tempfile(), env = as.environment(as.environment(entity@archOwn@objects)))
#    }
#    tryCatch(
#      lapply(entity$files[indx],
#        function(f){
#          f <- file.path(entity$cacheDir, f)
#          sys.source(f, env = as.environment(as.environment(entity@archOwn@objects)))
#        }
#      ),
#      error = function(e){
#        warning(e)
#      }
#    )
#    entity
#  }
#)