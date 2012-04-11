# TODO: Add comment
# 
# Author: furia
###############################################################################

setRefClass(
  Class = "FileCache",
  fields = list(
    cacheDir = "character",
    files = "list"
  ),
  methods = list(
    initialize = function(){
      .self$initFields(
        cacheDir = tempfile(pattern = "fileCache"),
        files = list()
      )
    }
  )
)

## addFile methods
setMethod(
  f = "addFile",
  signature = signature("FileCache", "character", "character", "ANY"),
  definition = function(entity, file, path, prefix){
    if(length(path) > 1 && (length(path) != length(file)))
      stop("Must provide either a single path, or one path for each file")
    if(any(file.info(file)$isdir) && length(path) > 1)
      stop("when adding directories, provide only a single path")
    
    if(!all(mk<-file.exists(file)))
      stop(sprintf("File not found: %s", file[!mk]))
    
    if(!hasZip() && length(entity$files) > 0){
      stop("Unable to add file. Zip is not installed on your system ")
    }
    
    if(missing(prefix))
      prefix <- ""
    if(!is.numeric(prefix) && !is.character(prefix))
      stop("Prefix must be a character or numeric")
    
    if(length(prefix) > 1 && (length(prefix) != length(file)))
      stop("Must provide either a prefix path, or one path for each file")
      
    
    path <- gsub("[\\/]+$", "", path)
    path <- gsub("[\\/]+", "/", path)
    path <- gsub("^[/]+", "", path)
    mk <- path %in% c("")
    destdir <- rep("/", length(path))
    cacheDir <- entity$cacheDir
    if(any(mk))
      destdir[mk] <- file.path(entity$cacheDir, sprintf("%s%s", prefix[mk],  path[mk]))
    if(any(!mk))
      destdir[!mk] <- file.path(entity$cacheDir, sprintf("%s%s", prefix[!mk],  path[!mk]))
    
    if(!all(mk <- file.exists(destdir)))
      lapply(destdir[!mk], function(d) dir.create(d, recursive = TRUE))
    
    if(length(file) == length(path)){
      for(i in 1:length(file)){
        if(file.info(file[i])$isdir){
          recursive = TRUE
        }else{
          recursive = FALSE
        }
        file.copy(file[i], destdir[i], overwrite = TRUE, recursive=recursive)
      }
    }else{
      file.copy(file, destdir, overwrite=TRUE, recursive=TRUE)
    }
    files <- dir(destdir, recursive=TRUE, all.files=T, full.names=T)
    ## drop directories
    if(any(mk <- file.info(files)$isdir))
      files <- files[!mk]
    
    ## clip out the cacheDir bit
    files <- gsub(entity$cacheDir, "", files, fixed=TRUE)
    
    files <- gsub("^[\\/]+", "", files)
    files <- gsub("[\\/]+", "/", files)
    if(any(!(mk <- files %in% entity$files)))
      entity$files <- c(entity$files, files[!mk])
    entity
  }
)

setMethod(
  f = "addFile",
  signature = signature("FileCache", "character", "missing", "ANY"),
  definition = function(entity, file, prefix){
    ## the default path
    path = "/"

    ## default prefix
    if(missing(prefix))
      prefix <- ""

    addFile(entity, file, path=path, prefix = prefix)
  }
)


## deleteFile methods





