# TODO: Add comment
# 
# Author: furia
###############################################################################

setRefClass(
  Class = "FileCache",
  fields = list(
    cacheDir = "character",
    metaData = "list",
    hasChanged = "logical"
  ),
  methods = list(
    initialize = function(){
      .self$initFields(
        cacheDir = tempfile(pattern = "fileCache"),
        metaData = emptyNamedList,
        hasChanged = FALSE
      )
    },
    addFileMetaData = function(srcPath, destPath, ...){
      .self$metaData[[destPath]] <- list(srcPath = srcPath)
      elp <- list(...)
      if(any(names(elp) == ""))
        stop("All elements must be named")
      if(length(elp) > 0)
        .self$metaData[[destPath]] <- c(.self$metaData[[destPath]], elp)
      .self$hasChanged <- TRUE
      invisible(.self$metaData)
    },
    getFileMetaData = function(destPath){
      .self$metaData[destPath]
    },
    deleteFileMetaData = function(destPath){
      if(missing(destPath)){
        .self$metaData <- emptyNamedList
        .self$hasChanged <- TRUE
      }else{
        indx <- which(names(.self$metaData) %in% destPath)
        if(length(indx) > 0){
          .self$metaData = metaData[-indx]
          .self$hasChanged <- TRUE
        }
      }
      invisible(.self$metaData)
    },
    cacheFileMetaData = function(){
      if(!file.exists(.self$cacheDir))
        dir.create(.self$cacheDir)
      cat(toJSON(.self$metaData), file=file.path(.self$cacheDir, "files.json"))
    },
    deleteMetaDataCache = function(){
      file.remove(file.path(.self$cacheDir, "files.json"))
    },
    loadMetaDataFromFile = function(){
      file = file.path(.self$cacheDir, "files.json")
      if(!file.exists(file)){
        .self$metaData <- emptyNamedList
      }else{
        .self$metaData <- fromJSON(file, simplifyWithNames=FALSE)
      }
      invisible(.self$metaData)
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





