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
              metaData = emptyNamedList,
              hasChanged = FALSE
          )
          cc <- tempfile(pattern = "fileCache")
          cc <- normalizePath(cc, mustWork=FALSE)
          .self$cacheDir <- cc
        },
        addFileMetaData = function(srcPath, destPath, ...){
          destPath <- as.character(.cleanFilePath(destPath))
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

setMethod(
    f = "FileCache",
    signature = signature("character", "missing"),
    definition = function(cacheDir){
      fc <- new("FileCache")
      fc$cacheDir <- cacheDir
      fc$loadMetaDataFromFile()
      fc
    }
)

setMethod(
    f = "FileCache",
    signature = signature("missing", "missing"),
    definition = function(){
      new("FileCache")
    }
)

.cleanFilePath <- 
    function(filePath)
{
  filePath <- normalizePath(filePath, mustWork = FALSE)
  filePath <- gsub("[\\/]+", "/", filePath)
  
  ##determine if the filePath is a directory by checking for a trailing slash
  if(grepl("/$", filePath)){
    isdir <- TRUE
  }else{
    isdir <- FALSE
  }
  
  ## If the file exists, check to see if it's a directory
  info <- file.info(filePath)
  if(!is.na(info$isdir) && info$isdir)
    isdir <- TRUE
  
  ## set the isDir attribute
  attr(filePath, "isDir") <- isdir
  
  filePath
}

setMethod(
    f = "addFileMetaData",  
    signature = signature("FileCache", "character", "character"),
    definition = function(object, srcPath, destPath){
      if(length(srcPath) != 1L)
        stop("must provide exactly 1 source file")
      if(length(destPath) != 1L)
        stop("must provide exactly 1 destination path")
      
      info <- file.info(srcPath)
      if(!is.na(info$isdir) && info$isdir)
        stop("Adding metaData for directories is not supported")
      
      
      ## get rid of evil back-slashes
      destPath <- gsub("[\\]+", "/", destPath)
      
      ## the dest path is a path relative the the root directory of the cache
      destPath <- file.path(object$cacheDir, destPath)
      destPath <- .cleanFilePath(destPath)
      
      ## if destPath is a directory, use the original filename
      ## make sure the srcPath is not a directory either. a little defensive programming here.
      if(attr(destPath, "isDir") && !(!is.na(info$isdir) && info$isdir)){
        srcPathClean <- as.character(.cleanFilePath(srcPath))
        srcFname <- gsub(".+/", "", srcPathClean)
        destPath <- normalizePath(file.path(as.character(destPath), srcFname), mustWork=FALSE)
      }
      
      ## add some file metadata. for now, just add the default info plus the relative file path and the file size
      relPath <- gsub(normalizePath(object$cacheDir, mustWork=FALSE), "", as.character(destPath), fixed = TRUE)
      relPath <- gsub("^/+", "", relPath)
      info <- as.list(file.info(srcPath))
      object$addFileMetaData(srcPath, destPath, relativePath = relPath, fileInfo = info)
    }
)

setMethod(
    f = "addFileMetaData",
    signature = signature("FileCache", "character", "missing"),
    definition = function(object, srcPath){
      destPath <- "/"
      addFileMetaData(object, srcPath, destPath)
    }
)

.generateFileList <- function(file, path){
  cleanPath <- function(x){
    x <- gsub("[\\/]+$", "", x)
    x <- gsub("[\\/]+", "/", x)
    x <- gsub("^/", "", x)
    mk <- x %in% c("")
    x[mk] <- "/"
    x
  }
  
  ## clean up the subdirectories
  path <- cleanPath(path)
  file <- unlist(lapply(file, function(f) as.character(synapseClient:::.cleanFilePath(f))))
  
  ## get a full listing of the files
  srcfiles <- lapply(file, function(f){
        info <- file.info(f)
        if(is.na(info$isdir) || !info$isdir)
          return(f)
        
        ff <- list.files(f, recursive=TRUE, full.names=TRUE, all.files=TRUE)
        for(i in 1:length(ff))
          ff[i] <- synapseClient:::.cleanFilePath(ff[i])
        ff
      }
  )
  
  ## build the destination files
  destfiles <- lapply(file, function(f){
        info <- file.info(f)
        if(is.na(info$isdir) || !info$isdir)
          return(gsub("^.+/", "", f))
        ff <- list.files(f, recursive=TRUE, full.names=TRUE, all.files=TRUE)
        f <- as.character(cleanPath(f))
        dd <- gsub("^.+/", "", f)
        file.path(dd,cleanPath(gsub(f, "", ff, fixed = TRUE)))
      }
  )
  
  ## build the relative destination file paths for each block of files
  if(length(path) == 1){
    destPaths <- lapply(destfiles, function(ff){
          rep(path, length(ff))
        }
    )
  }else if(length(path) == length(srcfiles)){
    destPaths <- lapply(1:length(file), function(i){
          rep(path[i], length(srcfiles[[i]]))
        }
    )
  }else{
    ## should never get here. defensive programming
    stop("error gnerating destination file paths. number of paths did not match number of files")
  }

  list(
      srcfiles = unlist(srcfiles),
      destfiles = unlist(destfiles),
      path = unlist(destPaths)
  )
}


setMethod(
    f = "addFile",
    signature = signature("FileCache", "character", "character"),
    definition = function(entity, file, path){
      
      if(!all(file.exists(file)))
        stop("One or more specified files does not exist")
      
      ## need to make sure the numbers of files and paths makes sense
      if(length(path) > 1 && length(path) != length(file))
        stop("Must provide either a single path value or a number of path values that equals the total number of files")
      
      files <- .generateFileList(file, path)
      
      ans <- lapply(1:length(files$srcfiles), function(i){
            ## build up the dest path
            destPath <- file.path(files$path[i], files$destfile[i])
            
            ## create the destination directory if it doesn't exist
            fname <- gsub("^.+/", "", destPath)
            pp <- gsub(fname, "", destPath, fixed = TRUE)
            if(!file.exists(file.path(entity$cacheDir, pp)))
              dir.create(file.path(entity$cacheDir, pp), recursive=TRUE)
            
            ## copy the file
            file.copy(files$srcfiles[i], file.path(entity$cacheDir, pp), recursive = TRUE)
            
            ## add the metadata to the FileCache
            addFileMetaData(entity, files$srcfiles[i], destPath)
          }
      )
      invisible(entity)
    }
)

setMethod(
    f = "addFile",
    signature = signature("FileCache", "character", "missing"),
    definition = function(entity, file){
      ## the default path
      path = "/"
      
      addFile(entity, file, path=path)
    }
)


## deleteFile methods





