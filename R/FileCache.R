# TODO: Add comment
# 
# Author: furia
###############################################################################

setRefClass(
    Class = "FileCache",
    fields = list(
        cacheRoot = "character",
        cacheDir = "character",
        metaData = "list",
        hasChanged = "logical",
        archiveFile = "character"
    ),
    methods = list(
        initialize = function(){
          .self$initFields(
              cacheRoot = normalizePath(tempfile(pattern="cacheRoot"), mustWork=FALSE),
              metaData = emptyNamedList,
              hasChanged = FALSE,
              archiveFile = "archive.zip"
          )
          .self$cacheDir <- file.path(.self$cacheRoot, pattern=sprintf("%s_unpacked", .self$archiveFile))
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
          if(!file.exists(.self$cacheRoot))
            dir.create(.self$cacheRoot)
          cat(toJSON(list(archiveFile = .self$archiveFile, metaData = .self$metaData)), file=file.path(.self$cacheRoot, "files.json"))
        },
        deleteMetaDataCache = function(){
          file.remove(file.path(.self$cacheRoot, "files.json"))
        },
        loadMetaDataFromFile = function(){
          file = file.path(.self$cacheRoot, "files.json")
          if(!file.exists(file)){
            .self$metaData <- emptyNamedList
            .self$archiveFile <- ""
          }else{
            dd <- fromJSON(file, simplifyWithNames=FALSE)
            .self$metaData <- dd$metaData
            .self$archiveFile <- dd$archiveFile
          }
          invisible(list(archiveFile = .self$archiveFile, metaData = .self$metaData))
        },
        files = function(){
          as.character(unlist(lapply(.self$getFileMetaData(), function(m) m$relativePath)))
        },
        createArchive = function(){
          ## zips up the archive contents and invisibly returns the full path to the created archive file
          ## this function should also update the metadata to reflect the fact that the archive was changed
          ## although this is not needed now, but will be when we wait to aggregate added files in the cache
          ## directory until archive creation time
          
          ## if the FileCache has no files, throw and exception
          if(length(.self$files()) == 0L)
            stop("There are not files to archive, add files using addFile then try again")
          
          ## this check should be done elsewhere, but for now let's leave it here.
          if(length(.self$files() > 1L) && ! hasZip())
            stop("Archive could not be created because it contains multiple files yet the system does not have zip installed.")
          
          if(!all(file.exists(file.path(.self$cacheDir, .self$files())))){
            ## more defensive programming. Getting here is potentially a bug unless the user
            ## mucked with the innards of the FileCache object or deleted a file from the cache directory
            stop("Not all of the file were present in the cache directory. this may be a bug. please report it.")
          }
          
          ## if the archive file is unset. set it to a logical default. by default we will assume the 
          ## system has zip installed so will use a .zip extension
          if(is.null(.self$archiveFile) || .self$archiveFile == ""){
            if(hasZip()){
              .self$archiveFile = "archive.zip"
            }else if(length(.self$files()) == 1L){
              .self$archiveFile = .self$files()[[1]]
            }else{
              ## defensive programming. should never get here
              stop("An error has occured in FileCache while determining number of files. Please report this bug.")
            }
          }

          ## if the cacheRoot doesn't exists, create it. this should never happen
          if(!file.exists(.self$cacheRoot))
            dir.create(.self$cacheRoot, recursive = TRUE)
          
          ## OK, now let's zip. fingers crossed ;)
          ## change directory to the cache directory
          oldDir <- getwd()
          setwd(.self$cacheDir)
          suppressWarnings(
            zipRetVal <- zip(file.path(.self$cacheRoot, .self$archiveFile), files=gsub("^/","",.self$files()))
          )
          setwd(oldDir)
          
          ## if zip failes, load uncompressed
          if(zipRetVal != 0L){
            msg <- sprintf("Unable to zip Entity Files. Error code: %i.",zipRetVal)
            stop(msg)
          }
          
          ##update the meta-data to indicate that all the files are now sourced from the zipFile
          ans <- lapply(names(.self$getFileMetaData()), function(name){
              m <- .self$getFileMetaData()[[name]]
              m$srcPath <- .self$archiveFile
              .self$metaData[[name]] <- m
            }
          )
          
          ## re-cache the metaData to disk
          .self$cacheFileMetaData()
          
          ## invisibly return the archive file name
          invisible(.self$archiveFile)
        },
        getArchiveFile = function(){
          ## getter for the archive file name. this will be a file name relative to the cacheRoot
          .self$archiveFile
        },
        unpackArchive = function(){
          ## unpacks the contents of the archive file, throwing an exception if the archiveFile member variable is not set
          ## invisibly returns the full path to the root directory into which the archive was unpacked
          
          ## remove the contents of the cacheDir
          unlink(.self$cacheDir, force=TRUE, recursive = TRUE)
          files <- .unpack(file.path(.self$cacheRoot, .self$archiveFile))
          invisible(.self$cacheDir)
        }
    )
)

setMethod(
    f = "FileCache",
    signature = signature("character", "missing", "missing"),
    definition = function(cacheRoot){
      fc <- new("FileCache")
      fc$cacheRoot <- cacheRoot
      fc$loadMetaDataFromFile()
      fc
    }
)

setMethod(
    f = "FileCache",
    signature = signature("missing", "missing", "missing"),
    definition = function(){
      new("FileCache")
    }
)

setMethod(
  f = "FileCache",
  signature = signature("missing", "missing", "character"),
  definition = function(archiveFile){
    ## this constructor requires that the archive exists. Currently the archive must be a file
    ## that can be unpacked using R's unzip function, or it should be a single file. This constructor
    ## in the future should possibly return a read-only FileCache if the user doesn't have zip installed
    archiveFile <- normalizePath(archiveFile, mustWork=TRUE)
    
    fc <- new("FileCache")
    fc$archiveFile <- gsub("^.+/", "", archiveFile)
    fc$cacheRoot <- gsub(fc$archiveFile, "", archiveFile, fixed = TRUE)
    fc$cacheDir <- file.path(fc$cacheRoot, sprintf("%s_unpacked", fc$archiveFile))
    fc
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
      
      ## convert all values to character for now
      lapply(names(info), function(i) info[[i]] <<- as.character(info[[i]]))
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
setMethod(
  f = "deleteFile",
  signature = signature("FileCache", "character"),
  definition = function(entity, file){
    file <- gsub("^[\\/]+","", file)
    file <- gsub("[\\]+","/", file)
    
    ## delete from the local cache
    if(!all(mk <- (file %in% entity$files())))
      stop(sprintf("Invalid file: %s\n", file[!mk]))
    
    indx <- which(entity$files() %in% file)
    deleteFiles <- names(entity$getFileMetaData())[indx]
    tryCatch(
      file.remove(deleteFiles, recursive=TRUE),
      error = function(e){
        warning(sprintf("Unable to remove file from local cache: %s", e))
      }
    )
    
    ## remove from the list of files
    entity$deleteFileMetaData(deleteFiles)
    
    ## clean up empty directories
    .recursiveDeleteEmptyDirs(entity$cacheDir)
    
    invisible(entity)
  }
)


## move file methods
setMethod(
  f = "moveFile",
  signature = "FileCache",
  definition = function(entity, src, dest){
    src <- gsub("^[\\\\/]+","", src)
    dest <- gsub("^[\\\\/]+","", dest)
    if(!(src %in% entity$files()))
      stop(sprintf("Invalid file: %s", src))
    
    if(dest %in% entity$files())
      stop(sprintf('Destination file "%s" already exists. Delete it using deleteFile() then try again.', dest))
    
    if(any(grepl(sprintf("^%s/",dest), entity$files())))
      stop(sprintf('Destination file "%s" already exists as a directory. Please choose a different destination filename and try again.', dest))
    
    ## if dest is a directory, move but don't rename
    if(grepl("[\\\\/]$", dest) || dest == ""){
      addFile(entity, file.path(entity$cacheDir, src), dest)
    }else{
      ## rename and copy the file to a temp directory, then add it from there
      filename <- gsub("^.+[\\\\/]", "", dest)
      tmpdir <- tempfile()
      dir.create(tmpdir, recursive=TRUE)
      newSrc <- file.path(tmpdir, filename)
      file.copy(file.path(entity$cacheDir, src), newSrc)
      path <- gsub(sprintf("[\\\\/]?%s$",filename),"", dest)
      addFile(entity, newSrc, path)               
    }
    
    ## delete the original file 
    deleteFile(entity, src)
  }         
)


