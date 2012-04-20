# TODO: Add comment
# 
# Author: furia
###############################################################################
setMethod(
  f = "setCacheRoot",
  signature = signature("FileCache", "character", "missing", "missing"),
  definition = function(object, path){
    setCacheRoot(object, path, FALSE)
  }
)

setMethod(
  f = "setCacheRoot",
  signature = signature("FileCache", "character", "logical", "missing"),
  definition = function(object, path, clean){
    if(file.exists(path)){
      if(!clean)
        stop("destination already exists, please remove or set forceClean to TRUE")
      
      ## if the new cacheroot is the same as the old one, do nothing
      if(normalizePath(path) == normalizePath(object$getCacheRoot(), mustWork=FALSE))
        return(object)
      
      ## remove the new cacheroot
      unlink(path, recursive=TRUE)
    }
    
    ## compute the cachedir <cacheroot>/<cachedir>
    cacheDir <- file.path(path, sprintf("%s_unpacked", object$archiveFile))
    dir.create(cacheDir, recursive=TRUE)
    
    ## copy over the existing archive. by default we copy everyting
    ## TODO: implement different copy modes: all, none, files, archive
    if(file.exists(object$getCacheRoot())){
      file.copy(file.path(object$getCacheRoot(), object$archiveFile), path)
      file.copy(object$getCacheDir(), path, recursive=TRUE)
      file.copy(file.path(object$getCacheRoot(), "files.json"), path)
    }
    
    ## clean up old files
    if(clean)
      unlink(object$getCacheRoot(), recursive = TRUE)
    
    ## set the member variables to reflect the new values
    object$cacheDir <- normalizePath(cacheDir)
    object$cacheRoot <- normalizePath(path)
   
    invisible(object)
  }
)


setMethod(
  f = "FileCache",
  signature = signature("character", "missing", "missing"),
  definition = function(cacheRoot){
    fc <- new("FileCache")
    if(!file.exists(cacheRoot))
      tryCatch(
        dir.create(cacheRoot, recursive=TRUE),
        warning = function(e){
          stop(e)
        }
      )
    if(is.na(file.info(cacheRoot)$isdir) || !file.info(cacheRoot)$isdir)
      stop("Cache root must be a directory")
    cacheRoot <- gsub("/+$", "", gsub("/+", "/", normalizePath(cacheRoot)))
    fc$cacheRoot <- cacheRoot
    fc$cacheDir <- file.path(fc$cacheRoot, sprintf("%s_unpacked", fc$archiveFile))
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
    fc$cacheRoot <- gsub("/+$", "", gsub(fc$archiveFile, "", archiveFile, fixed = TRUE))
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
    ##x <- gsub("[\\/]+$", "", x)
    x <- gsub("[\\/]+", "/", x)
    x <- gsub("^/", "", x)
    mk <- x %in% c("")
    x[mk] <- "/"
    x
  }
  
  if(missing(path))
    path <- "/"
  
  ## clean up the subdirectories
  if(length(path) > 1L || path != "/")
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
  srcfiles <- unlist(srcfiles)
  destfiles <- unlist(destfiles)
  destPaths <- unlist(destPaths)
  mk <- !grepl("/$", destPaths)
  if(any(mk)){
    destfiles[mk] <- destPaths[mk]
    destPaths[mk] <- "/"
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
            recursive=FALSE
            if(!is.na(file.info(file.path(entity$cacheDir, destPath))$isdir) && file.info(file.path(entity$cacheDir, destPath))$isdir){
              recursive <- TRUE
            }
            file.copy(files$srcfiles[i], file.path(entity$cacheDir, destPath), overwrite = TRUE, recursive = recursive)
            
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
    if(length(src) != 1L)
      stop("only one file can be moved at a time")
    if(length(src) != length(dest))
      stop("number of source and destination files must be the same")
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
      path <- dest
      if(grepl("/", dest)){
        splits <- strsplit(dest,"/")[[1]]
        path <- sprintf("%s/",file.path(splits[-length(splits)]))
      }
      addFile(entity, newSrc, path)               
    }
    
    ## delete the original file 
    deleteFile(entity, src)
  }         
)


