## Class Definitions. Class definitions for the specific Synapse entity types
## are located in SynapseEntityDefinitions.R
##
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

##
## the global cache is a singleton
##
setClass(
    Class = "GlobalCache",
    representation = representation(env = "environment"),
    prototype = prototype(
        env = new.env(parent=emptyenv())
    )
)

##
## a file cache factory makes sure that all in-memory copies
## of a file cache object hold a reference to the same copy
##
setClass(
    Class = "FileCacheFactory",
    representation = representation(env = "environment"),
    prototype = new.env(parent = emptyenv())
)

##
## class for storing typed properties. Right now this is only
## used for storing synapse annotations, but in the future it will also be
## used to store typed synapse properties once the JSON schema is integrated
## with the R Synapse client
##
setClass(
    Class = "TypedPropertyStore",
    representation = representation(
        stringAnnotations = "list",
        doubleAnnotations = "list",
        longAnnotations = "list",
        dateAnnotations = "list",
        blobAnnotations = "list"
    ),
    prototype = prototype(
        stringAnnotations = emptyNamedList,
        doubleAnnotations = emptyNamedList,
        longAnnotations = emptyNamedList,
        dateAnnotations = emptyNamedList,
        blobAnnotations = emptyNamedList
    )
)

##
## this class may not seem necessary since it's just a wrapper on
## a list, but it will allow for an easier changeover to typed
## properties once the R client integrates the Synapse JSON schema
## this class is intended to be used to keep track of properties
## for both the Synapse "Annotations" entity and the "Base" Synapse
## entity
##
setClass(
    Class = "SimplePropertyOwner",
    contains = "VIRTUAL",
    representation = representation(
        properties = "list"
    ),
    prototype = prototype(
        properties = emptyNamedList
    )
)

##
## A class for representing the Synapse Annotations entity
##
setClass(
    Class = "SynapseAnnotations",
    contains = "SimplePropertyOwner",
    representation = representation(
        annotations = "TypedPropertyStore"
    ),
    prototype = prototype(
        annotations <- new("TypedPropertyStore")
    )
)

##
## this class definition is way too complicated. need to move some of the business logic elsewhere
##
setRefClass(
    Class = "FileCache",
    fields = list(
        cacheRoot = "character",
        cacheDir = "character",
        metaData = "list",
        archiveFile = "character"
    ),
    methods = list(
        initialize = function(){
          .self$initFields(
              metaData = emptyNamedList,
              archiveFile = "archive.zip"
          )
          root <- tempfile(pattern="cacheRoot")

          cdir <- file.path(root, sprintf("%s_unpacked", .self$archiveFile))
          if(!file.exists(cdir))
            dir.create(cdir, recursive=TRUE)
          .self$cacheRoot <- normalizePath(root)
          .self$cacheDir <- normalizePath(cdir)

          .self$cacheRoot <- gsub("[\\/]+$", "", gsub("[\\/]+", "/", normalizePath(root, mustWork=TRUE)))
          cdir <- file.path(.self$cacheRoot, pattern=sprintf("%s_unpacked", .self$archiveFile))
          .self$cacheDir <- gsub("[\\/]+", "/", normalizePath(cdir, mustWork=TRUE))
        },
        addFileMetaData = function(srcPath, destPath, ...){
          destPath <- as.character(.cleanFilePath(destPath))
          .self$metaData[[destPath]] <- list(srcPath = srcPath)
          elp <- list(...)
          if(any(names(elp) == ""))
            stop("All elements must be named")
          if(length(elp) > 0)
            .self$metaData[[destPath]] <- c(.self$metaData[[destPath]], elp)
          invisible(.self$metaData)
        },
        getFileMetaData = function(destPath){
          .self$metaData[destPath]
        },
        deleteFileMetaData = function(destPath){
          if(missing(destPath)){
            .self$metaData <- emptyNamedList
          }else{
            indx <- which(names(.self$metaData) %in% destPath)
            if(length(indx) > 0){
              .self$metaData = metaData[-indx]
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
          }else{
            dd <- fromJSON(file, simplifyWithNames=FALSE)
            .self$metaData <- dd$metaData
            .self$archiveFile <- dd$archiveFile
            if(.self$archiveFile=="")
              .self$archiveFile <- "archive.zip"
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
          
          ## delete the existing archive file
          if(file.exists(file.path(.self$cacheRoot, .self$archiveFile)))
            unlink(file.path(.self$cacheRoot, .self$archiveFile))

          ## if the FileCache has no files, delete the archive file, if it exists and return NULL
          if(length(.self$files()) == 0L){
             unlink(file.path(.self$cacheRoot, .self$archiveFile))
          
            return(NULL)
          }
          
          ## if the FileCache has no files, throw and exception
          if(length(.self$files()) == 0L)
            stop("There are not files to archive, add files using addFile then try again")
          
          ## if the archive file doesn't have a zip extension. simply copy it to the root
          if(!grepl("\\.zip",.self$archiveFile)){
            if(length(.self$files()) != 1L)
              stop("can only have one file when not zipping")
            file.copy(file.path(.self$cacheDir, .self$files()), .self$cacheRoot)
            ## re-cache the metaData to disk
            .self$cacheFileMetaData()
            return(invisible(.self$archiveFile))
          }

          ## this check should be done elsewhere, but for now let's leave it here.
          if(length(.self$files() > 1L) && ! hasZip())
            stop("Archive could not be created because it contains multiple files yet the system does not have zip installed.")

          if(!all(file.exists(file.path(.self$cacheDir, .self$files())))){
            ## more defensive programming. Getting here is potentially a bug unless the user
            ## mucked with the innards of the FileCache object or deleted a file from the cache directory
            stop("Not all of the file were present in the cache directory. this may be a bug. please report it.")
          }


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

          ## remove the archive file if it exists
          archFile <- file.path(.self$getCacheRoot(), .self$archiveFile)
          if(file.exists(archFile))
            unlink(archFile)

          ## OK, now let's zip. fingers crossed ;)
          ## change directory to the cache directory
          oldDir <- getwd()
          setwd(.self$cacheDir)
          suppressWarnings(
              zipRetVal <- zip(file.path(.self$cacheRoot, .self$archiveFile), files=gsub("^[\\/]","",.self$files()))
          )
          setwd(oldDir)

          ## if zip failes, load uncompressed
          if(zipRetVal != 0L){
            msg <- sprintf("Unable to zip Entity Files. Error code: %i.",zipRetVal)
            stop(msg)
          }

          ##update the meta-data to indicate that all the files are now sourced from the zipFile
          ans <- lapply(names(.self$getFileMetaData()), function(fname){
                .self$setFileSource(fname, .self$archiveFile)
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
          files <- .unpack(file.path(.self$cacheRoot, .self$archiveFile), .self$cacheDir)

          ## populate the file metadata
          files <- .generateFileList(attr(files, "rootDir"))
          .self$deleteFileMetaData()

          lapply(files$srcfiles, function(i){
                info <- file.info(files$srcfiles[i])
                for(name in names(info))
                  info[[name]] <- as.character(info[[name]])

                rPath <- gsub(gsub("[\\/]+", "/", .self$cacheDir), "", i, fixed = TRUE)
                rPath <- gsub("^[\\/]", "", rPath)
                .self$metaData[[i]] <- list(srcPath=.self$archiveFile, relativePath = rPath, fileInfo=info)
              }
          )

          ## persist the metadata to disk
          .self$cacheFileMetaData()

          invisible(.self$cacheDir)
        },
        setFileSource = function(fname, srcPath){
          .self$metaData[[fname]]$srcPath <- srcPath
        },
        getCacheDir = function(){
          .self$cacheDir
        },
        getCacheRoot = function(){
          .self$cacheRoot
        },
        delete = function(){
          .self$deleteFileMetaData()
          unlink(.self$getCacheDir(), recursive=TRUE, force=TRUE)
        },
        setArchiveFileName = function(fileName){
          oldFile <- file.path(.self$getCacheRoot(), .self$archiveFile)
          newFile <- file.path(.self$getCacheRoot(), fileName)
          archFile <- .self$createArchive()

          if(!is.null(archFile)){
            if(file.exists(newFile))
              unlink(newFile, recursive = TRUE, force = TRUE)

            if(file.exists(oldFile)){
              success <- file.copy(oldFile, newFile, overwrite=TRUE)
              if(!success)
                stop("unable to move archive file")
              unlink(oldFile)
            }
          }
          .self$archiveFile <- fileName
          if(file.exists(.self$cacheDir))
            unlink(.self$cacheDir, recursive=TRUE)
          .self$cacheDir <- sprintf("%s_unpacked", newFile)
          if(file.exists(newFile))
            .self$unpackArchive()
        }
    )
)

##
## A simple wrapper around an environment. This allows the customization
## of the environment's behavior, including the ability to make the environment
## read-only
##
setClass(
    Class = "EnhancedEnvironment",
    representation = representation(
        env = "environment"
    )
)

##
## An enhanced environment that caches it's objects to disk using a FileCache
## class to manage it's on-disk cache
##
setClass(
    Class = "CachingEnhancedEnvironment",
    contains = "EnhancedEnvironment",
    representation = representation(
        cachePrefix = "character",
        fileCache = "FileCache",
        cacheSuffix = "character",
        cacheTmpSuffix = "character"
    )
)

##
## wrapping FileCache in ArchiveOwner will allow for seamless
## switching between read-only and write-only mode in the future
## without messing with the FileCache class. For example, we could
## allow the user to set an archive to read-only by coercing it
## to a ReadOnlyArchive owner. Also, it allows us to re-devfine
## the $ operator since archive owner is an S4 class. we couldn't
## do this with FileCache since it's R5. This is neccessary to maintain
## backward compatibility of the user interface
setClass(
    Class = "ArchiveOwner",
    representation = representation(
        fileCache = "FileCache",
        objects = "EnhancedEnvironment"
    )
)

##
## All non-locationable Synapse entities will be derived from this class
##
setClass(
    Class = "SynapseEntity",
    contains = "SimplePropertyOwner",
    representation = representation(
        annotations = "SynapseAnnotations",
        synapseEntityKind = "character",
        synapseWebUrl = "character"
    ),
    prototype = prototype(
        annotations = new("SynapseAnnotations"),
        synapseWebUrl = ""
    )
)

##
## This class gives an object the ability to own R binary objects, allowing
## users to call the CRUD operations giving access to owned R objects. Note
## that the objects held in the "EnhancedEnvironment" are pass-by-reference.
## this should be highlighted prominently so user's don't get confused since
## pass-by-reference is virtually never used in R.
##
## at first glance this class might seem unneccessary, but the reason for
## wrappying EnhancedEnvironment is that it allows for some flexibility
## in the classes that inherit from (or are composed of) ObjectOwner. Specifically
## this will allow the implemenation of read-only ObjectOwners.
##
setRefClass(
    Class = "CachingObjectOwner",
    fields = list(
        objects = "CachingEnhancedEnvironment"
    ),
    methods = list(
        getObject = function(which){
          getObject(.self, which)
        },
        addObject = function(object, name, unlist = FALSE){
          if(missing(name) && class(object) == "list")
            addObject(.self, object, name, unlist)
        },
        getEnv = function(){
          .self@objects
        },
        initialize = function(){
          .self$initFields(
            objects = new("CachingEnhancedEnvironment")
          )
          setPackageName(env=.self)
        },
        files = function(){
          ffun <- getMethod("files", "CachingEnhancedEnvironment")
          ffun(.self$objects)[grep(sprintf("^%s", .self$objects@cachePrefix), ffun(.self$objects))]
        }
    )
)

##
## All locationable Synapse entities will be derived from this class
## If it is possible to determine that an entity with an unrecognized
## value of the "type" property is "Locationable", then change this from
## an abstract class to a concrete class by removing "VIRTUAL" from the
## contains list
##
setClass(
    Class = "SynapseLocationOwner",
    contains = c("SynapseEntity"),
    representation = representation(
        archOwn = "ArchiveOwner"
    )
)

##
## These entities can own R objects that are added via addObject.
##
setClass(
    Class = "SynapseLocationOwnerWithObjects",
    contains = c("SynapseLocationOwner"),
    representation(
      objOwn = "CachingObjectOwner"
    )
)
