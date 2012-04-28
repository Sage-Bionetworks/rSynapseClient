# TODO: Add comment
# 
# Author: mfuria
###############################################################################

.setUp <- 
    function()
{
#  env <- attach(NULL, name = "testEnv")
#  tryCatch({
#        setPackageName("testEnv", env)
#        suppressWarnings(
#            setClass(
#                "LocOwnO",
#                contains = "SynapseLocationOwnerWithObjects",
#                where = env
#            ) 
#        )
#      },
#      error = function(e){
#        detach("testEnv")
#        stop(e)
#      }
#  )
  
  synapseClient:::.setCache("oldWarn", options("warn")[[1]])
  options(warn=2L)
}

.tearDown <-
    function()
{
#  detach("testEnv")
  options(warn = synapseClient:::.getCache("oldWarn"))
}

unitTestAddObject <-
    function()
{
  own <- new("SynapseLocationOwnerWithObjects")
  
  foo<-diag(10)
  copy <- addObject(own, foo)
  checkEquals(names(own$objects), "foo")
  checkEquals(names(copy$objects), "foo")
  
  ## check that the object loaded from the .rbin masks
  ## the one added via addObjects
  foo <- "bar"
  file <- tempfile(fileext=".rbin")
  save(foo, file=file)
  addFile(own, file)
  checkEquals("matrix", as.character(class(own$objects$foo)))
  copy <- synapseClient:::loadObjectsFromFiles(own)
  checkEquals("character", as.character(class(own$objects$foo)))
  checkEquals("character", as.character(class(copy$objects$foo)))
  
  ## now rename the "objects" foo so it will no longer be masked
  copy <- renameObject(own, "foo", "goo")
  checkEquals(2L, length(names(own$objects)))
  checkTrue(all(names(own$objects) == c("foo", "goo")))
  checkEquals(2L, length(names(copy$objects)))
  checkTrue(all(names(copy$objects) == c("foo", "goo")))
  
}

unitTestAddObjectRename <-
    function()
{
  own <- new("SynapseLocationOwnerWithObjects")
  
  foo<-diag(10)
  copy <- addObject(own, foo, "bar")
  checkEquals(length(names(own$objects)), 1L)
  checkEquals(names(own$objects), "bar")
  checkEquals(length(names(copy$objects)), 1L)
  checkEquals(names(copy$objects), "bar")
}

unitTestAddList <-
    function()
{
  own <- new("SynapseLocationOwnerWithObjects")
  
  foo <- list(b="a", one=1)
  copy <- addObject(own, foo, "bar")
  checkEquals(length(names(own$objects)), 1L)
  checkEquals(names(own$objects), "bar")
  checkEquals(as.character(class(own$objects$bar)), "list")
  checkEquals(length(names(copy$objects)), 1L)
  checkEquals(names(copy$objects), "bar")
  
  own <- new("SynapseLocationOwnerWithObjects")
  copy <- addObject(own, foo)
  checkEquals(length(names(own$objects)), 1L)
  checkEquals(names(own$objects), "foo")
  checkEquals(as.character(class(own$objects$foo)), "list")
  checkEquals(length(names(copy$objects)), 1L)
  checkEquals(names(copy$objects), "foo")
  
}


unitTestAddListFcn <- 
  function()
{
  own <- new("SynapseLocationOwnerWithObjects")
  foo <- list(a="b", one=1)
  copy <- addObject(own, foo)
  checkEquals(length(own$objects), 1L)
  checkEquals(names(own$objects), "foo")
  checkEquals(own$objects$foo$a, "b")
  checkEquals(own$objects$foo$one, 1L)
  checkEquals(length(copy$objects), 1L)
  checkEquals(names(copy$objects), "foo")
  checkEquals(copy$objects$foo$a, "b")
  checkEquals(copy$objects$foo$one, 1L)
  
  own <- new("CachingObjectOwner")
  copy <- addObject(own, foo, "bar")
  checkEquals(length(own$objects), 1L)
  checkEquals(names(own$objects), "bar")
  checkEquals(own$objects$bar$a, "b")
  checkEquals(own$objects$bar$one, 1L)
  checkEquals(length(copy$objects), 1L)
  checkEquals(names(copy$objects), "bar")
  checkEquals(copy$objects$bar$a, "b")
  checkEquals(copy$objects$bar$one, 1L)
}

unitTestAddListUnlist <-
    function()
{
  own <- new("SynapseLocationOwnerWithObjects")
  foo <- list(a="b", one=1)
  copy <- addObject(own, foo, unlist=T)
  checkEquals(length(own$objects), 2L)
  checkTrue(all(names(own$objects) == c("a", "one")))
  checkEquals(own$objects$a, "b")
  checkEquals(own$objects$one, 1L)
  checkEquals(length(copy$objects), 2L)
  checkTrue(all(names(copy$objects) == c("a", "one")))
  checkEquals(copy$objects$a, "b")
  checkEquals(copy$objects$one, 1L)
  
  own <- new("CachingObjectOwner")
  checkException(addObject(own, foo, "bar", T))
  
}

unitTestAddDataFrame <-
    function()
{
  
}

unitTestDeleteObject <-
    function()
{
  own <- new("SynapseLocationOwnerWithObjects")
  foo <- list(a="b", one=1)
  addObject(own, foo)
  checkEquals(length(own$objects), 1L)
  copy <- deleteObject(own, "foo")
  checkEquals(length(own$objects), 0L)
  checkEquals(length(copy$objects), 0L)
  
}

unitTestRenameObject <-
    function()
{
  own <- new("SynapseLocationOwnerWithObjects")
  foo <- "boo"
  addObject(own, foo)
  checkEquals(length(own$objects), 1L)
  copy <- renameObject(own, "foo", "bar")
  checkEquals(length(own$objects), 1L)
  checkEquals(length(copy$objects), 1L)
  checkEquals(copy$objects$bar, "boo")
  checkEquals(own$objects$bar, "boo")
}

unitTestGetObject <-
    function()
{
  own <- new("SynapseLocationOwnerWithObjects")
  foo <- "boo"
  addObject(own, foo)
  checkEquals(getObject(own, "foo"), "boo")
}

unitTestLoadObjectsFromDisk <-
    function()
{
  stop("notYetImplemented")
}

unitTestLoadObjectsMaskedByFileObjects <-
    function()
{
  stop("notYetImplemented")
}

unitTestShow <-
    function()
{
  stop("notYetImplemented")
}


##
##
## Double check that super-class methods all still work
##
##


unitTestNoArgConstructor <-
    function()
{
  stop("not yet implemented")
}

unitTestConstructorArchivePath <-
    function()
{
  stop("not yet implemented")
}

unitTestAddFile <- 
    function()
{
  own <- new("SynapseLocationOwnerWithObjects")
  
  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", Sys.time()), file = file)
  copy <- addFile(own, file)
  checkEquals(basename(file), own$files)
  checkEquals(basename(file), copy$files)
  
  ## make sure the cache re-initializes but running the exact same
  ## test again
  own <- new("SynapseLocationOwnerWithObjects")
  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", Sys.time()), file = file)
  addFile(own, file)
  checkEquals(basename(file), own$files)
  
  addFile(own, file, "foo.bar")
  checkEquals(length(own$files), 2L)
  checkTrue(all(c(basename(file), "foo.bar") %in% own$files))
  checkTrue(all(own$files %in% c(basename(file), "foo.bar")))
}

unitTestMoveFile <-
    function()
{
  own <- new("SynapseLocationOwnerWithObjects")
  
  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", Sys.time()), file = file)
  addFile(own, file)
  checkEquals(basename(file), own$files)
  
  copy <- moveFile(own, own$files, "newName.txt")
  checkEquals("newName.txt", copy$files)
  checkEquals("newName.txt", own$files)
  
  moveFile(own, own$files, "subdir/")
  checkEquals("subdir/newName.txt", own$files)
  
  moveFile(own, own$files, "/newSubDir2/anotherName2.txt")
  checkEquals("newSubDir2/anotherName2.txt", own$files)
  
  moveFile(own, own$files, "/newSubDir/anotherName.txt")
  checkEquals("newSubDir/anotherName.txt", own$files)
  
}

uniTestDeleteFile <-
    function()
{
  own <- new("SynapseLocationOwnerWithObjects")
  
  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", Sys.time()), file = file)
  addFile(own, file, "aFile.txt")
  checkEquals("aFile.txt", own$files)
  
  copy <- deleteFile(own, own$files)
  checkEquals(character(), own$files)
  checkEquals(character(), copy$files)
}

