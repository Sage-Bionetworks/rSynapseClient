# These are the test_Locationable tests adapted for Files
#
###############################################################################

.setUp <-
    function()
{
  synapseClient:::.setCache("oldWarn", options("warn")[[1]])
  options(warn=2L)
}

.tearDown <-
    function()
{
  options(warn = synapseClient:::.getCache("oldWarn"))
  if(!is.null(name <- synapseClient:::.getCache("detachMe"))){
    detach(name)
    synapseClient:::.deleteCache('detachMe')
  }
}

unitTestAddObject <-
    function()
{
  own <- new("File")

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
  copy <- synapseClient:::loadObjectsFromFiles(own, T)
  checkEquals("character", as.character(class(own$objects$foo)))
  checkEquals("character", as.character(class(copy$objects$foo)))

  ## now rename the "objects" foo so it will no longer be masked
  copy <- renameObject(own, "foo", "goo")
  checkEquals(2L, length(names(own$objects)))
  checkTrue(all(names(own$objects) == c("foo", "goo")))
  checkEquals(2L, length(names(copy$objects)))
  checkTrue(all(names(copy$objects) == c("foo", "goo")))
  checkEquals(1L, length(own$files))
  checkEquals(1L, length(copy$files))

}

unitTestAddObjectRename <-
    function()
{
  own <- new("File")

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
  own <- new("File")

  foo <- list(b="a", one=1)
  copy <- addObject(own, foo, "bar")
  checkEquals(length(names(own$objects)), 1L)
  checkEquals(names(own$objects), "bar")
  checkEquals(as.character(class(own$objects$bar)), "list")
  checkEquals(length(names(copy$objects)), 1L)
  checkEquals(names(copy$objects), "bar")

  own <- new("File")
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
  own <- new("File")
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
}

unitTestAddListUnlist <-
    function()
{
  own <- new("File")
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

}

unitTestDeleteObject <-
    function()
{
  own <- new("File")
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
  own <- new("File")
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
  own <- new("File")
  foo <- "boo"
  addObject(own, foo)
  checkEquals(getObject(own, "foo"), "boo")
}

unitTestAddFile <-
  function()
{
  own <- new("File")

  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", Sys.time()), file = file)
  copy <- addFile(own, file)
  checkEquals(basename(file), own$files)
  checkEquals(basename(file), copy$files)

  ## add an object and make sure the object file doesn't show up
  addObject(own, "foo", "bar")
  checkEquals(1L, length(own$files))
  checkEquals(1L, length(files(own)))
  checkEquals(basename(file), own$files)

  ## make sure the cache re-initializes but running the exact same
  ## test again
  own <- new("SynapseLocationOwner")
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

unitTestAddFile <-
    function()
{
  own <- new("File")

  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", Sys.time()), file = file)
  copy <- addFile(own, file)
  checkEquals(basename(file), own$files)
  checkEquals(basename(file), copy$files)

  ## make sure the cache re-initializes but running the exact same
  ## test again
  own <- new("File")
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

uniTestDeleteFile <-
    function()
{
  own <- new("File")

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



unitTestAddFile <-
  function()
{
  own <- new("File")
  
  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", Sys.time()), file = file)
  copy <- addFile(own, file)
  checkEquals(basename(file), own$files)
  checkEquals(basename(file), copy$files)
  
  ## make sure the cache re-initializes but running the exact same
  ## test again
  own <- new("File")
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

uniTestDeleteFile <-
  function()
{
  own <- new("File")
  
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

unitTestDoubleBracketAccessor <-
  function()
{
  own <- new("File")
  
  checkEquals(own[['files']], character())
  checkEquals(own[['objects']][], RJSONIO::emptyNamedList)
  checkEquals(own[['cacheDir']], own$cacheDir)
}

unitTestBracketAccessor <-
  function()
{
  own <- new("File")
  
  checkEquals(names(own['files']), 'files')
  checkEquals(names(own['objects']), 'objects')
  checkEquals(names(own['cacheDir']), 'cacheDir')
  checkEquals(own['files'][[1]], character())
  checkEquals(length(own['objects'][[1]]), 0L)
  checkEquals(own['cacheDir'][[1]],  own$cacheDir)
}

