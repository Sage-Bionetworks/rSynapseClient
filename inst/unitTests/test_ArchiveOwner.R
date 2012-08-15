# TODO: Add comment
#
# Author: mfuria
###############################################################################

.setUp <-
    function()
{
  synapseClient:::.setCache("oldWarn", options("warn")[[1]])
  options(warn = 2L)
}

.tearDown <-
    function()
{
  options(warn = synapseClient:::.getCache("oldWarn"))
  if(!is.null(name <- synapseClient:::.getCache("detachMe"))){
    detach(name, character.only = TRUE)
    synapseClient:::.deleteCache('detachMe')
  }
}

unitTestInitialize <-
  function()
{
  aa <- new("ArchiveOwner")
  ab <- new("ArchiveOwner")
  checkTrue(cacheDir(aa) != cacheDir(ab))
}

unitTestAddFile <-
    function()
{
  own <- new("ArchiveOwner")
  file <- gsub("[\\/]+", "/", tempfile())
  cat("THIS IS A TEST %s", Sys.time(), file = file)

  copy <- addFile(own, file)
  checkEquals(length(own@fileCache$files()), 1L)
  checkEquals(length(copy@fileCache$files()), 1L)

  copy <- addFile(own, file, "foo/")
  checkEquals(length(own@fileCache$files()), 2L)
  checkEquals(length(copy@fileCache$files()), 2L)
}

unitTestDeleteFile <-
    function()
{
  own <- new("ArchiveOwner")
  file <- tempfile()
  cat("THIS IS A TEST %s", Sys.time(), file = file)

  addFile(own, file)
  checkEquals(length(own@fileCache$files()), 1L)

  copy <- deleteFile(own, gsub("^.+/", "", basename(file)))
  checkEquals(length(own@fileCache$files()), 0L)
  checkEquals(length(copy@fileCache$files()), 0L)

}

unitTestMoveFile <-
    function()
{
  own <- new("ArchiveOwner")
  file <- tempfile()
  cat("THIS IS A TEST %s", Sys.time(), file = file)

  addFile(own, file)
  checkEquals(length(own@fileCache$files()), 1L)

  copy <- moveFile(own, own@fileCache$files(), "foo.bar")
  checkEquals(length(own@fileCache$files()), 1L)
  checkEquals(own@fileCache$files(), "foo.bar")
}

unitTestLoadObjectsFromFiles <-
    function()
{
  own <- new("ArchiveOwner")
  aMatrix <- diag(10)
  file <- gsub("[\\/]+", "/", tempfile())
  save(aMatrix, file=file)

  addFile(own, file)
  synapseClient:::loadObjectsFromFiles(own)
  checkEquals(length(objects(own@objects)), 0L)
  checkEquals(length(files(own)), 1L)
  checkEquals(files(own), gsub("^.+/","",file))

  moveFile(own,files(own), "file.rbin")
  synapseClient:::loadObjectsFromFiles(own)
  checkEquals(length(objects(own@objects)), 1L)
  checkEquals(length(files(own)), 1L)
  checkEquals(files(own), "file.rbin")
}

#unitTestObjects <-
#    function()
#{
# ## this function isn't implemented
#}

unitTestCacheDir <-
    function()
{
  own <- new("ArchiveOwner")
  checkEquals(cacheDir(own), own@fileCache$getCacheDir())
}

unitTestGetPackageName <-
  function()
{
  own <- new("ArchiveOwner")
  checkTrue(grepl("ArchiveOwner", getPackageName(own)))

  setPackageName("foo", own)
  checkEquals( "foo", getPackageName(own))
}


unitTestAttach <-
  function()
{
  own <- new("ArchiveOwner")

  own@objects$aNum <- 1L
  synapseClient:::.setCache("detachMe", getPackageName(own))
  attach(own)
  checkTrue(getPackageName(own) %in% search())
  checkTrue(objects(getPackageName(own)) == 'aNum')
}


unitTestDetach <-
  function()
{
  own <- new("ArchiveOwner")

  own@objects$aNum <- 1L
  attach(own)
  synapseClient:::.setCache("detachMe", getPackageName(own))
  checkTrue(getPackageName(own) %in% search())
  checkTrue(objects(getPackageName(own)) == 'aNum')
  detach(own)
  synapseClient:::.deleteCache("detachMe")
  checkTrue(!(getPackageName(own) %in% search()))
}

unitTestSetCacheRoot <-
  function()
{
  own <- synapseClient:::ArchiveOwner()
  file <- tempfile()
  cat("TESTFILE1", file = file)

  addFile(own, file)
  checkEquals(length(files(own)), 1L)
  checkEquals(files(own), basename(file))

  own2 <- synapseClient:::ArchiveOwner()

  archfile <- file.path(own@fileCache$getCacheRoot(), own@fileCache$getArchiveFile())
  own2 <- synapseClient:::setCacheRoot(own2, archfile, TRUE)
  checkEquals(length(files(own2)), 0L)

}
