#
#
# Author: mfuria
###############################################################################

.setUp <-
    function()
{
  synapseClient:::.setCache("oldWarn", options("warn")[[1]])
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
  options(warn = 2L)
  aa <- new("ArchiveOwner")
  ab <- new("ArchiveOwner")
  checkTrue(synapseClient:::cacheDir(aa) != synapseClient:::cacheDir(ab))
  options(warn = synapseClient:::.getCache("oldWarn"))
}

unitTestAddFile <-
    function()
{
  options(warn = 2L)
  own <- new("ArchiveOwner")
  file <- gsub("[\\/]+", "/", tempfile())
  cat("THIS IS A TEST %s", Sys.time(), file = file)

  copy <- addFile(own, file)
  checkEquals(length(own@fileCache$files()), 1L)
  checkEquals(length(copy@fileCache$files()), 1L)

  copy <- addFile(own, file, "foo/")
  checkEquals(length(own@fileCache$files()), 2L)
  checkEquals(length(copy@fileCache$files()), 2L)
  options(warn = synapseClient:::.getCache("oldWarn"))
}

unitTestDeleteFile <-
    function()
{
  options(warn = 2L)
  own <- new("ArchiveOwner")
  file <- tempfile()
  cat("THIS IS A TEST %s", Sys.time(), file = file)

  addFile(own, file)
  checkEquals(length(own@fileCache$files()), 1L)

  copy <- deleteFile(own, gsub("^.+/", "", basename(file)))
  checkEquals(length(own@fileCache$files()), 0L)
  checkEquals(length(copy@fileCache$files()), 0L)

  options(warn = synapseClient:::.getCache("oldWarn"))
}

unitTestMoveFile <-
    function()
{
  options(warn = 2L)
  own <- new("ArchiveOwner")
  file <- tempfile()
  cat("THIS IS A TEST %s", Sys.time(), file = file)

  addFile(own, file)
  checkEquals(length(own@fileCache$files()), 1L)

  copy <- moveFile(own, own@fileCache$files(), "foo.bar")
  checkEquals(length(own@fileCache$files()), 1L)
  checkEquals(own@fileCache$files(), "foo.bar")
  options(warn = synapseClient:::.getCache("oldWarn"))
}

unitTestLoadObjectsFromFiles <-
    function()
{
  options(warn = 2L)
  own <- new("ArchiveOwner")
  aMatrix <- diag(10)
  file <- gsub("[\\/]+", "/", tempfile())
  save(aMatrix, file=file)

  addFile(own, file)
  synapseClient:::loadObjectsFromFiles(own, T)
  checkEquals(length(objects(own@objects)), 0L)
  checkEquals(length(synapseClient:::files(own)), 1L)
  checkEquals(synapseClient:::files(own), gsub("^.+/","",file))

  moveFile(own,synapseClient:::files(own), "file.rbin")
  synapseClient:::loadObjectsFromFiles(own, T)
  checkEquals(length(objects(own@objects)), 1L)
  checkEquals(length(synapseClient:::files(own)), 1L)
  checkEquals(synapseClient:::files(own), "file.rbin")
  options(warn = synapseClient:::.getCache("oldWarn"))
}

#unitTestObjects <-
#    function()
#{
# ## this function isn't implemented
#}

unitTestCacheDir <-
    function()
{
  options(warn = 2L)
  own <- new("ArchiveOwner")
  checkEquals(synapseClient:::cacheDir(own), own@fileCache$getCacheDir())
  options(warn = synapseClient:::.getCache("oldWarn"))
}

unitTestGetPackageName <-
  function()
{
  options(warn = 2L)
  own <- new("ArchiveOwner")
  checkTrue(grepl("ArchiveOwner", getPackageName(own)))

  setPackageName("foo", own)
  checkEquals( "foo", getPackageName(own))
  options(warn = synapseClient:::.getCache("oldWarn"))
}


unitTestAttach <-
  function()
{
  options(warn = 2L)
  own <- new("ArchiveOwner")

  own@objects$aNum <- 1L
  synapseClient:::.setCache("detachMe", getPackageName(own))
  attach(own)
  checkTrue(getPackageName(own) %in% search())
  checkTrue(objects(getPackageName(own)) == 'aNum')
  options(warn = synapseClient:::.getCache("oldWarn"))
}


unitTestDetach <-
  function()
{
  options(warn = 2L)
  own <- new("ArchiveOwner")

  own@objects$aNum <- 1L
  attach(own)
  synapseClient:::.setCache("detachMe", getPackageName(own))
  checkTrue(getPackageName(own) %in% search())
  checkTrue(objects(getPackageName(own)) == 'aNum')
  detach(own)
  synapseClient:::.deleteCache("detachMe")
  checkTrue(!(getPackageName(own) %in% search()))
  options(warn = synapseClient:::.getCache("oldWarn"))
}

unitTestSetCacheRoot <-
  function()
{
  options(warn = 2L)
  own <- synapseClient:::ArchiveOwner()
  copy <- own
  file <- tempfile()
  cat("TESTFILE1", file = file)

  addFile(own, file)
  checkEquals(length(synapseClient:::files(own)), 1L)
  checkEquals(synapseClient:::files(own), basename(file))
  checkEquals(length(synapseClient:::files(copy)), 1L)

  newroot <- tempfile()
  synapseClient:::setCacheRoot(own, newroot, TRUE)
  checkEquals(length(synapseClient:::files(own)), 1L)
  checkEquals(synapseClient:::files(own), synapseClient:::files(copy))

  deleteFile(own, synapseClient:::files(own))
  checkEquals(length(synapseClient:::files(own)), 0L)
  checkEquals(length(synapseClient:::files(copy)), 0L)

  addFile(own, file)
  checkEquals(length(synapseClient:::files(own)), 1L)
  checkEquals(synapseClient:::files(own), basename(file))
  checkEquals(length(synapseClient:::files(copy)), 1L)

  own2 <- synapseClient:::ArchiveOwner()

  archfile <- file.path(own@fileCache$getCacheRoot(), own@fileCache$getArchiveFile())
  own2 <- synapseClient:::setCacheRoot(own2, archfile, TRUE)
  checkEquals(length(synapseClient:::files(own2)), 0L)
  options(warn = synapseClient:::.getCache("oldWarn"))
}
