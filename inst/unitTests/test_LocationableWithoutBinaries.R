#
#
# Author: furia
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
#  detach("testEnv")
  options(warn = synapseClient:::.getCache("oldWarn"))
  if(!is.null(name <- synapseClient:::.getCache("detachMe"))){
    detach(name, character.only = TRUE)
    synapseClient:::.deleteCache('detachMe')
  }
}

unitTestAddFile <-
  function()
{
  own <- new("LocationableWithoutBinaries")

  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", sample(10000,1)), file = file)
  copy <- addFile(own, file)
  checkEquals(basename(file), own$files)
  checkEquals(basename(file), copy$files)

  ## make sure the cache re-initializes but running the exact same
  ## test again
  own <- new("LocationableWithoutBinaries")
  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", sample(10000,1)), file = file)
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
  own <- new("LocationableWithoutBinaries")

  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", sample(10000,1)), file = file)
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
  own <- new("LocationableWithoutBinaries")

  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", sample(10000,1)), file = file)
  addFile(own, file, "aFile.txt")
  checkEquals("aFile.txt", own$files)

  copy <- deleteFile(own, own$files)
  checkEquals(character(), own$files)
  checkEquals(character(), copy$files)
}

unitTestDoubleBracketAccessor <-
    function()
{
  own <- new("LocationableWithoutBinaries")

  checkEquals(own[['files']], character())
  emptyNamedList<-structure(list(), names = character()) # copied from RJSONIO
  checkEquals(own[['objects']][], emptyNamedList)
  checkEquals(own[['cacheDir']], own$cacheDir)
}

unitTestBracketAccessor <-
    function()
{
  own <- new("LocationableWithoutBinaries")

  checkEquals(names(own['files']), 'files')
  checkEquals(names(own['objects']), 'objects')
  checkEquals(names(own['cacheDir']), 'cacheDir')
  checkEquals(own['files'][[1]], character())
  checkEquals(length(own['objects'][[1]]), 0L)
  checkEquals(own['cacheDir'][[1]],  own$cacheDir)
}

unitTestAttach <-
  function()
{
  ee <- new("LocationableWithoutBinaries")

  ee@archOwn@objects$aNum <- 1L
  attach(ee)
  synapseClient:::.setCache("detachMe", getPackageName(ee@archOwn))
  checkTrue(getPackageName(ee@archOwn) %in% search())
  checkTrue(objects(getPackageName(ee@archOwn)) == 'aNum')
}

unitTestDetach <-
  function()
{
  ee <- new("LocationableWithoutBinaries")

  ee@archOwn@objects$aNum <- 1L
  attach(ee)
  synapseClient:::.setCache("detachMe", getPackageName(ee@archOwn))
  checkTrue(getPackageName(ee@archOwn) %in% search())
  detach(ee)
  synapseClient:::.deleteCache("detachMe")
  checkTrue(!(getPackageName(ee@archOwn) %in% search()))
}


unitTestPackageName <-
  function()
{
  ee <- new("LocationableWithoutBinaries")
  checkTrue(grepl("^LocationableWithoutBinaries.+", getPackageName(ee@archOwn)))
}
