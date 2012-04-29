# TODO: Add comment
# 
# Author: furia
###############################################################################

unitTestInitialize <-
  function()
{
  fc <- new("FileCache")
  fc2 <- new("FileCache")
  checkTrue(fc$getCacheDir() != fc2$getCacheDir())
}

unitTestNoArgConstructor <-
  function()
{
  fc <- synapseClient:::FileCache()
  checkEquals(length(fc$cacheDir), 1L)
  checkTrue(fc$cacheDir != "")
  checkTrue(!is.null(fc$cacheDir))
  checkTrue(fc$getCacheDir() != fc$getCacheRoot())
  checkEquals(fc$getCacheDir(), file.path(fc$getCacheRoot(), sprintf("%s_unpacked", fc$getArchiveFile())))
  
  checkTrue(file.exists(fc$getCacheDir()))
  checkTrue(file.exists(fc$getCacheRoot()))
  
}

unitTestPathConstructor <-
  function()
{
  fcc <- synapseClient:::FileCache()
  fcc$addFileMetaData("srcFile1", "destFile1")
  fcc$addFileMetaData("srcFile2", "destFile2")
  
  fcc$cacheFileMetaData()
  
  fc <- synapseClient:::FileCache(fcc$cacheRoot)
  ans <- fc$getFileMetaData("destFile1")
  checkEquals(names(ans), "destFile1")
  checkEquals(length(ans), 1L)
  checkEquals(length(ans[[1]]), 1L)
  checkEquals(names(ans[[1]]), "srcPath")
  checkEquals(ans[[1]][[1]], "srcFile1")
  
  checkEquals(names(ans), "destFile1")
  checkEquals(length(ans), 1L)
  checkEquals(length(ans[[1]]), 1L)
  checkEquals(names(ans[[1]]), "srcPath")
  checkEquals(ans[[1]][[1]], "srcFile1")
  
}

unitTestArchiveFileConstructor <-
  function()
{
  archive <- tempfile(fileext=".zip")
  file1 <- tempfile()
  file.create(file1)
  zip(archive, file1)
  
  fc <- synapseClient:::FileCache(archiveFile=archive)
  checkTrue(file.exists(fc$cacheRoot))
  checkTrue(file.exists(file.path(fc$cacheRoot, fc$archiveFile)))
}

