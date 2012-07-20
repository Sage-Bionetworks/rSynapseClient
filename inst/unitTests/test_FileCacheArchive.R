# TODO: Add comment
# 
# Author: furia
###############################################################################


unitTestCreateArchive <-
  function()
{
  fc <- synapseClient:::FileCache()
  file1 <- tempfile()
  cat(sprintf("THIS IS A TEST: %s", Sys.time()), file = file1)
  addFile(fc, file1)
  ans <- fc$createArchive()
  checkEquals(ans, fc$archiveFile)
  checkTrue(file.exists(file.path(fc$cacheRoot, fc$archiveFile)))
}

unitTestUnpackArchive <-
  function()
{
  cacheRoot <- tempfile()
  dir.create(cacheRoot)
  cacheRoot <- normalizePath(cacheRoot)
  cacheRoot <- gsub("[\\/]+", "/", cacheRoot)
  archive <- tempfile(tmpdir=cacheRoot,fileext=".zip")
  archive <- gsub("[\\/]+", "/", archive)
  archiveFile <- gsub("^.+/", "", archive)
  file1 <- tempfile()
  file1 <- gsub("[\\/]+", "/", file1)
  cat(sprintf("THIS IS A TEST: %s", Sys.time()), file = file1)
  olddir <- getwd()
  setwd(tempdir())
  suppressWarnings(zip(archive, files = gsub("^.+/", "", file1)))
  setwd(olddir)
  fc <- synapseClient:::FileCache(archiveFile=archive)
  
  checkEquals(archiveFile, fc$archiveFile)
  checkEquals(cacheRoot, fc$cacheRoot)

  checkEquals(file.path(cacheRoot, sprintf("%s_unpacked", archiveFile)), fc$cacheDir)
  
  ans <- fc$unpackArchive()
  checkTrue(file.exists(ans))
  checkTrue(file.exists(file.path(ans,gsub("^.+/", "", file1))))
  
  checkEquals(length(fc$getFileMetaData()), 1L)
  checkEquals(length(fc$files()), 1L)
  checkEquals(fc$getFileMetaData()[[1]]$srcPath, fc$archiveFile)
  checkEquals(names(fc$getFileMetaData()) , gsub("/+", "/", file.path(fc$cacheDir, fc$files())))
}


## when there are no files, creating the archive should delete the archive file, if it exists
unitTestCreateArchiveDeleteLastFile <-
  function()
{
  fc <- synapseClient:::FileCache()
  file1 <- tempfile()
  cat(sprintf("THIS IS A TEST: %s", Sys.time()), file = file1)
  addFile(fc, file1)
  ans <- fc$createArchive()
  checkEquals(ans, fc$archiveFile)
  checkTrue(file.exists(file.path(fc$cacheRoot, fc$archiveFile)))
  
  deleteFile(fc, fc$files()[1L])
  checkTrue(length(fc$files()) == 0L)
  checkEquals(length(dir(fc$cacheDir)), 0L)
  
  ans <- fc$createArchive()
  checkTrue(is.null(ans))
  checkTrue(!file.exists(file.path(fc$cacheRoot, fc$archiveFile)))
  
}

unitTestCreateArchiveNoFiles <-
  function()
{
  fc <- synapseClient:::FileCache()
  ans <- fc$createArchive()
  checkTrue(is.null(ans))
  checkTrue(!file.exists(file.path(fc$cacheRoot, fc$archiveFile)))
}

