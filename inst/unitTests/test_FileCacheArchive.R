# TODO: Add comment
# 
# Author: furia
###############################################################################


unitTestCreateArchive <-
  function()
{
  fc <- FileCache()
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
  archive <- tempfile(fileext=".zip")
  archiveFile <- gsub("^.+/", "", archive)
  cacheRoot <- sprintf("%s/", gsub("/+","/", normalizePath(tempdir())))
  file1 <- tempfile()
  cat(sprintf("THIS IS A TEST: %s", Sys.time()), file = file1)
  suppressWarnings(zip(archive, files = file1))
  
  fc <- FileCache(archiveFile=archive)
  
  checkEquals(archiveFile, fc$archiveFile)
  checkEquals(cacheRoot, fc$cacheRoot)

  checkEquals(file.path(cacheRoot, sprintf("%s_unpacked", archiveFile)), fc$cacheDir)
  
#  ans <- fc$unpackArchive()
#  checkTrue(file.exists(ans))
  
}

