# TODO: Add comment
# 
# Author: furia
###############################################################################

unitTestChangeRoot <-
  function()
{
  cacheRoot <- tempfile()
  dir.create(cacheRoot)
  cacheRoot <- normalizePath(cacheRoot)
  archive <- tempfile(tmpdir=cacheRoot,fileext=".zip")
  archiveFile <- basename(archive)
  file1 <- tempfile()
  cat(sprintf("THIS IS A TEST: %s", Sys.time()), file = file1)
  olddir <- getwd()
  setwd(tempdir())
  suppressWarnings(zip(archive, files = gsub("^.+/", "", file1)))
  setwd(olddir)
  fc <- synapseClient:::FileCache(archiveFile=archive)
  
  checkEquals(archiveFile, fc$archiveFile)
  cacheRoot <- gsub("[\\/]+", "/", cacheRoot)
  checkEquals(cacheRoot, fc$cacheRoot)
  
  checkEquals(file.path(cacheRoot, sprintf("%s_unpacked", archiveFile)), fc$cacheDir)
  
  ans <- fc$unpackArchive()
  
  oldCacheDir <- fc$cacheDir
  oldMD5 <- as.character(tools::md5sum(file.path(fc$cacheRoot, fc$archiveFile)))
  ## change the cacheRoot
  newRoot <- tempfile()
  checkTrue(!file.exists(newRoot))
  synapseClient:::setCacheRoot(fc, newRoot)

  newRoot <- gsub("[\\/]+", "/", normalizePath(newRoot))

  checkEquals(newRoot, fc$cacheRoot)
  checkTrue(oldCacheDir != fc$cacheDir)
  checkEquals(file.path(fc$cacheRoot, sprintf("%s_unpacked", fc$archiveFile)), fc$cacheDir )
  checkTrue(all(dir(fc$cacheDir) %in% dir(oldCacheDir)))
  checkTrue(all(dir(cacheRoot) %in% dir(fc$cacheRoot)))
  checkTrue(file.exists(fc$cacheDir))
  
  checkEquals(as.character(tools::md5sum(file.path(fc$cacheRoot, fc$archiveFile))), oldMD5)
  
  ## now unpack
  unlink(fc$cacheDir, force=T, recursive=T)
  fc$unpackArchive()
  
  
  checkEquals(newRoot, fc$cacheRoot)
  checkTrue(oldCacheDir != fc$cacheDir)
  checkEquals(file.path(fc$cacheRoot, sprintf("%s_unpacked", fc$archiveFile)), fc$cacheDir )
  checkTrue(all(dir(fc$cacheDir) %in% dir(oldCacheDir)))
  checkTrue(all(dir(cacheRoot) %in% dir(fc$cacheRoot)))
  checkTrue(file.exists(fc$cacheDir))
  
  root2 <- fc$cacheRoot
  ## now set the cache root and clean
  ## change the cacheRoot
  newRoot <- tempfile()
  checkTrue(!file.exists(newRoot))
  synapseClient:::setCacheRoot(fc, newRoot, clean=T)

  newRoot <- gsub("[\\/]+", "/", normalizePath(newRoot))
  
  checkEquals(newRoot, fc$cacheRoot)
  checkTrue(oldCacheDir != fc$cacheDir)
  checkEquals(file.path(fc$cacheRoot, sprintf("%s_unpacked", fc$archiveFile)), fc$cacheDir )
  checkTrue(all(dir(fc$cacheDir) %in% dir(oldCacheDir)))
  checkTrue(all(dir(cacheRoot) %in% dir(fc$cacheRoot)))
  checkTrue(file.exists(fc$cacheDir))
  
  checkEquals(as.character(tools::md5sum(file.path(fc$cacheRoot, fc$archiveFile))), oldMD5)
  
  checkTrue(!file.exists(root2))
  
}