# TODO: Add comment
# 
# Author: furia
###############################################################################

.tearDown <- function(){
  ## clear out the file cache factory
  resetFactory(new("FileCacheFactory"))  
 }

unitTestNoArg <- function(){
  fc <- getFileCache()
  checkEquals(fc$archiveFile, "archive.zip")
  checkEquals(as.character(class(fc)), "FileCache")
}

unitTestNewFileCacheInvalidFile <-
  function()
{
  archivefile <- tempfile(fileext=".zip")
  checkException(getFileCache(archivefile))
  
  archivefile <- tempfile(fileext=".bz")
  checkException(getFileCache(archivefile))
  
  archivefile <- tempfile(fileext=".TAR")
  checkException(getFileCache(archivefile))
  
  archivefile <- tempfile(fileext="tar.gz")
  checkException(getFileCache(archivefile))
  
  archivefile <- tempfile(fileext=".ZIP")
  checkException(getFileCache(archivefile)) 
}

unitTestExistingCacheZipNotInFactory <-
  function()
{
  cacheRoot <- tempfile()
  dir.create(cacheRoot)
  cacheRoot <- normalizePath(cacheRoot)
  archivefile <- tempfile(tmpdir=cacheRoot, fileext=".zip")
  file <- tempfile()
  file.create(file)
  suppressWarnings(
    zip(archivefile, files = file)
  )
  
  fc <- getFileCache(archivefile)
  checkEquals(fc$cacheRoot, cacheRoot)
  checkEquals(length(availFileCaches()), 1L)
  checkEquals(availFileCaches(), cacheRoot)
  
  fc.copy <- getFileCache(fc$cacheRoot)
  checkEquals(fc.copy$cacheRoot, fc$cacheRoot)
  checkEquals(fc.copy$files(), fc$files())
  checkEquals(fc.copy$archiveFile, fc.copy$archiveFile)
  
  fc.copy <- getFileCache(archivefile)
  checkEquals(fc.copy$cacheRoot, fc$cacheRoot)
  checkEquals(fc.copy$files(), fc$files())
  checkEquals(fc.copy$archiveFile, fc.copy$archiveFile)
  
  checkEquals(length(availFileCaches()), 1L)
  
}

unitTestNewArchiveNoArg <-
  function()
{
  fc <- getFileCache()
  checkEquals(as.character(class(fc)), "FileCache")
  checkEquals(length(availFileCaches()), 0L)
}

unitTestExistingRootDirNotInFactory <-
  function()
{
  fc <- getFileCache()
  file <- tempfile()
  cat(sprintf("Testing...1 %s", Sys.time()), file = file)
  addFile(fc, file)
  fc$archiveFile <- "foo.bar.zip"
  fc$cacheFileMetaData()
  
  fc.copy <- getFileCache(fc$cacheRoot)
  checkEquals(fc.copy$cacheRoot, fc$cacheRoot)
  checkEquals(fc.copy$files(), fc$files())
  checkEquals(fc.copy$archiveFile, fc.copy$archiveFile)
}

unitTestExistingSingleFileNotInFactory <-
  function()
{
  cacheRoot <- tempfile()
  dir.create(cacheRoot)
  cacheRoot <- gsub("/+", "/", normalizePath(cacheRoot))
  archivefile <- tempfile(tmpdir=cacheRoot, fileext=".zip")
  file <- tempfile()
  file.create(file)
  suppressWarnings(
    zip(archivefile, files = file)
  )
  
  fc <- getFileCache(archivefile)
  checkEquals(fc$cacheRoot, cacheRoot)
  checkEquals(length(availFileCaches()), 1L)
  checkEquals(availFileCaches(), cacheRoot)
}

unitTestSingleFileNotInFactory <-
  function()
{
  cacheRoot <- tempfile()
  dir.create(cacheRoot)
  cacheRoot <- normalizePath(cacheRoot)
  file <- tempfile(tmpdir=cacheRoot)
  cat(sprintf("Testing...1 %s", Sys.time()), file = file)
  
  fc <- getFileCache(file)
  checkEquals(fc$cacheDir, file.path(sprintf("%s_unpacked", file)))
}





