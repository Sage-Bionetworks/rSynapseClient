# TODO: Add comment
# 
# Author: furia
###############################################################################

unitTestAddMetaData <-
  function()
{
  fc <- new("FileCache")
  fc$addFileMetaData("srcFile1", "destFile1")
  
  ans <- fc$getFileMetaData("destFile1")
  checkEquals(names(ans), "destFile1")
  checkEquals(length(ans), 1L)
  checkEquals(length(ans[[1]]), 1L)
  checkEquals(names(ans[[1]]), "srcPath")
  checkEquals(ans[[1]][[1]], "srcFile1")
  
  fc$addFileMetaData("srcFile2", "destFile2")
  ans <- fc$getFileMetaData("destFile1")
  checkEquals(names(ans), "destFile1")
  checkEquals(length(ans), 1L)
  checkEquals(length(ans[[1]]), 1L)
  checkEquals(names(ans[[1]]), "srcPath")
  checkEquals(ans[[1]][[1]], "srcFile1")
  
  ans <- fc$getFileMetaData("destFile2")
  checkEquals(names(ans), "destFile2")
  checkEquals(length(ans), 1L)
  checkEquals(length(ans[[1]]), 1L)
  checkEquals(names(ans[[1]]), "srcPath")
  checkEquals(ans[[1]][[1]], "srcFile2")
  
  ans <- fc$getFileMetaData()
  checkTrue(all(names(ans) == c("destFile1", "destFile2")))
  checkEquals(length(ans), 2L)
  checkEquals(length(ans[[1]]), 1L)
  checkEquals(names(ans[[1]]), "srcPath")
  checkEquals(ans[[1]][[1]], "srcFile1")
  
  checkEquals(length(ans[[2]]), 1L)
  checkEquals(names(ans[[2]]), "srcPath")
  checkEquals(ans[[2]][[1]], "srcFile2")
}

unitTestDeleteMetaData <-
  function()
{
  fc <- new("FileCache")
  fc$addFileMetaData("srcFile1", "destFile1")
  fc$addFileMetaData("srcFile2", "destFile2")
  
  fc$deleteFileMetaData("destFile1")
  checkEquals(length(fc$getFileMetaData()), 1L)
  fc$deleteFileMetaData("destFile1")
  checkEquals(length(fc$getFileMetaData()), 1L)
  
  checkEquals(names(fc$getFileMetaData()), "destFile2")
  
  fc$deleteFileMetaData("destFile2")
  checkEquals(length(fc$getFileMetaData()), 0L)
  
  fc$deleteFileMetaData()
  checkEquals(length(fc$getFileMetaData()), 0L)
  fc$addFileMetaData("srcFile1", "destFile1")
  fc$addFileMetaData("srcFile2", "destFile2")
  fc$deleteFileMetaData()
  checkEquals(length(fc$getFileMetaData()), 0L)
}

unitTestCacheMetaData <-
  function()
{
  fc <- new("FileCache")
  fc$addFileMetaData("srcFile1", "destFile1")
  fc$addFileMetaData("srcFile2", "destFile2")
  
  fc$cacheFileMetaData()
  fc$deleteFileMetaData()
  
  fc$loadMetaDataFromFile()
  
  ans <- fc$getFileMetaData("destFile1")
  checkEquals(names(ans), "destFile1")
  checkEquals(length(ans), 1L)
  checkEquals(length(ans[[1]]), 1L)
  checkEquals(names(ans[[1]]), "srcPath")
  checkEquals(ans[[1]][[1]], "srcFile1")
  
  ans <- fc$getFileMetaData("destFile2")
  checkEquals(names(ans), "destFile2")
  checkEquals(length(ans), 1L)
  checkEquals(length(ans[[1]]), 1L)
  checkEquals(names(ans[[1]]), "srcPath")
  checkEquals(ans[[1]][[1]], "srcFile2")
  
  ans <- fc$getFileMetaData()
  checkTrue(all(names(ans) == c("destFile1", "destFile2")))
  checkEquals(length(ans), 2L)
  checkEquals(length(ans[[1]]), 1L)
  checkEquals(names(ans[[1]]), "srcPath")
  checkEquals(ans[[1]][[1]], "srcFile1")
  
  checkEquals(length(ans[[2]]), 1L)
  checkEquals(names(ans[[2]]), "srcPath")
  checkEquals(ans[[2]][[1]], "srcFile2")
  
}

unitTestLoadMetaDataFromFile <-
  function()
{
  fc <- new("FileCache")
  
  ## load data when no metaData file is present
  fc$loadMetaDataFromFile()
  checkEquals(length(fc$getFileMetaData()), 0L)
  
  fc$addFileMetaData("srcFile1", "destFile1")
  fc$addFileMetaData("srcFile2", "destFile2")
  fc$loadMetaDataFromFile()
  checkEquals(length(fc$getFileMetaData()), 0L)
  
}

unitTestCacheMetaData <-
  function()
{
  fc <- new("FileCache")
  fc$cacheFileMetaData()
  
  fc$loadMetaDataFromFile()
  checkEquals(length(fc$getFileMetaData()), 0L)
  
  fc$addFileMetaData("srcFile1", "destFile1")
  fc$addFileMetaData("srcFile2", "destFile2")
  
  fc$loadMetaDataFromFile()
  checkEquals(length(fc$getFileMetaData()), 0L)
  
}

unitTestDeleteMetaDataCache <-
  function()
{
  fc <- new("FileCache")
  
  fc$addFileMetaData("srcFile1", "destFile1")
  fc$addFileMetaData("srcFile2", "destFile2")
  fc$cacheFileMetaData()
  
  fc$loadMetaDataFromFile()
  checkEquals(length(fc$getFileMetaData()), 2L)
  
  fc$deleteMetaDataCache()
  fc$loadMetaDataFromFile()
  checkEquals(length(fc$getFileMetaData()), 0L)
}

unitTestAddMetaDataElipsis <-
  function()
{
  fc <- new("FileCache")
  
  fc$addFileMetaData("srcFile1", "destFile1", status="new", format="doc")
  ans <- fc$getFileMetaData("destFile1")
  checkTrue(all(names(ans[[1]]) == c("srcPath", "status", "format")))
  checkEquals(ans[[1]][["status"]], "new")
  checkEquals(ans[[1]][["format"]], "doc")
  checkEquals(ans[[1]][["srcPath"]], "srcFile1")
  
  fc$deleteFileMetaData()
  fc$addFileMetaData("srcFile1", "destFile1", foo=list(status="new", format="doc"))
  ans <- fc$getFileMetaData("destFile1")
  checkTrue(all(names(ans[[1]]) == c("srcPath", "foo")))
  checkEquals(ans[[1]][['foo']][["status"]], "new")
  checkEquals(ans[[1]][['foo']][["format"]], "doc")
  checkEquals(ans[[1]][["srcPath"]], "srcFile1")
  
  ## try caching and reading back
  fc$cacheFileMetaData()
  fc$deleteFileMetaData()
  fc$loadMetaDataFromFile()
  ans <- fc$getFileMetaData("destFile1")
  checkTrue(all(names(ans[[1]]) == c("srcPath", "foo")))
  checkEquals(ans[[1]][['foo']][["status"]], "new")
  checkEquals(ans[[1]][['foo']][["format"]], "doc")
  checkEquals(ans[[1]][["srcPath"]], "srcFile1")
  
}

unitTestNoArgConstructor <-
  function()
{
  fc <- FileCache()
  checkEquals(length(fc$cacheDir), 1L)
  checkTrue(fc$cacheDir != "")
  checkTrue(!is.null(fc$cacheDir))
  fc$addFileMetaData("srcFile1", "destFile1")
  fc$addFileMetaData("srcFile2", "destFile2")
}

unitTestPathConstructor <-
  function()
{
  fcc <- FileCache()
  fcc$addFileMetaData("srcFile1", "destFile1")
  fcc$addFileMetaData("srcFile2", "destFile2")
  
  fcc$cacheFileMetaData()
  
  fc <- FileCache(fcc$cacheRoot)
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

unitTestAddFileInfo <-
  function()
{
  fc <- new("FileCache")
  srcFile <- tempfile()
  cat(sprintf("THIS IS A TEST %s", Sys.time()), file = srcFile)
  
  addFileMetaData(fc, srcFile, file.path(fc$cacheDir, "/foo/bar.txt"))
  
}

unitTestAddFileInfoTwiceSameNameDifferentSlashes <-
  function()
{
  fc <- new("FileCache")
  s1 <- tempfile()
  s2 <- tempfile()
  
  path1 <- file.path(fc$cacheDir, "/foo/bar")
  path2 <- file.path(fc$cacheDir, "///foo\\\bar")
  addFileMetaData(fc, s1, path1)
  checkEquals(length(fc$metaData), 1L)
  addFileMetaData(fc, s2, path1)
  checkEquals(length(fc$metaData), 1L)
  
  ## check that it works properly when the directory exists
  fc <- new("FileCache")
  dir.create(file.path(fc$cacheDir, "foo/bar"), recursive = TRUE)
  path1 <- file.path(fc$cacheDir, "/foo/bar/")
  path2 <- file.path(fc$cacheDir, "///foo\\\bar")
  addFileMetaData(fc, s1, path1)
  checkEquals(length(fc$metaData), 1L)
  addFileMetaData(fc, s2, path1)
  checkEquals(length(fc$metaData), 2L)
  checkTrue(file.remove(file.path(fc$cacheDir, "foo/bar")))
  checkTrue(file.remove(file.path(fc$cacheDir, "foo")))
}


unitTestAddFileToExistingDirectory <-
  function()
{
  fc <- new("FileCache")
  checkTrue(dir.create(file.path(fc$cacheDir, "aDir"), recursive = TRUE))
  s1 <- tempfile()
  cat(sprintf("THIS IS ANOTHER TEST%s", Sys.time()), file = s1)
  
  addFileMetaData(fc, s1, "aDir")
  addFileMetaData(fc, s1, "/aDir")
  addFileMetaData(fc, s1, "aDir/")
  checkEquals(length(fc$metaData), 1L)
  
  srcFname <- gsub(tempdir(), "", s1, fixed = TRUE)
  srcFname <- gsub("[\\/]+", "", srcFname)
  checkTrue(grepl(sprintf("%s$", srcFname), names(fc$metaData)))
}

unitTestAddMetaDataByPassingDirectory <-
  function()
{
  fc <- new("FileCache")
  srcDir <- tempfile()
  checkTrue(dir.create(srcDir, recursive = TRUE))
  s1 <- tempfile(tmpdir = srcDir)
  cat(sprintf("test one %s", Sys.time()), file=s1)
  s2 <- tempfile(tmpdir = srcDir)
  cat(sprintf("test two %s", Sys.time()), file=s2)
  
  checkException(addFileMetaData(fc, srcDir, "/"))
  
}

unitTestAddMetaDataForRootDirContents <-
  function()
{
  fc <- new("FileCache")
  checkTrue(dir.create(file.path(fc$cacheDir, "aDir"), recursive = TRUE))
  s1 <- tempfile()
  cat(sprintf("THIS IS ANOTHER TEST%s", Sys.time()), file = s1)
  
  addFileMetaData(fc, s1)
  addFileMetaData(fc, s1, "//")
  addFileMetaData(fc, s1, "\\")
  checkEquals(length(fc$metaData), 1L)
  
  srcFname <- gsub(tempdir(), "", s1, fixed = TRUE)
  srcFname <- gsub("[\\/]+", "", srcFname)
  checkTrue(grepl(sprintf("%s$", srcFname), names(fc$metaData)))
}


unitTestPassByReference <-
  function()
{
  fc <- new("FileCache")
  fc.copy <- fc
  
  fc$addFileMetaData("srcFile1", "destFile1")
  checkEquals(length(fc.copy$metaData), 1L)
  checkEquals(names(fc.copy$metaData), names(fc$metaData))
  
  
}
