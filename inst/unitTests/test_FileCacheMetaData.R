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

#unitTestConstructors <-
#  function()
#{
#  
#}
