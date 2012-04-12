## Unit tests for deleting entity files
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

unitTestDeleteFile <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/foo/bar"
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  checkTrue(length(fc$files()) == 1L)
  
  deleteFile(fc, fc$files()[1L])
  checkTrue(length(fc$files()) == 0L)
  checkEquals(length(dir(fc$cacheDir)), 0L)
}

unitTestCleanUp <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/foo/bar"
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  checkTrue(length(fc$files()) == 1L)
  
  file2 <- tempfile()
  d <- diag(x=2, nrow=10, ncol=10)
  save(d, file=file2)
  path2 <- "/foo"
  addFile(fc, file2, path2)
  checkTrue(length(fc$files()) == 2L)
  
  file3 <- tempfile()
  d <- diag(x=3, nrow=10, ncol=10)
  save(d, file=file3)
  path3 <- "/foo"
  addFile(fc, file3, path3)
  checkTrue(length(fc$files()) == 3L)
  
  checkTrue(all(file.exists(file.path(fc$cacheDir, fc$files()))))
  
  deleteFile(fc, fc$files()[1])
  checkTrue(length(fc$files()) == 2L)
  checkTrue(all(file.exists(file.path(fc$cacheDir, fc$files()))))
  
  checkTrue(!file.exists(file.path(fc$cacheDir, path, gsub("^.+[\\/]+", "", file))))
  checkTrue(!file.exists(file.path(fc$cacheDir, path)))
  
  deleteFile(fc, fc$files()[1])
  checkTrue(length(fc$files()) == 1L)
  checkTrue(all(file.exists(file.path(fc$cacheDir, fc$files()))))
  checkTrue(!file.exists(file.path(fc$cacheDir, path2, gsub("^.+[\\/]+", "", file2))))
  checkTrue(file.exists(file.path(fc$cacheDir, path2)))
  
  deleteFile(fc, fc$files()[1])
  checkTrue(length(fc$files()) == 0L)
  checkTrue(length(file.exists(file.path(fc$cacheDir, fc$files()))) == 0)
  checkTrue(!file.exists(file.path(fc$cacheDir, path3, gsub("^.+[\\/]+", "", file3))))
  checkTrue(!file.exists(file.path(fc$cacheDir, path3)))
}


unitTestDeleteFileFromRoot <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/"
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  checkTrue(length(fc$files()) == 1L)
  checkEquals(checksum, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[1]))))
  
  oldPath <- fc$files()[1]
  layer <- deleteFile(fc, fc$files()[1])
  checkEquals(length(fc$files()), 0L)
  checkTrue(!file.exists(file.path(fc$cacheDir, oldPath)))
}

unitTestReturnValue <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/foo/bar"
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  checkTrue(length(fc$files()) == 1L)
  
  fc <- deleteFile(fc, fc$files()[1L])
  checkTrue(length(fc$files()) == 0L)
  checkEquals(length(dir(fc$cacheDir)), 0L)
}


