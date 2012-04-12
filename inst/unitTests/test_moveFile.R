## Unit tests for moving entity files
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

unitTestMoveFileNewPathDirExists <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/foo/bar"
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  checkTrue(length(fc$files()) == 1L)
  checkEquals(checksum, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[1]))))
  
  checkException(moveFile(fc, fc$files()[1], "/foo"))
  
}

unitTestMoveFileNewPath <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/foo/bar"
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  checkTrue(length(fc$files()) == 1L)
  checkEquals(checksum, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[1]))))
  
  moveFile(fc, fc$files()[1], "/foo/")
  checkTrue(length(fc$files()) == 1L)
  checkEquals(fc$files()[1], file.path("foo", gsub("^.+[\\\\/]+","", file)))
  
  moveFile(fc, fc$files()[1], "/")
  checkTrue(length(fc$files()) == 1L)
  checkEquals(fc$files()[1], file.path(gsub("^.+[\\\\/]+","", file)))
  
  moveFile(fc, fc$files()[1], "/foo/")
  checkTrue(length(fc$files()) == 1L)
  checkEquals(fc$files()[1], file.path("foo", gsub("^.+[\\\\/]+","", file)))
  
  moveFile(fc, fc$files()[1], "")
  checkTrue(length(fc$files()) == 1L)
  checkEquals(fc$files()[1], file.path(gsub("^.+[\\\\/]+","", file)))
  
}


unitTestRenameFile <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/foo/bar"
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  checkTrue(length(fc$files()) == 1L)
  checkEquals(checksum, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[1]))))
  
  moveFile(fc, fc$files()[1], "/foo/")
  checkTrue(length(fc$files()) == 1L)
  checkEquals(fc$files()[1], file.path("foo", gsub("^.+[\\\\/]+","", file)))
  
}

unitTestRenameFileNewPath <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/foo/bar"
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  checkTrue(length(fc$files()) == 1L)
  checkEquals(checksum, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[1]))))
  
  oldPath <- fc$files()[1]
  moveFile(fc, fc$files()[1], "/foo/newFileName")
  checkTrue(length(fc$files()) == 1L)
  checkEquals(fc$files()[1], "foo/newFileName")
  checkEquals(checksum, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[1]))))
  checkTrue(!file.exists(file.path(fc$files(), oldPath)))
  
}

unitTestRenameFileToRoot <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/foo/bar"
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  checkTrue(length(fc$files()) == 1L)
  checkEquals(checksum, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[1]))))
  
  oldPath <- fc$files()[1]
  moveFile(fc, fc$files()[1], "/newFileName")
  checkTrue(length(fc$files()) == 1L)
  checkEquals(fc$files()[1], "newFileName")
  checkEquals(checksum, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[1]))))
  checkTrue(!file.exists(file.path(fc$files(), oldPath)))
}

unitTestMoveFileToRoot <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/foo/bar"
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  checkTrue(length(fc$files()) == 1L)
  checkEquals(checksum, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[1]))))
  
  oldPath <- fc$files()[1]
  moveFile(fc, fc$files()[1], "/")
  checkTrue(length(fc$files()) == 1L)
  checkEquals(fc$files()[1], gsub("^.+[\\\\/]", "", file))
  checkEquals(checksum, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[1]))))
  checkTrue(!file.exists(file.path(fc$files(), oldPath)))
}


unitTestMoveFileOverExisting <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/foo/bar"
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  checkTrue(length(fc$files()) == 1L)
  checkEquals(checksum, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[1]))))
  
  file2 <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file2)
  path2 <- "/foo"
  checksum2 <- as.character(tools::md5sum(file2))
  addFile(fc, file2, path2)
  checkTrue(length(fc$files()) == 2L)
  checkEquals(checksum2, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[2]))))
  checkEquals(fc$files()[2], sprintf("%s/%s",gsub("^[\\\\/]+", "", path2), gsub("^.+[\\\\/]","", file2)))
  
  checkException(moveFile(fc, fc$files()[1], fc$files()[2]))
  checkTrue(length(fc$files()) == 2L)
  checkTrue(all(file.exists(file.path(fc$cacheDir, fc$files()))))
  checkEquals(checksum, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[1]))))
  checkEquals(checksum2, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[2]))))
  
  checkException(moveFile(fc, fc$files()[1], "/foo"))
}

unitTestReturnValue <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/foo/bar"
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  fc <- addFile(fc, file, path)
  checkTrue(length(fc$files()) == 1L)
}