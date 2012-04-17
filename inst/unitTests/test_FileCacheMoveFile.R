## Unit tests for moving entity files
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

unitTestSimpleMove <-
  function()
{
  fc <- new("FileCache")
  file <- tempfile()
  cat("THIS IS A TEST %s", Sys.time(), file = file)
  
  addFile(fc, file)
  checkEquals(length(fc$files), 1L)
  
  copy <- moveFile(fc, fc$files(), "foo.bar")
  checkEquals(length(fc$files()), 1L)
  checkEquals(fc$files(), "foo.bar")
}

unitTestSimpleMoveToDir <-
  function()
{
  fc <- new("FileCache")
  file <- tempfile()
  cat("THIS IS A TEST %s", Sys.time(), file = file)
  
  addFile(fc, file, "foo.bar")
  checkEquals(length(fc$files), 1L)
  
  copy <- moveFile(fc, fc$files(), "blah/")
  checkEquals(length(fc$files()), 1L)
  checkEquals(fc$files(), "blah/foo.bar")
  
  
  fc <- new("FileCache")
  file <- tempfile()
  cat("THIS IS A TEST %s", Sys.time(), file = file)
  
  addFile(fc, file, "foo.bar")
  checkEquals(length(fc$files), 1L)
  
  copy <- moveFile(fc, fc$files(), "/blah/")
  checkEquals(length(fc$files()), 1L)
  checkEquals(fc$files(), "blah/foo.bar")
}

unitTestSimpleMoveToRoot <-
  function()
{
  fc <- new("FileCache")
  file <- tempfile()
  cat("THIS IS A TEST %s", Sys.time(), file = file)
  
  addFile(fc, file, "subdir/foo.bar")
  checkEquals(length(fc$files), 1L)
  
  copy <- moveFile(fc, fc$files(), "/")
  checkEquals(length(fc$files()), 1L)
  checkEquals(fc$files(), "foo.bar")
}


unitTestMoveFileNewPathDirExists <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/foo/bar/"
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
  path <- "/foo/bar/"
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


unitTestMoveFile <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/foo/bar/"
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  checkTrue(length(fc$files()) == 1L)
  checkEquals(checksum, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[1]))))
  
  moveFile(fc, fc$files()[1], "/foo/")
  checkTrue(length(fc$files()) == 1L)
  checkEquals(fc$files()[1], file.path("foo", gsub("^.+[\\\\/]+","", file)))
  
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
  
  oldPath <- fc$files()[1]
  moveFile(fc, fc$files()[1], "/foo/newFileName")
  checkTrue(length(fc$files()) == 1L)
  checkEquals(fc$files()[1], "foo/newFileName")
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
  path <- "/foo/bar/"
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

unitTestMoveFileRenameIntoNewSubDir <-
  function()
{

  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  addFile(fc, file)
  checkTrue(length(fc$files()) == 1L)
  checkEquals(checksum, as.character(tools::md5sum(file.path(fc$cacheDir, fc$files()[1]))))
  
  ## path specified with a leading slash
  path <- "/subdir/newName.rbin"
  moveFile(fc, fc$files(), path)
  checkEquals(fc$files(), "subdir/newName.rbin")
  
  ## no leading slash
  path <- "subdir2/newName2.rbin"
  moveFile(fc, fc$files(), path)
  checkEquals(fc$files(), "subdir2/newName2.rbin")
}

unitTestMoveDirectoryToRoot <-
  function()
{
  stop("not yet implemented")
}

unitTestMoveDirectoryToNewSubdir <-
  function()
{
  stop("not yet implemented")
}







