## Unit tests for adding files to entities
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

unitTestOverwriteFile <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/foo/bar"
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  checkEquals(length(fc$getFileMetaData()), 1L)
  checkEquals(checksum, as.character(tools::md5sum(names(fc$getFileMetaData())[1])))
  
  d <- diag(x = 2, nrow=10, ncol=10)
  save(d, file=file)
  checksum2 <- as.character(tools::md5sum(file))
  addFile(fc, file, path)
  checkEquals(length(fc$getFileMetaData()), 1L)
  checkEquals(checksum2, as.character(tools::md5sum(names(fc$getFileMetaData())[1])))
  checkTrue(checksum2 != checksum)
  
  file2 <- tempfile()
  d <- diag(x = 2, nrow=10, ncol=10)
  save(d, file=file2)
  checksum3 <- as.character(tools::md5sum(file2))
  addFile(fc, file2, path)
  checkEquals(length(fc$getFileMetaData()), 2L)
  checkEquals(checksum3, as.character(tools::md5sum(names(fc$getFileMetaData())[2])))
  
  ## overwrite again
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  
  addFile(fc, file, path)
  checkEquals(length(fc$getFileMetaData()), 2L)
  checkEquals(checksum, as.character(tools::md5sum(names(fc$getFileMetaData())[1])))
  checkEquals(checksum3, as.character(tools::md5sum(names(fc$getFileMetaData())[2])))
}

unitTestAddFileToRoot <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/"
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  checkEquals(length(fc$getFileMetaData()), 1L)
  checkEquals(checksum, as.character(tools::md5sum(names(fc$getFileMetaData())[1])))
  
  file <- tempfile()
  d <- diag(x = 2, nrow=10, ncol=10)
  save(d, file=file)
  path <- ""
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  checkEquals(length(fc$getFileMetaData()), 1L)
  checkEquals(checksum, as.character(tools::md5sum(names(fc$getFileMetaData())[1])))
  
  file <- tempfile()
  d <- diag(x = 2, nrow=10, ncol=10)
  save(d, file=file)
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  fc <- addFile(fc, file)
  checkEquals(length(fc$getFileMetaData()), 1L)
  checkEquals(checksum, as.character(tools::md5sum( names(fc$getFileMetaData())[1])))
}

unitTestMultipleSlashes <-
  function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "////foo//bar"
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  relPaths <- as.character(unlist(lapply(fc$metaData, function(m) m$relativePath)))
  checkEquals(relPaths[1], sprintf("%s/%s", "foo/bar", gsub("^.+[\\\\//]","",file)))
  
  path <- "////foo\\\\bar"
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  relPaths <- as.character(unlist(lapply(fc$metaData, function(m) m$relativePath)))
  checkEquals(relPaths[1], sprintf("%s/%s", "foo/bar", gsub("^.+[\\\\//]","",file)))
  
  path <- "\\\\\\\\foo\\\\bar"
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  relPaths <- as.character(unlist(lapply(fc$metaData, function(m) m$relativePath)))
  checkEquals(relPaths[1], sprintf("%s/%s", "foo/bar", gsub("^.+[\\\\//]","",file)))
  
}

unitTestAddDirNoPathTwoFiles <-
  function()
{
  dir <- tempfile()
  dir.create(file.path(dir,"/subdir"), recursive=T)
  file <- file.path(dir, "/subdir/myFile.rbin")
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  
  file2 <- file.path(dir, "myFile2.rbin")
  d <- diag(x=2,nrow=10, ncol=10)
  save(d, file=file2)
  
  fc <- new(Class="FileCache")
  addFile(fc, dir)
  relPaths <- as.character(unlist(lapply(fc$metaData, function(m) m$relativePath)))
  checkTrue(all(file.path(gsub("^.+[\\\\/]+", "", dir), c("subdir/myFile.rbin", "myFile2.rbin")) %in% relPaths))
  
}

unitTestAddDirNoPath <-
  function()
{
  dir <- tempfile()
  file <- file.path(dir, "/subdir/myFile.rbin")
  dir.create(file.path(dir, "/subdir"), recursive = TRUE)
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  
  fc <- new(Class="FileCache")
  addFile(fc, dir)
  checkEquals(fc$getFileMetaData()[[1]]$relativePath, sprintf("%s/%s", gsub("^.+[\\\\/]", "", dir), "subdir/myFile.rbin"))
  
}

unitTestAddDirAndFileTwoPaths <-
  function()
{
  fc <- new(Class="FileCache")
  file <- tempfile()
  file <- normalizePath(file, mustWork=F)
  d <- diag(nrow=10,ncol=10)
  save(d, file=file)
  
  adir <- normalizePath(tempfile(), mustWork=F)
  dir.create(adir, recursive=T)
  afile1 <- normalizePath(tempfile(tmpdir=adir), mustWork=F)
  d1 <- diag(nrow=100,ncol=100)
  save(d1, file=afile1)
  
  afile2 <- normalizePath(tempfile(tmpdir=adir), mustWork=F)
  d2 <- diag(nrow=1000,ncol=1000)
  save(d2, file=afile2)
  
  addFile(fc, c(adir,file), c("foo", "bar"))
  
  checkEquals(length(fc$metaData), 3L)
  
  relPaths <- as.character(unlist(lapply(fc$metaData, function(m) m$relativePath)))
  expectedPaths <- gsub("/+", "/", file.path(c("bar", "foo", "foo"), gsub(tempdir(), "", c(file, list.files(adir, recursive=T, full.names=T)), fixed = TRUE)))
  checkTrue(all(relPaths %in% expectedPaths))
}

unitTestTwoFilesOnePath <-
  function()
{
  fc <- new(Class="FileCache")
  file1 <- tempfile()
  d <- diag(nrow=10,ncol=10)
  save(d, file=file1)
  
  file2 <- tempfile()
  d <- diag(x=2,nrow=10,ncol=10)
  save(d, file=file2)
  path <- "aPath"
  addFile(fc, c(file1, file2), path)
  checkEquals(length(names(fc$getFileMetaData())), 2L)
  relPaths <- as.character(unlist(lapply(fc$metaData, function(m) m$relativePath)))
  checkTrue(all(file.path(path, gsub("^.+[\\\\/]+", "", c(file1, file2))) %in% relPaths))
}

unitTestTwoFilesThreePaths <-
  function()
{
  fc <- new(Class="FileCache")
  file <- tempfile()
  file.create(file)
  
  checkException(addFile(fc, c(file, file), c("one", "two", "three")))
}

unitTestAddToSubDirKeepName <-
    function()
{
  file <- tempfile()
  d <- diag(nrow=10, ncol=10)
  save(d, file=file)
  path <- "/foo/"
  checksum <- as.character(tools::md5sum(file))
  
  fc <- new(Class="FileCache")
  addFile(fc, file, path)
  
  relPaths <- as.character(unlist(lapply(fc$metaData, function(m) m$relativePath)))
  checkEquals(length(relPaths), 1L)
  checkEquals(relPaths, gsub("^/", "", gsub("/+","/", gsub(tempdir(), "", file.path(path, file), fixed = TRUE))))
  checkEquals(checksum, as.character(tools::md5sum(names(fc$getFileMetaData())[1])))
  
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
    checkEquals(length(fc$getFileMetaData()), 1L)
    checkEquals(checksum, as.character(tools::md5sum(names(fc$getFileMetaData())[1])))
}




