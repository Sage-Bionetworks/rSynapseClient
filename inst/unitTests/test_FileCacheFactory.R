# TODO: Add comment
#
# Author: furia
###############################################################################

.setUp <-
  function()
{
  synapseClient:::.setCache("oldWarn", options("warn")[[1]])
  options(warn=2)
  synapseClient:::resetFactory(new("FileCacheFactory"))
}

.tearDown <-
  function()
{
  options(warn = synapseClient:::.getCache("oldWarn"))
  synapseClient:::.deleteCache("oldWarn")
  ## clear out the file cache factory
  synapseClient:::resetFactory(new("FileCacheFactory"))
}

unitTestNoArg <-
  function()
{
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
  cacheRoot <- gsub("[\\/]+", "/", normalizePath(cacheRoot))
  archivefile <- tempfile(tmpdir=cacheRoot, fileext=".zip")
  file <- gsub("[\\/]+", "/", tempfile())
  file.create(file)
  suppressWarnings(
    zip(archivefile, files = file)
  )

  fc <- getFileCache(archivefile)
  checkEquals(fc$cacheRoot, cacheRoot)
  checkEquals(length(synapseClient:::availFileCaches()), 1L)
<<<<<<< HEAD
  checkEquals(synapseClient:::availFileCaches(), cacheRoot)

  fc.copy <- getFileCache(fc$cacheRoot)
=======
  checkEquals(synapseClient:::availFileCaches(), archivefile)
  
  fc.copy <- getFileCache(file.path(fc$cacheRoot, fc$archiveFile))
>>>>>>> ae97a52... Fixed bug in FileCacheFactory (SYNR-220)
  checkEquals(fc.copy$cacheRoot, fc$cacheRoot)
  checkEquals(fc.copy$files(), fc$files())
  checkEquals(fc.copy$archiveFile, fc.copy$archiveFile)

  fc.copy <- getFileCache(archivefile)
  checkEquals(fc.copy$cacheRoot, fc$cacheRoot)
  checkEquals(fc.copy$files(), fc$files())
  checkEquals(fc.copy$archiveFile, fc.copy$archiveFile)

  checkEquals(length(synapseClient:::availFileCaches()), 1L)

}

unitTestNewArchiveNoArg <-
  function()
{
  fc <- getFileCache()
  checkEquals(as.character(class(fc)), "FileCache")
  checkEquals(length(synapseClient:::availFileCaches()), 1L)
  checkEquals(fc$getCacheRoot(), synapseClient:::availFileCaches())
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
  cacheRoot <- gsub("[\\/]+", "/", normalizePath(cacheRoot))
  archivefile <- tempfile(tmpdir=cacheRoot, fileext=".zip")
  file <- tempfile()
  file.create(file)
  suppressWarnings(
    zip(archivefile, files = file)
  )

  fc <- getFileCache(archivefile)
  checkEquals(fc$cacheRoot, cacheRoot)
  checkEquals(length(synapseClient:::availFileCaches()), 1L)
  checkEquals(synapseClient:::availFileCaches(), cacheRoot)
}

unitTestSingleFileNotInFactory <-
  function()
{
  cacheRoot <- tempfile()
  dir.create(cacheRoot)
  cacheRoot <- gsub("[\\/]+", "/", normalizePath(cacheRoot))
  file <- gsub("[\\/]+", "/", tempfile(tmpdir=cacheRoot))
  cat(sprintf("Testing...1 %s", Sys.time()), file = file)

  fc <- getFileCache(file)
  checkEquals(fc$cacheDir, file.path(sprintf("%s_unpacked", file)))
}

unitTestExistingEmptyDir <-
  function()
{
  root <- tempfile()
  dir.create(root)
  root <- gsub("[\\/]+", "/", normalizePath(root))
  fc <- getFileCache(root)
  checkEquals(fc$archiveFile, "archive.zip")
  checkEquals(fc$cacheRoot, root)
  checkEquals(fc$cacheDir, file.path(root, sprintf("%s_unpacked", fc$archiveFile)))

  ## get another instance of the cache
  fcc <- getFileCache(root)
  checkEquals(fc$archiveFile, "archive.zip")
  checkEquals(fc$cacheRoot, root)
  checkEquals(fc$cacheDir, file.path(root, sprintf("%s_unpacked", fc$archiveFile)))

  #add a file and make sure it's refelected in both copes
  file <- tempfile()
  cat("Hello World\n", file=file)
  addFile(fcc,file)

  checkEquals(fcc$files, fc$files)

  checkTrue(!file.exists(file.path(fc$cacheRoot, fc$archiveFile)))
  fc$createArchive()
  checkTrue(file.exists(file.path(fc$cacheRoot, fc$archiveFile)))

}

unitTestDirNotExist <-
  function()
{
  root <- gsub("[\\/]+","/", tempfile())
  fc <- getFileCache(root)
  root <- gsub("[\\/]+","/", normalizePath(root))
  checkEquals(fc$archiveFile, "archive.zip")
  checkEquals(fc$cacheRoot, root)
  checkEquals(fc$cacheDir, file.path(root, sprintf("%s_unpacked", fc$archiveFile)))
}


unitTestSetCacheRootNewRootNotExist <-
  function()
{
  root <- tempfile()
  dir.create(root)
  root <- gsub("[\\/]+", "/", normalizePath(root))
  fc <- getFileCache(root)

  file <- gsub("[\\/]+", "/", tempfile())
  cat("Hello World\n", file=file)
  addFile(fc,file)
  checkTrue(file.exists(file.path(fc$cacheDir, basename(file))))

  fc$createArchive()
  checkTrue(file.exists(file.path(fc$cacheRoot, fc$archiveFile)))


  newRoot <- tempfile()
  synapseClient:::setCacheRoot(fc,newRoot)
  newRoot <- gsub("[\\/]+", "/", normalizePath(newRoot))
  checkEquals(fc$archiveFile, "archive.zip")
  checkEquals(fc$cacheRoot, newRoot)
  checkEquals(fc$cacheDir, file.path(newRoot, sprintf("%s_unpacked", fc$archiveFile)))
  checkTrue(file.exists(file.path(fc$cacheRoot, fc$archiveFile)))
  checkTrue(file.exists(file.path(newRoot, sprintf("%s_unpacked", fc$archiveFile))))
  checkTrue(file.exists(file.path(fc$cacheDir, basename(file))))

}

unitTestSingleNonCompressedFileArchive <-
  function()
{
  root <- tempfile()
  dir.create(root)
  file <- tempfile(tmpdir=root)
  cat("Hello World\n", file=file)

  fc <- getFileCache(file)
  fc$unpackArchive()
  checkTrue(file.exists(file.path(fc$cacheDir, basename(file))))

  checkTrue(file.remove(file.path(fc$cacheRoot, fc$archiveFile)))

  fc$createArchive()
  checkTrue(file.exists(file.path(fc$cacheRoot, fc$archiveFile)))
}

<<<<<<< HEAD
unitTestRemoveFileCache <-
  function()
{
  root <- tempfile()
  dir.create(root)
  file <- tempfile(tmpdir=root)
  cat("Hello World\n", file=file)

  fc <- synapseClient:::getFileCache(file)
  checkEquals(length(synapseClient:::availFileCaches()), 1L)  
  checkEquals(normalizePath(synapseClient:::availFileCaches()), normalizePath(root))

  synapseClient:::removeFileCache(root)
  checkEquals(length(synapseClient:::availFileCaches()), 0L)
}
=======
unitTestMultipleArchivesSameRoot <-
    function()
{
  root <- tempfile()
  dir.create(root)
  file1 <- tempfile(tmpdir = root)
  file2 <- tempfile(tmpdir = root)
  
  cat("Hello World\n", file=file1)
  cat("Hello Universe\n", file=file2)
  
  fc1 <- getFileCache(file1)
  fc2 <- getFileCache(file2)
  
  checkEquals(normalizePath(file1), file.path(fc1$cacheRoot, fc1$archiveFile))
  checkEquals(normalizePath(file2), file.path(fc2$cacheRoot, fc2$archiveFile))
  
}

>>>>>>> ae97a52... Fixed bug in FileCacheFactory (SYNR-220)

unitTestGetFileCache <-
  function()
{
  fc <- synapseClient:::getFileCache()
  fc2 <- synapseClient:::getFileCache(fc$getCacheRoot())

  checkEquals(fc$cacheRoot, fc2$cacheRoot)

  file <- tempfile()
  cat("THISISATEST", file = file)
  addFile(fc, file)

  checkEquals(length(fc$files()), 1L)
  checkEquals(length(fc2$files()), 1L)
  checkEquals(fc$files(), basename(file))
  checkEquals(fc$files(), fc2$files())

}


unitTestChangeRoot <-
  function()
{
  fc <- synapseClient:::getFileCache()
  checkEquals(normalizePath(fc$cacheRoot), normalizePath(synapseClient:::availFileCaches()))
  fc2 <- synapseClient:::getFileCache(fc$getCacheRoot())
  checkEquals(normalizePath(fc$cacheRoot), normalizePath(synapseClient:::availFileCaches()))

  checkEquals(fc$cacheRoot, fc2$cacheRoot)

  file <- tempfile()
  cat("THISISATEST", file = file)
  addFile(fc, file)

  checkEquals(length(fc$files()), 1L)
  checkEquals(fc$files(), basename(file))
  checkEquals(fc$files(), fc2$files())

  newRoot <- tempfile()
  synapseClient:::setCacheRoot(fc2, newRoot)
  checkEquals(normalizePath(fc2$getCacheRoot()),  normalizePath(newRoot))
  checkTrue(file.exists(newRoot))
  checkTrue(file.exists(file.path(fc$getCacheDir(), fc$files())))
  checkEquals(length(synapseClient:::availFileCaches()), 1L)
  checkEquals(normalizePath(fc$getCacheRoot()), normalizePath(synapseClient:::availFileCaches()))

  checkEquals(fc$cacheRoot, fc2$cacheRoot)
  checkEquals(length(fc$files()), 1L)
  checkEquals(fc$files(), basename(file))
  checkEquals(fc$files(), fc2$files())

  file2 <- tempfile()
  cat("THISISANOTHERTEST", file = file2)
  addFile(fc2, file2)

  checkEquals(length(fc$files()), 2L)
  checkTrue(all(fc$files() %in% c(basename(file), basename(file2))))
  checkTrue(all(fc$files() %in% fc2$files()))

  fc3 <- synapseClient:::getFileCache(fc2$getCacheRoot())

  checkEquals(length(fc3$files()), 2L)
  checkTrue(all(fc3$files() %in% c(basename(file), basename(file2))))
  checkTrue(all(fc3$files() %in% fc2$files()))

}
