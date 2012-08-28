.setUp <-
  function()
{
  synapseClient:::resetFactory(new("FileCacheFactory"))
}

.tearDown <-
  function()
{
  synapseClient:::resetFactory(new("FileCacheFactory"))
}

unitTestResetPathNoClean <-
  function()
{
  fc <- new("FileCache")

  croot <- fc$getCacheRoot()
  cdir <- fc$getCacheDir()
  checkTrue(file.exists(croot))
  checkTrue(file.exists(cdir))


  newdir <- tempfile()
  synapseClient:::setCacheRoot(fc, newdir, FALSE)
  checkTrue(file.exists(fc$getCacheRoot()))
  checkTrue(file.exists(fc$getCacheDir()))
  checkTrue(file.exists(croot))
  checkTrue(file.exists(cdir))

}

unitTestResetPathClean <-
  function()
{
  fc <- new("FileCache")

  croot <- fc$getCacheRoot()
  cdir <- fc$getCacheDir()
  checkTrue(file.exists(croot))
  checkTrue(file.exists(cdir))


  newdir <- tempfile()
  synapseClient:::setCacheRoot(fc, newdir, TRUE)
  checkTrue(file.exists(fc$getCacheRoot()))
  checkTrue(file.exists(fc$getCacheDir()))
  checkTrue(!file.exists(croot))
  checkTrue(!file.exists(cdir))

}


unitTestDirExistsNoClean <-
  function()
{
  fc <- new("FileCache")

  croot <- fc$getCacheRoot()
  cdir <- fc$getCacheDir()
  checkTrue(file.exists(croot))
  checkTrue(file.exists(cdir))


  newdir <- tempfile()
  synapseClient:::setCacheRoot(fc, newdir, FALSE)
  checkTrue(file.exists(fc$getCacheRoot()))
  checkTrue(file.exists(fc$getCacheDir()))
  checkTrue(file.exists(croot))
  checkTrue(file.exists(cdir))

  checkException(synapseClient:::setCacheRoot(fc, croot))
}

unitTestDirExistsClean <-
  function()
{
  fc <- new("FileCache")

  croot <- fc$getCacheRoot()
  cdir <- fc$getCacheDir()
  checkTrue(file.exists(croot))
  checkTrue(file.exists(cdir))

  file <- tempfile()
  cat(sprintf("test %s", Sys.time()), file=file)
  addFile(fc,file)

  ## don't clean the first time we change
  newdir <- tempfile()
  synapseClient:::setCacheRoot(fc, newdir, FALSE)
  checkTrue(file.exists(fc$getCacheRoot()))
  checkTrue(file.exists(fc$getCacheDir()))
  checkTrue(file.exists(file.path(fc$getCacheDir(), basename(file))))
  checkTrue(file.exists(croot))
  checkTrue(file.exists(cdir))
  checkTrue(file.exists(file.path(cdir, basename(file))))
  croot <- fc$getCacheRoot()
  cdir <- fc$getCacheDir()
}

unitTestChangeRootExistingFile <-
  function()
{
  fc <- new("FileCache")
  file <- tempfile()
  cat("THISISATEST", file=file)
  addFile(fc,file)

  checkEquals(fc$files(), basename(file))

  newroot <- tempfile()
  synapseClient:::setCacheRoot(fc, newroot, TRUE)

  checkEquals(gsub("[\\/]+", "/", normalizePath(newroot)), fc$getCacheRoot())

  cacheDir <- file.path(fc$getCacheRoot(), "archive.zip_unpacked")
  checkEquals(cacheDir, fc$getCacheDir())

  checkTrue(file.exists(file.path(cacheDir, basename(file))))

  checkTrue(all(grepl(sprintf("^%s",fc$getCacheDir()), names(fc$getFileMetaData()))))
}

unitTestChangeRootExistingArchive <-
  function()
{
  fc <- new("FileCache")
  file <- tempfile()
  cat("THISISATEST", file=file)
  addFile(fc,file)

  archFile <- fc$createArchive()
  checkTrue(file.exists(file.path(fc$getCacheRoot(), archFile)))

  oldroot <- fc$getCacheRoot()
  newroot <- tempfile()

  synapseClient:::setCacheRoot(fc, newroot, TRUE)
  checkTrue(!file.exists(oldroot))
  checkTrue(file.exists(newroot))
  checkTrue(file.exists(file.path(newroot, fc$archiveFile)))
  checkEquals(gsub("[\\/]+", "/", normalizePath(newroot)), fc$getCacheRoot())
  checkTrue(file.exists(fc$getCacheDir()))
  checkTrue(file.exists(file.path(fc$getCacheDir(), basename(file))))
}



