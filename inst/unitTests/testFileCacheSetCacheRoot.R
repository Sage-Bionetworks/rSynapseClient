


testResetPathNoClean <-
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

testResetPathClean <-
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


testDirExistsNoClean <-
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

testDirExistsClean <-
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
  checkTrue(file.exists(file.path(fc$getCacheRoot(), basename(file))))
  checkTrue(file.exists(croot))
  checkTrue(file.exists(cdir))
  checkTrue(file.exists(file.path(cdir, basename(file))))
  croot <- fc$getCacheRoot()
  cdir <- fc$getCacheDir()
  
  checkException(synapseClient:::setCacheRoot(fc, croot, TRUE))
}
