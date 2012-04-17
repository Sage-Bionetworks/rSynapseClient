# TODO: Add comment
# 
# Author: furia
###############################################################################
.setUp <- 
  function()
{
  env <- attach(NULL, name = "testEnv")
  tryCatch({
      setPackageName("testEnv", env)
      suppressWarnings(
        setClass(
          "LocOwn",
          contains = "SynapseLocationOwner",
          where = env
        ) 
      )
    },
    error = function(e){
      detach("testEnv")
      stop(e)
    }
  )
  
  synapseClient:::.setCache("oldWarn", options("warn")[[1]])
}

.tearDown <-
  function()
{
  detach("testEnv")
  options(warn = synapseClient:::.getCache("oldWarn"))
}


unitTestConstructors <-
  function()
{
  stop("not yet implemented")
}

unitTestAddFile <- 
  function()
{
  own <- new("LocOwn")
  
  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", Sys.time()), file = file)
  copy <- addFile(own, file)
  checkEquals(gsub("^.+/", "", file), own$files)
  checkEquals(gsub("^.+/", "", file), copy$files)
  
  ## make sure the cache re-initializes but running the exact same
  ## test again
  own <- new("LocOwn")
  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", Sys.time()), file = file)
  addFile(own, file)
  checkEquals(gsub("^.+/", "", file), own$files)
  
  addFile(own, file, "foo.bar")
  checkEquals(length(own$files), 2L)
  checkTrue(all(c(gsub("^.+/", "", file), "foo.bar") %in% own$files))
  checkTrue(all(own$files %in% c(gsub("^.+/", "", file), "foo.bar")))
}

unitTestMoveFile <-
  function()
{
  own <- new("LocOwn")
  
  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", Sys.time()), file = file)
  addFile(own, file)
  checkEquals(gsub("^.+/", "", file), own$files)
  
  copy <- moveFile(own, own$files, "newName.txt")
  checkEquals("newName.txt", copy$files)
  checkEquals("newName.txt", own$files)
  
  moveFile(own, own$files, "subdir/")
  checkEquals("subdir/newName.txt", own$files)
  
  moveFile(own, own$files, "/newSubDir2/anotherName2.txt")
  checkEquals("newSubDir2/anotherName2.txt", own$files)
  
  moveFile(own, own$files, "/newSubDir/anotherName.txt")
  checkEquals("newSubDir/anotherName.txt", own$files)
  
}

uniTestDeleteFile <-
  function()
{
  stop("not yet implemented")
}

unitTestLoadFromFiles <-
  function()
{
  
}
  
