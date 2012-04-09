# TODO: Add comment
# 
# Author: furia
###############################################################################

.setUp <- 
  function()
{
  env <- attach(NULL, name = "testEnv")
  setPackageName("testEnv", env)
  suppressWarnings(
      setRefClass(
          "rObjOwn",
          contains = "ReadOnlyObjectOwner",
          where = env
      )
  )
}

.tearDown <-
  function()
{
  detach("testEnv")
}

unitTestGet <-
  function()
{
  own <- getRefClass("rObjOwn")$new()
  own$objects$bar <- "foo"
  checkEquals(own$objects$bar, "foo")
}

unitTestGetInvalidObject <-
  function()
{
  own <- getRefClass("rObjOwn")$new()
  checkException(getObject(own, "bar"))
}

unitTestNames <-
  function()
{
  own <- getRefClass("rObjOwn")$new()
  checkTrue(all(names(own) == c("cacheDir", "files", "objects")))
  own$objects$bar <- "foo"
  checkTrue(all(names(own) == c("cacheDir", "files", "objects")))
}

unitTestBracketAccessor <-
  function()
{
  own <- getRefClass("rObjOwn")$new()
  own$objects$foo <- "boo"
  own$objects$bar <- "goo"
  own$objects$coo <- "zoo"
  checkTrue(all(names(own[]) == c("cacheDir", "files", "objects")))
  checkTrue(all(names(own[NULL]) == c("cacheDir", "files", "objects")))
  
  checkTrue(all(names(own[c("objects", "files")]) == c("objects", "files")))
  checkEquals(names(own[1]), "cacheDir")
  checkTrue(all(names(own[c(2,1)]) == c("files", "cacheDir")))
}

unitTestDoubleBracketAccessor <-
  function()
{
  own <- getRefClass("rObjOwn")$new()
  own$objects$foo <- "boo"
  own$objects$bar <- "goo"
  own$objects$coo <- "zoo"
  
  own$cacheDir <- "/foo/bar"
  
  ## this test has a dependency on the behavior of Enhanced Environment
  checkTrue(all(names(own[['objects']]) == c("bar", "coo", "foo")))
  checkEquals(own[['cacheDir']], "/foo/bar")
}

