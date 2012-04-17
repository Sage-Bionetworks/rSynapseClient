# TODO: Add comment
# 
# Author: furia
###############################################################################

.setUp <- 
  function()
{
  synapseClient:::.setCache("oldWarn", options("warn")[[1]])
  options(warn=2)
}

.tearDown <-
  function()
{
  options(warn=synapseClient:::.getCache("oldWarn"))
}

unitTestGet <-
  function()
{
  own <- new("CachingObjectOwner")
  own$objects$bar <- "foo"
  checkEquals(own$objects$bar, "foo")
}

unitTestGetInvalidObject <-
  function()
{
  own <- new("CachingObjectOwner")
  checkTrue(is.null(getObject(own, "bar")))
}

unitTestNames <-
  function()
{
  own <- new("CachingObjectOwner")
  checkTrue(all(names(own) == c("objects")))
  own$objects$bar <- "foo"
  checkTrue(all(names(own) == c("objects")))
}

unitTestAddObject <-
  function()
{
  own <- new("CachingObjectOwner")
  own$objects$foo <- "bar"
  checkEquals(names(own$objects), "foo")
  checkEquals(own$objects$foo, "bar")
  
  copy <- addObject(own, "boo", "blah")
  checkTrue(all(names(own$objects) %in% c("foo", "blah")))
  checkTrue(all(c("foo", "blah") %in% names(own$objects)))
  checkEquals(own$objects$foo, "bar")
  checkEquals(own$objects$blah, "boo")
  
  checkTrue(all(names(copy$objects) %in% c("foo", "blah")))
  
}

unitTestAddObjectNoName <-
    function()
{
  own <- new("CachingObjectOwner")
  foo <- "bar"
  copy <- addObject(own, foo)
  checkEquals(names(own$objects), "foo")
  checkEquals(own$objects$foo, "bar")
  checkEquals(names(copy$objects), "foo")
  checkEquals(copy$objects$foo, "bar")
  
  copy <- addObject(own, "boo", "blah")
  checkTrue(all(names(own$objects) %in% c("foo", "blah")))
  checkTrue(all(c("foo", "blah") %in% names(own$objects)))
  checkEquals(own$objects$foo, "bar")
  checkEquals(own$objects$blah, "boo")
  
  checkTrue(all(names(copy$objects) %in% c("foo", "blah")))
  
}

unitTestGetObject <-
  function()
{
  own <- new("CachingObjectOwner")
  own$objects$foo <- "bar"
  checkEquals("bar", getObject(own, "foo"))
}

unitTestDeleteObject <-
  function()
{
  own <- new("CachingObjectOwner")
  own$objects$foo <- "bar"
  copy <- deleteObject(own, "foo")
  checkEquals(length(own$objects), 0L)
  checkEquals(length(copy$objects), 0L)
}

unitTestRenameObject <-
  function()
{
  own <- new("CachingObjectOwner")
  own$objects$foo <- "bar"

  copy <- renameObject(own, "foo", "boo")
  checkEquals(length(own$objects), 1L)
  checkEquals(length(copy$objects), 1L)
  checkEquals(own$objects$boo, "bar")
  checkEquals(copy$objects$boo, "bar")
}

## not sure that this method should be exposed
## leaving getEnv unimplemented for now.
#unitTestGetEnv <-
#  function()
#{
#  stop("Not Yet Implemented")
#}

unitTestAddListUnlist <-
  function()
{
  own <- new("CachingObjectOwner")
  aList <- list(foo = "bar", boo = 1L)
  addObject(own, aList)
  
  checkEquals(length(own$objects), 1L)
  checkEquals("aList", names(own$objects))
  checkEquals(own$objects$aList$foo, "bar")
  checkEquals(own$objects$aList$boo, 1L)
  checkEquals("list", as.character(class(own$objects$aList)))
  
  own <- new("CachingObjectOwner")
  addObject(own, aList, unlist = TRUE)
  
  checkEquals(length(own$objects), 2L)
  checkTrue(all(c("foo", "boo") %in% names(own$objects)))
  checkEquals(own$objects$foo, "bar")
  checkEquals(own$objects$boo, 1L)
  
  own <- new("CachingObjectOwner")
  addObject(own, aList, unlist = FALSE)
  
  checkEquals(length(own$objects), 1L)
  checkEquals("aList", names(own$objects))
  checkEquals(own$objects$aList$foo, "bar")
  checkEquals(own$objects$aList$boo, 1L)
  checkEquals("list", as.character(class(own$objects$aList)))
  
}


unitTestaddDataFrameUnlist <-
    function()
{
  own <- new("CachingObjectOwner")
  aList <- data.frame(foo = "bar", boo = 1L, stringsAsFactors=F)
  addObject(own, aList)
  
  checkEquals(length(own$objects), 1L)
  checkEquals("aList", names(own$objects))
  checkEquals(own$objects$aList$foo, "bar")
  checkEquals(own$objects$aList$boo, 1L)
  
  own <- new("CachingObjectOwner")
  addObject(own, aList, unlist = TRUE)
  
  checkEquals(length(own$objects), 2L)
  checkTrue(all(c("foo", "boo") %in% names(own$objects)))
  checkEquals(own$objects$foo, "bar")
  checkEquals(own$objects$boo, 1L)
  
  own <- new("CachingObjectOwner")
  addObject(own, aList, unlist = FALSE)
  
  checkEquals(length(own$objects), 1L)
  checkEquals("aList", names(own$objects))
  checkEquals(own$objects$aList$foo, "bar")
  checkEquals(own$objects$aList$boo, 1L)
}

unitTestNoZip <- 
  function()
{
  ## need to verify the proper behavior when zip is not installed:
  ##
  ## 1) Double check the behavior of CachingEnhancedEnvironment (see
  ## "unitTestNoZip" in test_CachingEnhancedEnvironment.R these tests
  ## can be less thorough but should server as a double-check
  ##
  ## 2) loadCachedObjects() method should recognize and load the object
  ## with the ".R_OBJECTS_" prefix.
  ##
  ## 3) If loadCachedObjects is called on a system with zip for an 
  ## archive created on a system without zip (as indicated by the presence
  ## of a file with the ".R_OBJECTS_" prefix) the function should 
  ## print an informative message with instructions for how to "repair"
  ## the archive.
  ##
  ## 4) The archive can be repaired by calling the loadObjects() method
  ## on the enclosed CachingEnhancedEnvironment object with and optional
  ## flag set. (not yet implemented)
  ##
  ## 5) Once the archive "repaired", the restriction of a storing only
  ## a single object should be lifted.
  
  stop("Not yet implmented: Bruce?")
}



