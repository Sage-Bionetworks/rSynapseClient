# TODO: Add comment
# 
# Author: furia
###############################################################################

unitTestAssignment <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee[['foo']] <- "bar"
  checkEquals(names(ee), "foo") 
}

unitTestAddObjectAssignment <-
  function()
{
  ee <- new("EnhancedEnvironment")
  addObject(ee, "bar", "foo")
  checkEquals(names(ee), "foo")
  boo <- "blah"
  copy <- addObject(ee, boo)
  checkTrue(all(c('foo','boo') %in% names(ee)))
  checkEquals(length(names(ee)), 2L)
  
  checkTrue(all(as.character(class(ee)) == as.character(class(copy))))
  checkTrue(all(c('foo','boo') %in% names(copy)))
  checkEquals(length(names(copy)), 2L)
}

unitTestNames <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  checkEquals(names(ee), "foo")
}

unitTestNameObjectsStartingWithDot <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  checkEquals(objects(ee), "foo")
  
  ## add names starting with dot
  addObject(ee, "boo", ".bar")
  
  checkEquals(length(names(ee)), 2L)
  checkTrue(all(c(".bar", "foo") %in% names(ee)))
}

unitTestObjects <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  checkEquals(objects(ee), "foo")
}

unitTestListObjectsStartingWithDot <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  checkEquals(objects(ee), "foo")
  
  ## add names starting with dot
  addObject(ee, "boo", ".bar")
  
  checkEquals(length(objects(ee, all.names=TRUE)), 2L)
  checkEquals(length(objects(ee)), 1L)
  checkTrue(objects(ee) == "foo")
  checkTrue(all(c(".bar", "foo") %in% objects(ee, all.names=TRUE)))
}

unitTestBracketAccessor <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  
  checkEquals(ee["foo"], list(foo="bar"))
  checkEquals(class(ee["foo"]), "list")
  checkEquals(names(ee["foo"]), "foo")
}

unitTestDoubleBracketAccessor <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  
  ## get a single object
  checkEquals(ee[["foo"]], "bar")
  ee$blah <- "boo"

  ## get two objects. should be an exception
  checkException(ee[[c("foo", "blah")]])
}

unitTestAccessor <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  
  checkEquals(ee$foo, "bar")
}

unitTestMultipleObjects <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  ee$boo <- "blah"
  
  checkTrue(all(names(ee) == sort(c("foo", "boo"))))
  checkTrue(all(ee[1:2] == c("blah", "bar")))
  checkTrue(all(names(ee[1:2]) == c("boo", "foo")))
  checkTrue(all(ee[c(2,1)] == c("bar", "blah")))
  checkTrue(all(names(ee[c(2,1)]) == c("foo", "boo")))
  
  checkTrue(is.null(names(ee[[1]])))
  checkEquals(ee[[1]], "blah")
  checkEquals(ee[[2]], "bar")
  
  checkTrue(all(names(ee[]) == sort(c("foo", "boo"))))
  checkTrue(all(ee[] == c("blah", "bar")))
}

unitTestShow <-
  function()
{
  ee <- new("EnhancedEnvironment")
  checkEquals(capture.output(show(ee)), "character(0)")
  ee$foo <- "bar"
  checkEquals(capture.output(show(ee)), "[1] foo (character)")
  ee$boo <- 1
  checkTrue(all(capture.output(show(ee)) == c("[1] boo (numeric)", "[2] foo (character)")))
}

unitTestShowMulipleClasses <-
  function()
{
  ee <- new("EnhancedEnvironment")
  checkEquals(capture.output(show(ee)), "character(0)")
  ee$boo <- 1
  checkEquals(capture.output(show(ee)), "[1] foo (character)")
  foo <- "A"
  class(foo) <- c("FakeClass1", "FakeClass2")
  addObject(ee, foo)
  checkTrue(all(capture.output(show(ee)) == c("[1] boo (numeric)", "[2] foo (FakeClass1,FakeClass2)")))
}

unitTestDoubleBracketAccessorNegativeIndices <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$boo <- 1
  ee$foo <- "bar"
  
  checkEquals(ee[[-1]], "bar")
  checkEquals(ee[[-2]], 1)
  
  checkException(ee[[c(-2, -1)]])
}

unitTestDoubleBracketAccessorNegativeIndicesOutOfBounds <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$boo <- 1
  ee$foo <- "bar"
  
  checkException(ee[[-2]])
  checkException(ee[[-0]])
}


unitTestBracketAccessorNegativeIndices <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$boo <- 1
  ee$foo <- "bar"
  
  checkEquals(ee[-1][[1]], "bar")
  checkEquals(names(ee[-1]), "foo")
  checkEquals(ee[-2][[1]], 1)
  checkEquals(names(ee[-2]), "boo")
  
  checkEquals(length(ee[c(-2, -1)]), 0L)
  
}
unitTestBracketAccessorNegativeIndicesOutOfBounds <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$boo <- 1
  ee$foo <- "bar"
  
  checkException(ee[-2])
  checkException(ee[-0])
}

unitTestBracketAccessorNumericIndicies <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  
  checkEquals(ee[1], list(foo="bar"))
  checkEquals(class(ee[1]), "list")
  checkEquals(names(ee[1]), "foo")
  
  checkException(ee[2])
}

unitTestDoubleBracketAccessorNumericIndicies <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  
  checkEquals(ee[[1]], "bar")
  checkException(ee[[2]])
}

unitTestDoubleBracketAccessor <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  
  ## get a single object
  checkEquals(ee[["foo"]], "bar")
  ee$blah <- "boo"
  
  ## get two objects. should be an exception
  checkException(ee[[c("foo", "blah")]])
}

unitTestDeleteObject <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  ee$boo <- "blah"

  copy <- deleteObject(ee, "foo")
  checkEquals(length(ee), 1L)
  checkEquals(length(copy), 1L)
  checkEquals(ee[[1]], "blah")
  
  deleteObject(ee, "boo")
  checkEquals(length(ee), 0L)
  
  ## TODO: get this working
  ## checkWarning(deleteObject(ee, "fakeObject"))
}

unitTestDeleteMultipleObjects <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  ee$boo <- "blah"
  
  deleteObject(ee, c("foo", "boo"))
  checkEquals(length(ee), 0L)
}

unitTestGetObject <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  ee$boo <- "blah"

  checkEquals("bar", getObject(ee, "foo"))
}

unitTestGetMulipleObjects <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  ee$boo <- "blah"
  
  checkEquals("bar", getObject(ee, "foo"))
  
  checkTrue(all(c("bar", "blah") == getObject(ee, c("foo", "boo"))))
  checkTrue(all(c("blah", "bar") == getObject(ee, c("boo", "foo"))))
  checkTrue(all(c("bar", "blah", "bar") == getObject(ee, c("foo", "boo", "foo"))))
  checkTrue(all(c("bar", "blah", "bar") == getObject(ee, c("foo", "boo", "foo"))))
}

unitTestAddList <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$aList <- list(foo="bar", boo = "blah")
  
  checkEquals(length(ee), 1L)
  checkEquals("list", class(getObject(ee, "aList")))
  checkEquals(2L, length(getObject(ee, "aList")))
  checkTrue(all(names(getObject(ee, "aList")) == c("foo", "boo")))
  checkTrue(all(as.character(getObject(ee, "aList")) == c("bar", "blah")))
  
  ee$aNum <- 1L
  checkEquals(length(ee), 2L)
  checkEquals("list", class(getObject(ee, "aList")))
  checkEquals(2L, length(getObject(ee, "aList")))
  checkTrue(all(names(getObject(ee, "aList")) == c("foo", "boo")))
  checkTrue(all(as.character(getObject(ee, "aList")) == c("bar", "blah")))
}

unitTestAddDataFrame <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$aList <- data.frame(list(foo="bar", boo = "blah"))
  
  checkEquals(length(ee), 1L)
  checkEquals("data.frame", class(getObject(ee, "aList")))
  checkEquals(2L, length(getObject(ee, "aList")))
  checkTrue(all(names(getObject(ee, "aList")) == c("foo", "boo")))
  
  ee$aNum <- 1L
  checkEquals(length(ee), 2L)
  checkEquals("data.frame", class(getObject(ee, "aList")))
  checkEquals(2L, length(getObject(ee, "aList")))
  checkTrue(all(names(getObject(ee, "aList")) == c("foo", "boo")))
}

unitTestRenameObject <-
  function()
{
  ee <- new("EnhancedEnvironment")
  
  ee$aList <- 1L
  checkEquals(length(ee), 1L)
  checkEquals(1L, getObject(ee, "aList"))
  
  ee$aNum <- data.frame(list(foo="bar", boo = "blah"))
  copy <- renameObject(ee, 'aNum', 'aList')
  checkEquals(length(ee), 1L)
  checkEquals("data.frame", class(getObject(ee, "aList")))
  checkEquals(2L, length(getObject(ee, "aList")))
  checkTrue(all(names(getObject(ee, "aList")) == c("foo", "boo")))

  checkEquals(length(copy), 1L)
  checkEquals("data.frame", class(getObject(copy, "aList")))
  checkEquals(2L, length(getObject(copy, "aList")))
  checkTrue(all(names(getObject(copy, "aList")) == c("foo", "boo")))

}

unitTestRenameMultipleObjects <-
  function()
{
  ee <- new("EnhancedEnvironment")
  
  ee$aNum <- 1L
  ee$aList <- list(foo="bar", boo = "blah")
  renameObject(ee, c('aNum', 'aList'), c('aList', 'aNum'))
  
  checkEquals(length(ee), 2L)
  checkEquals("integer", class(getObject(ee, "aList")))
  checkEquals(2L, length(getObject(ee, "aNum")))
  checkEquals("list", as.character(class(getObject(ee, "aNum"))))
  checkTrue(all(names(getObject(ee, "aNum")) == c("foo", "boo")))
  

}

unitTestAsEnvironment <-
  function()
{
  ee <- new("EnhancedEnvironment")
  
  ee$aNum <- 1L
  env <- as.environment(ee)
  checkEquals("environment", class(env))
  checkEquals("aNum", objects(env))
}








