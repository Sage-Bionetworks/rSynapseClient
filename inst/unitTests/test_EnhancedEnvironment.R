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
  
  checkTrue(all(as.character(class(ee)) == as.character(copy)))
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
  checkTrue(all(c(".bar", "foo") %in% objects(ee)))
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
  
  checkEquals(length(objects(ee), all.names=TRUE), 2L)
  checkEquals(length(objects(ee)), 1L)
  checkTrue(objects(ee) == "foo")
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
  
  checkEquals(ee[["foo"]], "bar")
  ee$blah <- "boo"
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
  stop("Not Yet Implemented")
}

unitTestBracketAccessorNegativeIndices <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestBracketAccessorNonNegativeIndices <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestDeleteObject <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestDeleteMultipleObjects <-
  function()
{
  stop("Not Yet Implemented")  
}

unitTestGetObject <-
  function()
{
  stop("Not Yet Implemented") 
}

unitTestGetMulipleObjects <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestAddList <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestAddDataFrame <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestRenameObject <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestRenameMultipleObjects <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestAsEnvironment <-
  function()
{
  stop("Not Yet Implemented")
}








