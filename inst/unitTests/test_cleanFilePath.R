#
# 
# Author: furia
###############################################################################

unitTestTrailingForwardSlash <-
  function()
{
  tmp <- tempfile()
  path <- file.path(tmp, "foo/")
  ans <- synapseClient:::.cleanFilePath(path)
  checkTrue(attr(ans, "isDir"))
  
  checkEquals(as.character(ans),  gsub("[\\/]+", "/", path))
  
  path <- file.path(tmp, "foo///")
  ans <- synapseClient:::.cleanFilePath(path)
  checkTrue(attr(ans, "isDir"))
  
  checkEquals(as.character(ans), gsub("[\\/]+", "/", file.path(tmp, "foo/")))
  
}

unitTestTrailingBackSlash <-
  function()
{
  tmp <- tempfile()
  ## R syntax makes it so backslashes must come in pairs
  path <- file.path(tmp, "foo\\")
  ans <- synapseClient:::.cleanFilePath(path)
  checkTrue(attr(ans, "isDir"))
  
  checkEquals(as.character(ans), gsub("[\\/]+", "/", file.path(tmp, "foo/")))
  
  ## R syntax makes it so backslashes must come in pairs
  path <- file.path(tmp, "foo\\\\")
  ans <- synapseClient:::.cleanFilePath(path)
  checkTrue(attr(ans, "isDir"))
  
  checkEquals(as.character(ans), gsub("[\\/]+", "/", file.path(tmp, "foo/")))
  
}

unitTestNoTrailingSlash <-
  function()
{
  path <- file.path(tempfile(), "foo")

  ans <- synapseClient:::.cleanFilePath(path)
  checkTrue(!attr(ans, "isDir"))
}

unitTestNoTrailingDirExists <-
  function()
{
  path <- tempfile()
  dir.create(path)
  
  ans <- synapseClient:::.cleanFilePath(path)
  checkTrue(attr(ans, "isDir"))
  unlink(path)
}



