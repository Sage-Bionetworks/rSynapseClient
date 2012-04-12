# TODO: Add comment
# 
# Author: furia
###############################################################################

unitTestTrailingForwardSlash <-
  function()
{
  path <- "foo/"
  ans <- synapseClient:::.cleanFilePath(path)
  checkTrue(attr(ans, "isDir"))
  
  checkEquals(as.character(ans), path)
  
  path <- "foo///"
  ans <- synapseClient:::.cleanFilePath(path)
  checkTrue(attr(ans, "isDir"))
  
  checkEquals(as.character(ans), "foo/")
  
}

unitTestTrailingBackSlash <-
  function()
{
  
  ## R syntax makes it so backslashes must come in pairs
  path <- "foo\\"
  ans <- synapseClient:::.cleanFilePath(path)
  checkTrue(attr(ans, "isDir"))
  
  checkEquals(as.character(ans), "foo/")
  
  ## R syntax makes it so backslashes must come in pairs
  path <- "foo\\\\"
  ans <- synapseClient:::.cleanFilePath(path)
  checkTrue(attr(ans, "isDir"))
  
  checkEquals(as.character(ans), "foo/")
  
}

unitTestNoTrailingSlash <-
  function()
{
  path <- "foo"

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
  file.remove(path)
}



