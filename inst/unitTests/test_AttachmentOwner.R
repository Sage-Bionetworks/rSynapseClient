.setUp <-
    function()
{
  synapseClient:::.setCache("oldWarn", options("warn")[[1]])
  options(warn = 2L)
}

.tearDown <-
    function()
{
  options(warn = synapseClient:::.getCache("oldWarn"))
  synapseClient:::.deleteCache('oldWarn')
}

unitTestConstructor <-
	function()
{
	own <- synapseClient:::AttachmentOwner()
	checkTrue(grepl(gsub("[/\\]+", "/", normalizePath(tempdir())), own@fileCache$cacheDir))
}
unitTestConstructorSpecifyCacheDir <-
	function()
{
	cacheDir <- file.path(tempdir(), "attachments")
	own <- synapseClient:::AttachmentOwner(cacheDir)
	checkEquals(own@fileCache$cacheRoot, gsub("[/\\]+", "/", normalizePath(cacheDir)))
}

