.setUp <-
	function()
{
	synapseClient:::.setCache("oldWarn", options('warn')[[1]])
	synapseClient:::.setCache("oldCacheDir", synapseCacheDir())
}

.tearDown <-
	function()
{
	options(warn=synapseClient:::.getCache("oldWarn"))
	synapseClient:::.deleteCache("oldWarn")

	for(d in synapseClient:::.getCache("fileChmod")){
		Sys.chmod(d,'0755')
		unlink(d, recursive=TRUE, force=TRUE)
	}
	synapseClient:::.deleteCache('fileChmod')

	for(d in synapseClient:::.getCache("removeDirs")){
		unlink(d, recursive=TRUE, force=TRUE)
	}
	synapseClient:::.deleteCache("removeDirs")
	synapseCacheDir(synapseClient:::.getCache("oldCacheDir"))
	synapseClient:::.deleteCache("oldCacheDir")
}

unitTestReglarFileWithSameNameExists <-
	function()
{
	dd <- tempfile()
	checkTrue(!file.exists(dd))

	## create a regular file with this name
	cat("hello world!", file=dd)

	## make sure that no error is generated --expecting a warning instead
	options(warn=0)
	synapseCacheDir(dd)

	synapseCacheDir(dd)
}

unitTestNewDirReadOnlyParent <-
	function()
{
	if(Sys.info()[['sysname']] != "Windows"){
		parent = tempfile()
		dd = tempfile(tmpdir=parent)
		synapseClient:::.setCache("fileChmod", parent)
		dir.create(parent)

		## set parent permissions to read only
		Sys.chmod(parent, "400")

		## make sure that we only get a warning
		options(warn=0)
		synapseCacheDir(dd)
		checkEquals(dd, synapseCacheDir())

		## make sure that a warning is produced
		
		synapseCacheDir(dd)
	}
}

unitTestSetCacheDirNewDirDoesNotExists <-
	function()
{
	##fail if any warnings are generated
	
	dd <- tempfile()
	checkTrue(!file.exists(dd))

	synapseCacheDir(dd)
	checkTrue(file.exists(dd))
	checkTrue(file.info(dd)$isdir)

	checkEquals(dd, synapseCacheDir())

}

unitTestSetCacheDirUseTilde <-
	function()
{
	##fail if any warnings are generated
	

	## make a cache Directory that is a subdir of ~/.synapseCache
	dd <- tempfile(,tmpdir="~/.synapseCache")

	synapseCacheDir(dd)
	checkEquals(dd, synapseCacheDir())

}




