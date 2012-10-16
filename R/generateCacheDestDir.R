.generateCacheDestDir <-
	function(url, version = NULL)
{
	parsedUrl <- synapseClient:::.ParsedUrl(url)
  destdir <- file.path(synapseCacheDir(), gsub("^/", "", parsedUrl@pathPrefix))
  if(!is.null(version))
     destdir <- file.path(destdir, "version", version)
  path.expand(destdir)
}
