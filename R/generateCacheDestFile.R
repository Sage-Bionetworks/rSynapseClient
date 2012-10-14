.generateCacheDestFile <-
	function(url, version = NULL)
{
	parsedUrl <- synapseClient:::.ParsedUrl(url)
  file.path(.generateCacheDestDir(url, version), parsedUrl@file)
}