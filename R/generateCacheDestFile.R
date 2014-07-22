.generateCacheDestFile <-
	function(url, version = NULL)
{
	parsedUrl <- .ParsedUrl(url)
  file.path(.generateCacheDestDir(url, version), parsedUrl@file)
}