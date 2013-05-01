.entityFileCachePath <- 
	function(id, version) 
{
	uri <- .generateEntityUri(id, version)
	url <- .ParsedUrl(url=synapseRepoServiceEndpoint()$endpoint)
	paste(url@authority, url@path, uri, sep="")
}