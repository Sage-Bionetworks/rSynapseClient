.generateEntityUri <- 
	function(id, version=NULL) 
{
	uri <- paste("/entity/", id, sep="")
	if (!is.null(version)) uri <- paste(uri, "/version/", version, sep="")
	uri
}