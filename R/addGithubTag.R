## TRY A LITTLE GIT INTEGRATION
setMethod(
  f = "addGithubTag",
  signature = signature("Code", "character"),
  definition = function(entity, url){
    
    ## CHECK TO MAKE SURE THERE IS NO LOCATION FOR THIS CODE OBJECT YET
    if(!is.null(propertyValue(entity, "locations"))){
      stop("This Code Entity already has a location.")
    }
    
    class(entity) <- "GithubCode"
    
    destfile <- .curlWriterDownload(url=url)
    
    if(!exists("destfile")){
      stop(paste("Downloaded file", destfile, "does not exists"))
    }
    
    md5 <- as.character(tools::md5sum(destfile))
    file.remove(destfile)
    
    ## SET LOCATIONS AND MD5 PROPERTIES
    propertyValue(entity, "locations") <- list(c(path=url, type="external"))
    propertyValue(entity, "md5") <- md5
    annotValue(entity, "githubRepo") <- dirname(dirname(url))
    annotValue(entity, "githubRepoTag") <- basename(url)
    
    entity
  }
)

