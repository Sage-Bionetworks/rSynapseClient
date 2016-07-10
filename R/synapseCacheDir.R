## get/set the Synapse cache directory locations
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseCacheDir <- function(cacheDir){
  if(missing(cacheDir)){
    return(.getCache("synapseCacheDir"))
  }
  if(is.null(cacheDir))
  	cacheDir <- "~/.synapseCache"

  if(!is.null(cacheDir) && cacheDir != "" && !file.exists(cacheDir)){
  	tryCatch({
  			dir.create(cacheDir, recursive=TRUE)
  		},
  		error = function(e){
  			warning(sprintf("Unable to set cache directory to %s: e", cacheDir, e))
  		}
  	)
  }
  isdir <- file.info(cacheDir)$isdir
  if(is.na(isdir)){
  	## don't warn
  } else if(file.access(cacheDir, 2) != 0L){
  	warning(sprintf("The requested cache directory '%s' is not writable so some features will\nnot be functional. Use the 'synapseCacheDir()' function to set a writable cache directory.", cacheDir))
  } else if(!isdir){
  	warning(sprintf("Could not set cache directory to '%s' since a regular file with that name\n already exists. Some features will\nnot be functional. Use the 'synapseCacheDir()'\nfunction to set a writable cache directory.", cacheDir))
  } 
  .setCache("synapseCacheDir", cacheDir)
}
