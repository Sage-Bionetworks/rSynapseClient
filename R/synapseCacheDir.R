## get/set the Synapse cache directory locations
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseCacheDir <- function(cacheDir){
  if(missing(cacheDir)){
    return(.getCache("synapseCacheDir"))
  }
  .setCache("synapseCacheDir", cacheDir)
}
