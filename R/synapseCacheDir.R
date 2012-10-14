## get/set the Synapse cache directory locations
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseCacheDir <- function(cacheDir){
  if(missing(cacheDir)){
    return(.getCache("synapseCacheDir"))
  }
  if(!file.access(cacheDir, 2))
  	warning(sprintf("The cache directory '%s' is not writable so some features will\nnot be functional. Use the 'synapseCacheDir()' function to set a writable cache directory.", cacheDir))
  .setCache("synapseCacheDir", cacheDir)
}
