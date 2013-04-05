#
# 
# Author: furia
###############################################################################

synapseObjectCache <- function(cacheDir){
  if(missing(cacheDir)){
    return(.getCache("rObjCacheDir"))
  }
  .setCache("rObjCacheDir", cacheDir)
}

