## Set and get data location preferences
## 
## Author: Nicole Deflaxu <nicole.deflaux@sagebase.org>
###############################################################################

synapseDataLocationPreferences <- function(locationTypes){
  if(missing(locationTypes)){
    return(.getCache("synapseDataLocationPreferences"))
  }
  if(!all(locationTypes %in% kSupportedDataLocationTypes)){
    ind <- which(!(locationTypes %in% kSupportedDataLocationTypes))
    stop(paste("unsupported repository location(s):", locationTypes[ind]))
  }
  .setCache("synapseDataLocationPreferences", locationTypes)
}
