# TODO: Add comment
# 
# Author: furia
###############################################################################


synapseEntityTypeMap <-
  function()
{
  synapseEntityTypeMap<-.getCache("synapseEntityTypeMap")
  if (is.null(synapseEntityTypeMap)) synapseEntityTypeMap<-list()
  synapseEntityTypeMap
}

getSynapseTypeFromClass <-
  function(class)
{
  map <- synapseEntityTypeMap()
  indx <- which(map == class)
  if(length(indx) == 0L)
    return(NULL)
  names(map[indx])
}

getClassFromSynapseEntityType <-
  function(type)
{
  map <- synapseEntityTypeMap()
  map[[type]]
}