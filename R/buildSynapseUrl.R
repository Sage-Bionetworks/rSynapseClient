## build a synapse url. for use by onweb function to open the entity
## page on the synapse web client.
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.buildSynapseUrl <- 
  function(entity)
{
  url <- sprintf("%s/#Lookup:%s", synapsePortalEndpoint(), entity)
}
