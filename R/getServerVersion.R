#
# returns the version of the Synapse server
#
getServerVersion<-function() {
  synapseGet("/version", anonymous=T)$version
}
