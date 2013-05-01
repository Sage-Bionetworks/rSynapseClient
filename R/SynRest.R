# A collection of generic, public functions for making calls to the Synapse Rest API
# 
# Author: brucehoff
###############################################################################



synRestGET<-function(uri, endpoint=synapseServiceEndpoint("REPO")) {
  synapseGet(uri, endpoint)
}

synRestDELETE<-function(uri, endpoint=synapseServiceEndpoint("REPO")) {
  synapseDelete(uri, endpoint)
}

synRestPOST<-function(uri, body, endpoint=synapseServiceEndpoint("REPO")) {
  synapsePost(uri=uri, entity=body, endpoint=endpoint)
} 

synRestPUT<-function(uri, body, endpoint=synapseServiceEndpoint("REPO")) {
  synapsePut(uri=uri, entity=body, endpoint=endpoint)
} 

