# A collection of generic, public functions for making calls to the Synapse Rest API
# 
# Author: brucehoff
###############################################################################



synRestGET<-function(uri, endpoint=synapseServiceEndpoint("REPO")) {
  if (class(endpoint)=="list") endpointObject<-endpoint else endpointObject<-parseEndpoint(endpoint)
  synapseGet(uri, endpointObject)
}

synRestDELETE<-function(uri, endpoint=synapseServiceEndpoint("REPO")) {
  if (class(endpoint)=="list") endpointObject<-endpoint else endpointObject<-parseEndpoint(endpoint)
  synapseDelete(uri, endpointObject)
}

synRestPOST<-function(uri, body, endpoint=synapseServiceEndpoint("REPO")) {
  if (class(endpoint)=="list") endpointObject<-endpoint else endpointObject<-parseEndpoint(endpoint)
  synapsePost(uri=uri, entity=body, endpoint=endpointObject)
} 

synRestPUT<-function(uri, body, endpoint=synapseServiceEndpoint("REPO")) {
  if (class(endpoint)=="list") endpointObject<-endpoint else endpointObject<-parseEndpoint(endpoint)
  synapsePut(uri=uri, entity=body, endpoint=endpointObject)
} 

