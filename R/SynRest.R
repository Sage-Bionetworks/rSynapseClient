# A collection of generic, public functions for making calls to the Synapse Rest API
# 
# Note:  As a convenience, if the 'body' of POST or PUT extends SimplePropertyOwner,
# we convert it to a list before passing it on.
#
# Author: brucehoff
###############################################################################



synRestGET<-function(uri, endpoint=synapseRepoServiceEndpoint()) {
  if (class(endpoint)=="list") endpointObject<-endpoint else endpointObject<-parseEndpoint(endpoint)
  synapseGet(uri, endpointObject)
}

synRestDELETE<-function(uri, endpoint=synapseRepoServiceEndpoint()) {
  if (class(endpoint)=="list") endpointObject<-endpoint else endpointObject<-parseEndpoint(endpoint)
  synapseDelete(uri, endpointObject)
}

synRestPOST<-function(uri, body, endpoint=synapseRepoServiceEndpoint()) {
  if (class(endpoint)=="list") endpointObject<-endpoint else endpointObject<-parseEndpoint(endpoint)
  if (is(body, "SimplePropertyOwner")) body<-as.list.SimplePropertyOwner(body)
  synapsePost(uri=uri, entity=body, endpoint=endpointObject)
} 

synRestPUT<-function(uri, body, endpoint=synapseRepoServiceEndpoint()) {
  if (class(endpoint)=="list") endpointObject<-endpoint else endpointObject<-parseEndpoint(endpoint)
  if (is(body, "SimplePropertyOwner")) body<-as.list.SimplePropertyOwner(body)
  synapsePut(uri=uri, entity=body, endpoint=endpointObject)
} 

