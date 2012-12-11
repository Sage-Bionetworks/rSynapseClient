#
# Show Synapse Terms of Use in web browser
#
touOnWeb<-function() {
  utils::browseURL(sprintf("%s/termsOfUse.html", synapseAuthServiceEndpoint()))
}