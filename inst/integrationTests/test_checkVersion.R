
.setUp <-
  function()
{
  # set versions service endpoint
  synapseClient:::.setCache('oldVersionsEndpoint', synapseClient:::.getVersionsEndpoint())
  synapseClient:::synapseVersionsServiceEndpoint("http://dev-versions.synapse.sagebase.org/synapseRClient")
}

.tearDown <- 
  function()
{
  # set versions service endpoint
  synapseClient:::synapseVersionsServiceEndpoint(synapseClient:::.getCache('oldVersionsEndpoint'))
}

integrationTestCheckVersionLatest <- function()
{
  genericMessage<-"\n\nOn January 1, all clients will be required to upgrade to the latest version.\n"

  # latest version, should just display the 'message' field
  message<-synapseClient:::.checkLatestVersionGivenMyVersion("0.19-0")
  checkEquals(genericMessage, message)
  error<-try(synapseClient:::.checkBlackListGivenMyVersion("0.19-0"), silent=TRUE)
  checkTrue(is.null(error))
  }

integrationTestCheckVersionOld <- function()
{
   
  # old version (not blacklisted), should display message to upgrade
  message<- synapseClient:::.checkLatestVersionGivenMyVersion("0.18")
  upgradeMessage<-"Please upgrade to the latest version of the Synapse Client, 0.19-0, having the following changes/features:\nThis version includes new provenance features.\n\nOn January 1, all clients will be required to upgrade to the latest version.\n"
  checkEquals(upgradeMessage, message)
  error<-try(synapseClient:::.checkBlackListGivenMyVersion("0.18"), silent=TRUE)
  checkTrue(is.null(error))

}


integrationTestCheckVersionBlackListed <- function()
{
   # black listed version, should throw exception
  error<-try(synapseClient:::.checkBlackListGivenMyVersion("0.11"), silent=TRUE)
  checkEquals("try-error", class(error))
  blackListMessage<-"Error in synapseClient:::.checkBlackListGivenMyVersion(\"0.11\") : \n  This version of the Synapse Client, 0.11, has been disabled.  Please upgrade to the latest version, 0.19-0.\nOn January 1, all clients will be required to upgrade to the latest version.\n"
  checkEquals(blackListMessage, error[1])
}

