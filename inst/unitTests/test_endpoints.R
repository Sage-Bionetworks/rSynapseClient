## Unit tests for setting/getting service endtpoints
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.setUp <-
  function()
{
  synapseClient:::.setCache('oldAuthMode', synapseClient:::authMode())
  synapseClient:::.setCache('oldSessionToken', synapseClient:::.getCache("sessionToken"))
  synapseClient:::.setCache('oldHmacKey', synapseClient:::.getCache("hmacSecretKey"))
  synapseClient:::.setCache('oldRepoEndpoint', synapseRepoServiceEndpoint())
  synapseClient:::.setCache('oldAuthEndpoint', synapseAuthServiceEndpoint())
  synapseClient:::.setCache('oldPortalEndpoint', synapsePortalEndpoint())
  synapseClient:::sessionToken(NULL)
  hmacSecretKey(NULL)
  synapseAuthServiceEndpoint("http://foobar.com")
  synapseRepoServiceEndpoint("http://boobar.com")
  synapsePortalEndpoint("http://barboo.com")
}


.tearDown <-
  function()
{
  synapseAuthServiceEndpoint(synapseClient:::.getCache('oldAuthEndpoint'))
  synapseRepoServiceEndpoint(synapseClient:::.getCache('oldRepoEndpoint'))
  synapsePortalEndpoint(synapseClient:::.getCache('oldPortalEndpoint'))
  synapseClient:::sessionToken(synapseClient:::.getCache('oldSessionToken'))
  hmacSecretKey(synapseClient:::.getCache('oldHmacKey'))
  synapseClient:::authMode(synapseClient:::.getCache('oldAuthMode'))
}

unitTestSetAuth <-
  function()
{
  checkTrue(is.null(synapseClient:::.getCache("sessionToken")))
  checkTrue(is.null(synapseClient:::.getCache("hmacSecretKey")))
  synapseClient:::sessionToken("1234")
  hmacSecretKey("5678")
  checkEquals(synapseClient:::sessionToken(), "1234")
  checkEquals(hmacSecretKey(), "5678")
  checkEquals(synapseAuthServiceEndpoint(), "http://foobar.com")
  synapseAuthServiceEndpoint('http://authme.com')
  checkEquals(synapseAuthServiceEndpoint(), 'http://authme.com')
  checkTrue(is.null(synapseClient:::.getCache("sessionToken")))
  checkTrue(is.null(synapseClient:::.getCache("hmacSecretKey")))
}

unitTestSetRepo <-
  function()
{
  checkTrue(is.null(synapseClient:::.getCache("sessionToken")))
  checkTrue(is.null(synapseClient:::.getCache("hmacSecretKey")))
  synapseClient:::sessionToken("1234")
  hmacSecretKey("5678")
  checkEquals(synapseClient:::sessionToken(), "1234")
  checkEquals(hmacSecretKey(), "5678")
  
  checkEquals(synapseRepoServiceEndpoint(), "http://boobar.com")
  synapseRepoServiceEndpoint('http://repome.com')
  checkEquals(synapseRepoServiceEndpoint(), 'http://repome.com')
  checkTrue(is.null(synapseClient:::.getCache("sessionToken")))
  checkTrue(is.null(synapseClient:::.getCache("hmacSecretKey")))
}

unitTestSetPortal <-
  function()
{
  checkTrue(is.null(synapseClient:::.getCache("sessionToken")))
  checkTrue(is.null(synapseClient:::.getCache("hmacSecretKey")))
  synapseClient:::sessionToken("1234")
  hmacSecretKey("5678")
  checkEquals(synapseClient:::sessionToken(), "1234")
  checkEquals(hmacSecretKey(), "5678")
  
  checkEquals(synapsePortalEndpoint(), "http://barboo.com")
  synapsePortalEndpoint('http://portalme.com')
  checkEquals(synapsePortalEndpoint(), 'http://portalme.com')
  
  ## don't log out of all we're doing is re-setting the portal endpoint
  checkEquals(synapseClient:::sessionToken(), "1234")
  checkEquals(hmacSecretKey(), "5678")
}


unitTestResetEndpoints <-
  function()
{
  checkTrue(is.null(synapseClient:::.getCache("sessionToken")))
  checkTrue(is.null(synapseClient:::.getCache("hmacSecretKey")))
  synapseClient:::sessionToken("1234")
  hmacSecretKey("5678")
  checkEquals(synapseClient:::sessionToken(), "1234")
  checkEquals(hmacSecretKey(), "5678")
  
  checkEquals(synapsePortalEndpoint(), "http://barboo.com")
  checkEquals(synapseAuthServiceEndpoint(), "http://foobar.com")
  checkEquals(synapseRepoServiceEndpoint(), "http://boobar.com")	
  
  synapseResetEndpoints()	
  checkEquals(synapseRepoServiceEndpoint(), 'https://repo-alpha.sagebase.org/repo/v1')
  checkEquals(synapseAuthServiceEndpoint(), 'https://auth-alpha.sagebase.org/auth/v1')
  checkEquals(synapsePortalEndpoint(), 'http://synapse.sagebase.org')
  checkTrue(is.null(synapseClient:::.getCache("sessionToken")))
  checkTrue(is.null(synapseClient:::.getCache("hmacSecretKey")))
}
