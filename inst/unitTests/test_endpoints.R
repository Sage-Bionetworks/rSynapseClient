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
  synapseClient:::.setCache('oldFileEndpoint', synapseFileServiceEndpoint())
  synapseClient:::.setCache('oldPortalEndpoint', synapsePortalEndpoint())
  synapseClient:::.setCache('oldVersionsEndpoint', synapseClient:::.getVersionsEndpoint())
  synapseClient:::sessionToken(NULL)
  hmacSecretKey(NULL)
  synapseFileServiceEndpoint("http://shoobar.com")
  synapseAuthServiceEndpoint("http://foobar.com")
  synapseRepoServiceEndpoint("http://boobar.com")
  synapsePortalEndpoint("http://barboo.com")
}


.tearDown <-
  function()
{
  synapseFileServiceEndpoint(synapseClient:::.getCache('oldFileEndpoint')$endpoint)
  synapseAuthServiceEndpoint(synapseClient:::.getCache('oldAuthEndpoint')$endpoint)
  synapseRepoServiceEndpoint(synapseClient:::.getCache('oldRepoEndpoint')$endpoint)
  synapseClient:::synapseVersionsServiceEndpoint(synapseClient:::.getCache('oldVersionsEndpoint'))
  synapsePortalEndpoint(synapseClient:::.getCache('oldPortalEndpoint')$endpoint)
  synapseClient:::sessionToken(synapseClient:::.getCache('oldSessionToken'))
  hmacSecretKey(synapseClient:::.getCache('oldHmacKey'))
  synapseClient:::authMode(synapseClient:::.getCache('oldAuthMode'))
}

unitTestSetAuth <-
  function()
{
  #checkTrue(is.null(synapseClient:::.getCache("sessionToken")))
  #checkTrue(is.null(synapseClient:::.getCache("hmacSecretKey")))
  synapseClient:::sessionToken("1234")
  hmacSecretKey("5678")
  checkEquals(synapseClient:::sessionToken(), "1234")
  checkEquals(hmacSecretKey(), "5678")
  checkEquals(synapseAuthServiceEndpoint()$endpoint, "http://foobar.com")
  synapseAuthServiceEndpoint('http://authme.com')
  checkEquals(synapseAuthServiceEndpoint()$endpoint, 'http://authme.com')
  #checkTrue(is.null(synapseClient:::.getCache("sessionToken")))
  #checkTrue(is.null(synapseClient:::.getCache("hmacSecretKey")))
}

unitTestSetFile <-
  function()
{
  checkEquals(synapseFileServiceEndpoint()$endpoint, "http://shoobar.com")
  synapseFileServiceEndpoint('http://fileme.com')
  checkEquals(synapseFileServiceEndpoint()$endpoint, 'http://fileme.com')
}

unitTestSetRepo <-
  function()
{
  #checkTrue(is.null(synapseClient:::.getCache("sessionToken")))
  #checkTrue(is.null(synapseClient:::.getCache("hmacSecretKey")))
  synapseClient:::sessionToken("1234")
  hmacSecretKey("5678")
  checkEquals(synapseClient:::sessionToken(), "1234")
  checkEquals(hmacSecretKey(), "5678")
  
  checkEquals(synapseRepoServiceEndpoint()$endpoint, "http://boobar.com")
  synapseRepoServiceEndpoint('http://repome.com')
  checkEquals(synapseRepoServiceEndpoint()$endpoint, 'http://repome.com')
  #checkTrue(is.null(synapseClient:::.getCache("sessionToken")))
  #checkTrue(is.null(synapseClient:::.getCache("hmacSecretKey")))
}

unitTestSetPortal <-
  function()
{
  #checkTrue(is.null(synapseClient:::.getCache("sessionToken")))
  #checkTrue(is.null(synapseClient:::.getCache("hmacSecretKey")))
  synapseClient:::sessionToken("1234")
  hmacSecretKey("5678")
  checkEquals(synapseClient:::sessionToken(), "1234")
  checkEquals(hmacSecretKey(), "5678")
  
  checkEquals(synapsePortalEndpoint()$endpoint, "http://barboo.com")
  synapsePortalEndpoint('http://portalme.com')
  checkEquals(synapsePortalEndpoint()$endpoint, 'http://portalme.com')
  
  ## don't log out of all we're doing is re-setting the portal endpoint
  checkEquals(synapseClient:::sessionToken(), "1234")
  checkEquals(hmacSecretKey(), "5678")
}


unitTestResetEndpoints <-
  function()
{
  #checkTrue(is.null(synapseClient:::.getCache("sessionToken")))
  #checkTrue(is.null(synapseClient:::.getCache("hmacSecretKey")))
  synapseClient:::sessionToken("1234")
  hmacSecretKey("5678")
  checkEquals(synapseClient:::sessionToken(), "1234")
  checkEquals(hmacSecretKey(), "5678")
  
  checkEquals(synapsePortalEndpoint()$endpoint, "http://barboo.com")
  checkEquals(synapseFileServiceEndpoint()$endpoint, "http://shoobar.com")
  checkEquals(synapseAuthServiceEndpoint()$endpoint, "http://foobar.com")
  checkEquals(synapseRepoServiceEndpoint()$endpoint, "http://boobar.com")	
  
  synapseResetEndpoints()	
  checkEquals(synapseRepoServiceEndpoint()$endpoint, 'https://repo-prod.prod.sagebase.org/repo/v1')
  checkEquals(synapseAuthServiceEndpoint()$endpoint, 'https://auth-prod.prod.sagebase.org/auth/v1')
  checkEquals(synapseFileServiceEndpoint()$endpoint, 'https://file-prod.prod.sagebase.org/file/v1')
  checkEquals(synapsePortalEndpoint()$endpoint, 'http://synapse.sagebase.org')
  #checkTrue(is.null(synapseClient:::.getCache("sessionToken")))
  #checkTrue(is.null(synapseClient:::.getCache("hmacSecretKey")))
}

unitTestSetVersionsEndpoint <- function()
{
  synapseClient:::synapseVersionsServiceEndpoint("http://boobar.com")
  checkEquals("http://boobar.com", synapseClient:::.getVersionsEndpoint())
}
