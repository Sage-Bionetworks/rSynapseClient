## Unit test synapseLogin
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.setUp <-
  function()
{
  synapseClient:::.setCache('oldSessionToken', synapseClient:::.getCache("sessionToken"))
  synapseClient:::.setCache('oldHmacKey', synapseClient:::.getCache("hmacSecretKey"))
  synapseClient:::sessionToken(NULL)
  hmacSecretKey(NULL)
}


.tearDown <-
  function()
{
  synapseClient:::sessionToken(synapseClient:::.getCache('oldSessionToken'))
  hmacSecretKey(synapseClient:::.getCache('oldHmacKey'))
}

unitTestNotLoggedInHmac <- function(){
  gotException <- FALSE
  tryCatch(getEntity(Project(list(id='bar'))),
    error = function(e) {
      gotException <- TRUE
      checkTrue(grepl("Please authenticate", e))
    }      )
}
