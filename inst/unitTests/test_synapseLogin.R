# TODO: Add comment
# 
###############################################################################
.setUp <-
        function()
{
        synapseClient:::.setCache('oldAuthMode', synapseClient:::authMode())
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
        synapseClient:::authMode(synapseClient:::.getCache('oldAuthMode'))
}

unitTestNotLoggedInToken <- function(){
    synapseClient:::authMode('auth')
    gotException <- FALSE
    tryCatch(createEntity(Dataset(list(name='foo'))), 
             error = function(e) {
                 gotException <- TRUE
                 checkTrue(grepl("please log into Synapse", e))
                 }	)
}

unitTestNotLoggedInHmac <- function(){
    synapseClient:::authMode('hmac')
    gotException <- FALSE
    tryCatch(createEntity(Dataset(list(name='foo'))),
             error = function(e) {
                 gotException <- TRUE
                 checkTrue(grepl("Please Authenticate", e))
                 }      )
}
