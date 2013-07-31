## Login to synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
##         Joseph Wu <joseph.wu@sagebase.org>
###############################################################################

setApiCredentials <-
    function(username, secretKey)
{
  synapseClient:::userName(username)
  synapseClient:::hmacSecretKey(secretKey)
}

synapseLogin <- function(username = "", password = "", sessionToken = "", apiKey = "", rememberMe = FALSE) {
    ## parameters must be of length 1
    if(any(length(username) !=1 || length(password) != 1 || length(sessionToken) != 1 || length(apiKey) != 1))
        stop("Please provide a single username and password")
    
    ## replace nulls with empty strings
    if(is.null(username))     username <- ""
    if(is.null(password))     password <- ""
    if(is.null(sessionToken)) sessionToken <- ""
    if(is.null(apiKey))       apiKey <- ""
    
    credentials <- list(username = username, password = password, 
                        sessionToken = sessionToken, apiKey = apiKey)
    .doAuth(credentials)
  
	if (rememberMe) {
		json <- .readSessionCache()
		json[[userName()]] <- hmacSecretKey()
		.writeSessionCache(json)
	}
}

.doAuth <- function(credentials) {
	## Tries to authenticate in the following order:
    
    ## 1) supplied username and password
    if (all(credentials$username != "" && credentials$password != "")) {
        userName(credentials$username)
        credentials$sessionToken <- .getSessionToken(credentials)
        .doHmac(credentials)
	
    ## 2) supplied username and API key (base 64 encoded)
    } else if (all(credentials$username != "" && credentials$apiKey != "")) {
        userName(credentials$username)
        hmacSecretKey(credentials$apiKey)

    ## 3) supplied session token
    } else if (all(credentials$sessionToken != "")) {
        .doUsername(credentials)
        .doHmac(credentials)
    
    ## Need to read from the session cache
    } else {
        sessions <- .readSessionCache()
        
        ## 4) supplied username and cached API key
        if (all(credentials$username != "" && credentials$username %in% sessions)) {
            userName(credentials$username)
            hmacSecretKey(sessions[[userName()]])
        
        ## Need to read from the config file
        } else {
            config <- ConfigParser()
            
            if (all(Config.hasOption(config, "authentication", "username"))) {
                userName(Config.getOption(config, "authentication", "username"))
                
                ## 5) username in the configuration file and cached API key
                if (all(userName() %in% sessions)) {
                    hmacSecretKey(sessions[[userName()]])
                
                ## 6) username and API key in the configuration file
                } else if (all(Config.hasOption(config, "authentication", "apikey"))) {
                    hmacSecretKey(Config.getOption(config, "authentication", "apikey"))

                ## 7) username and password in the configuraton file
                } else if (all(Config.hasOption(config, "authentication", "password"))) {
                    .doHmac(list(
                        .getSessionToken(list(username = userName()
                                              password = Config.getOption(config, "authentication", "password")))
                    ))
                }

            ## 8) session token in the configuration file
            } else if (all(Config.hasOption(config, "authentication", "sessiontoken"))) {
                .doHmac(list(Config.getOption(config, "authentication", "sessiontoken")))
            
            ## Failure
            } else {
                stop("No credentials provided")
            }
        }
    }
    
    ## Print out a greeting
    .doWelcome()
}

.getSessionToken <- function(credentials) {
    ## Fetch a session token using a username and password
    kService <- "/session"
  
    entity <- list()
    entity$email <- credentials$username
    entity$password <- credentials$password
  
    checkBlackList()
    checkLatestVersion()
  
    response <- synapsePost(uri = kService, 
        entity = entity, 
        endpoint = synapseServiceEndpoint("AUTH"),
        anonymous = TRUE
    )
    sessionToken(response$sessionToken)
    return(response$sessionToken)
}

.doHmac <- function(credentials) {
    ## Use a session token to fetch an API key
    kService <- "/secretKey"

    if(is.null(credentials$username) || credentials$username == "")
        stop("Must provide username in hmac mode")

    entity <- list()
    entity$email <- credentials$username

    ## Request the secret key
    response <- synapseGet(uri = kService, 
        endpoint=synapseServiceEndpoint("AUTH"),
        entity = entity, 
        anonymous = FALSE
    )

    hmacSecretKey((response$secretKey)
}

.doUsername <- function(credentials) {
    userName(synapseGet(uri="/userProfile")$userName)
}


.readSessionCache <- function() {
	sessionFile <- .sessionFile
	if (!file.exists(sessionFile)) {
		return(list())
	}
	
	return(fromJSON(readFile(sessionFile)))
}

.writeSessionCache <- function(json) {
	sessionFile <- .sessionFile
	file.writeLines(toJSON(json), sessionFile)
}

.sessionFile <- function() {
	return(paste(synapseCacheDir(), "/.session", sep=""))
}

.doWelcome <- function() {  
    ## try to fetch user's display name from profile for a proper greeting
    greetingName <- tryCatch({
        synapseGet(uri="/userProfile")$displayName
    },
        error = function(e) {
        userName()
    })

    message(sprintf("Welcome %s!", greetingName))
}

synapseLogout <- function(localOnly=FALSE, forgetMe=FALSE) {
    kService <- "/secretKey"

    if (!localOnly){
        response <- synapseDelete(uri = kService,
            endpoint = synapseServiceEndpoint("AUTH")
        )
    }
    
    ## Global logouts invalidate API keys, so local copies must be deleted
    if (forgetMe || !localOnly) {
        sessions <- .readSessionCache()
        sessions <- subset(sessions, names(sessions) != userName())
        .writeSessionCache(sessions)
    }
    
    userName(NULL)
    hmacSecretKey(NULL)
    sessionToken(NULL)
    message("Goodbye.")
}

.generateHMACSignature<-function(url, timestamp, username = userName(), base64EncodedSecretKey= hmacSecretKey(), algo="sha1") {
  data<-paste(username, url, timestamp, sep="")
  #Base64 decode the key
  secretKey <- base64(base64EncodedSecretKey, encode=FALSE, "raw")
  #create the HMACSHA1 hash
  hash<-hmac(secretKey, data, algo=algo, raw=TRUE)
  #Base64 encode the result
  base64(hash, encode=TRUE)
}

.stuffHeaderAuth <- function(header) {
  c(header, sessionToken = sessionToken())
}

# uri is, for example, /repo/v1/dataset
.stuffHeaderHmac <- function(header, uri) {
  timestamp <- .nowAsString()
  ## this is a hack. drop leap seconds
  timestamp <- gsub(":6[01][\\.]", ":59.", timestamp)
  
  c(header, userId = userName(), 
    signatureTimestamp = timestamp, 
    signature = .generateHMACSignature(uri, timestamp)
  )
}

userName <- 
  function(name)
{
  if(missing(name))
    return(.getCache("username"))
  .setCache("username", name)
}

hmacSecretKey <- function(secretKey) {
  kAuthMode <- "hmac"
  if(missing(secretKey)){
    key <- .getCache("base64secretKey")
    if(is.null(key))
      stop("Please Authenticate")
    return(key)
  }
  .setCache("base64secretKey", secretKey)
  authMode(kAuthMode)
}

sessionToken <- function(token) {
    if (missing(token))
        return(.getCache("sessionToken"))
    .setCache("sessionToken", token)
}