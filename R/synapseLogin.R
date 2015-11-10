## Login to synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
##         Joseph Wu <joseph.wu@sagebase.org>
###############################################################################

setApiCredentials <-
    function(username, secretKey)
{
  userName(username)
  hmacSecretKey(secretKey)
}

synapseLogin <- function(username = "", password = "", sessionToken = "", apiKey = "", rememberMe = FALSE) {
    ## parameters must be of length 1
    if(any(length(username) !=1 || length(password) != 1 || length(sessionToken) != 1 || length(apiKey) != 1))
        stop("Please provide a single username and password")
    
    checkBlackList()
    checkLatestVersion()
    
    ## replace nulls with empty strings
    if(is.null(username))     username <- ""
    if(is.null(password))     password <- ""
    if(is.null(sessionToken)) sessionToken <- ""
    if(is.null(apiKey))       apiKey <- ""
    
    credentials <- list(username = username, password = password, 
                        sessionToken = sessionToken, apiKey = apiKey)
    synapseLogout(localOnly=TRUE, silent=TRUE)
    .doAuth(credentials)
  
	if (rememberMe) {
		json <- .readSessionCache()
		json[[userName()]] <- hmacSecretKey()
        json[["<mostRecent>"]] <- userName()
		.writeSessionCache(json)
	}
}

.doAuth <- function(credentials) {
	## Tries to authenticate in the following order:
    ## - Supplied username and password
    if (all(credentials$username != "" && credentials$password != "")) {
        userName(credentials$username)
        .getSessionToken(credentials)
        .doHmac()
	
    ## - Supplied username and API key (base 64 encoded)
    } else if (all(credentials$username != "" && credentials$apiKey != "")) {
        userName(credentials$username)
        hmacSecretKey(credentials$apiKey)

    ## - Supplied session token
    } else if (all(credentials$sessionToken != "")) {
        .refreshSessionToken(credentials)
        sessionToken(credentials$sessionToken)
        .doUsername(credentials)
        .doHmac()
    
    ## Need to read from the session cache
    } else {
        sessions <- .readSessionCache()
        
        ## - Most recent username and API key
        if (all(credentials$username == "" && "<mostRecent>" %in% names(sessions))) {
            credentials$username <- sessions[["<mostRecent>"]]
        }
        
        ## - Supplied username and cached API key
        if (all(credentials$username != "" && credentials$username %in% names(sessions))) {
            userName(credentials$username)
            hmacSecretKey(sessions[[userName()]])
        
        ## Need to read from the config file
        } else {
            config <- try(ConfigParser())
            if (class(config) != "try-error") {
                if (all(Config.hasOption(config, "authentication", "username"))) {
                    userName(Config.getOption(config, "authentication", "username"))
                    
                    ## - Username in the configuration file and cached API key
                    if (all(userName() %in% names(sessions))) {
                        hmacSecretKey(sessions[[userName()]])
                    
                    ## - Username and API key in the configuration file
                    } else if (all(Config.hasOption(config, "authentication", "apikey"))) {
                        hmacSecretKey(Config.getOption(config, "authentication", "apikey"))

                    ## - Username and password in the configuraton file
                    } else if (all(Config.hasOption(config, "authentication", "password"))) {
                        .getSessionToken(list(username = userName(), 
                                              password = Config.getOption(config, "authentication", "password")))
                        .doHmac()
                    }

                ## - Session token in the configuration file
                } else if (all(Config.hasOption(config, "authentication", "sessiontoken"))) {
                    sessionToken(Config.getOption(config, "authentication", "sessiontoken"))
                    .doHmac()
                }
            }
            
            # Resort to terminal/Tk login if all else has failed
            # i.e. there's still no secret key
            if (class(try(hmacSecretKey(), silent=TRUE)) == "try-error") {
                ## Check to see if the "useTk" option is set
                useTk <- .getCache("useTk")
                if (is.null(useTk)) {
                    useTk <- .decideTk()
                }

                ## Initiate login
                if (useTk) {
                    message(.doTkLogin(credentials))
                }else{
                    message(.doTerminalLogin(credentials))
                }
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
  
    response <- synapsePost(uri = kService, 
        entity = entity, 
        endpoint = synapseServiceEndpoint("AUTH"),
        anonymous = TRUE
    )
    sessionToken(response$sessionToken)
    return(response$sessionToken)
}

.refreshSessionToken <- function(credentials) {
    ## Refreshes the session token so that it can be used for another 24 hours
    kService <- "/session"

    entity <- list()
    entity$sessionToken <- credentials$sessionToken

    response <- synapsePut(uri =kService, 
        entity=entity, 
        endpoint=synapseServiceEndpoint("AUTH"), 
        anonymous=TRUE)
}

.doHmac <- function() {
    ## Use a session token to fetch an API key
    kService <- "/secretKey"

    ## Request the secret key
    response <- synapseGet(uri = kService, 
        endpoint=synapseServiceEndpoint("AUTH"),
        anonymous = FALSE
    )

    hmacSecretKey(response$secretKey)
}

.doUsername <- function(credentials) {
    userName(synapseGet(uri="/userProfile")$userName)
}


.readSessionCache <- function() {
	sessionFile <- .sessionFile()
	if (!file.exists(sessionFile)) {
		return(list())
	}
	
	return(synFromJson(readFile(sessionFile)))
}

.writeSessionCache <- function(json) {
	sessionFile <- .sessionFile()
	writeLines(toJSON(json), sessionFile)
}

.sessionFile <- function() {
	return(paste(synapseCacheDir(), "/.session", sep=""))
}

.decideTk <- function() {
    ## If this is a UNIX terminal, do a terminal login
    if (tolower(.Platform$GUI) == "rstudio") {
        useTk <- FALSE
        
    } else if (tolower(.Platform$OS.type) == "unix") {
        if (tolower(.Platform$GUI) %in% c("aqua", "x11")) {
            ## Don't use Tk for terminal or for CRAN R GUI
            ## the CRAN R GUI locks up when Tk is initialized
            ## if it is not installed properly
            useTk <- FALSE
        } else {
            ## Another GUI is being used. Check to see if Tk is installed
            useTk <- .hasTk()
        }

    ## This is a non OSX/UNIX OS. Tk installation that comes with R should work.
    } else {
        useTk <- .hasTk()
    }
    return(useTk)
}

.doTerminalLogin <- function(credentials) {
    credentials <- .terminalGetCredentials(credentials)
    if(!is.null(credentials)) {
        .doAuth(credentials)
    }
}

.doTkLogin <- function(credentials) {
    credentials <- tryCatch(
        .tkGetCredentials(credentials),
        error = function(e){
            .setCache("useTk", FALSE)
            return(.doTerminalLogin(credentials))
        }
    )
    if(!is.null(credentials)) {
        .doAuth(credentials)
    }
}

.doWelcome <- function() {  
    ## try to fetch user's display name from profile for a proper greeting
    greetingName <- synapseGet(uri="/userProfile")$userName

    message(sprintf("Welcome %s!", greetingName))
}

synapseLogout <- function(localOnly=FALSE, forgetMe=FALSE, silent=FALSE) {
    kService <- "/session"

    # Remove the HMAC key so that the session token is used
    hmacSecretKey(NULL)

    if (!localOnly && !is.null(sessionToken())) {
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
    sessionToken(NULL)
    
    if (!silent) {
        message("Goodbye.")
    }
}

invalidateAPIKey <- function() {
    kService <- "/secretKey"
    
    response <- synapseDelete(uri = kService,
        endpoint = synapseServiceEndpoint("AUTH")
    )
    
    synapseLogout(localOnly=TRUE, forgetMe=TRUE)
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

.stuffHeader <- function(header, uri) {
    # Try to fetch the HMAC header.
    # If that fails, fall back on the session token.
    # This should only happen when logging in. 
    header <- tryCatch(
        .stuffHeaderHmac(header, uri), 
        error = function(e) .stuffHeaderAuth(header))
    if (!("signature" %in% names(header) || "sessionToken" %in% names(header))) {
        stop("Please authenticate")
    }
    return(header)
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
  if(missing(secretKey)){
    key <- .getCache("base64secretKey")
    if(is.null(key))
      stop("Please authenticate")
    return(key)
  }
  .setCache("base64secretKey", secretKey)
}

sessionToken <- function(token) {
    if (missing(token))
        return(.getCache("sessionToken"))
    .setCache("sessionToken", token)
}