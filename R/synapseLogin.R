## Login to synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

setApiCredentials <-
    function(username, secretKey)
{
  synapseClient:::userName(username)
  synapseClient:::hmacSecretKey(secretKey)
}

synapseLogin <- 
  function(username = "", password = "", mode = "auth")
{
  ## username and password must both be of length
  if(any(length(username) !=1 || length(password) != 1))
    stop("Please provide a single username and password")
  mode <- tolower(mode)
  if(!(mode %in% c("auth", "hmac")))
    stop(sprintf("Invalid authentication mode: %s", mode))
  
  ## replace nulls with empty strings
  if(is.null(username))
    username <- ""
  if(is.null(password))
    password <- ""
  
  credentials <- list(username = username, password = password, mode=mode)
  .doAuth(credentials)
  .setCache("sessionTimestamp", Sys.time())
}

.doHmac <-
  function(credentials)
{
  ## constants
  kService <- "/secretKey"
  ## end constants
  
  if(is.null(credentials$username) || credentials$username == "")
    stop("Must provide username in hmac mode")
  
  entity <- list()
  entity$email <- credentials$username
  
  ## Request the secret key
  response <- synapseGet(uri = kService, 
    service="AUTH",
    entity = entity, 
    anonymous = FALSE
  )
  
  ## Cache the sessionToken. No need to check validity since it was just created
  hmacSecretKey(response$secretKey)
}

.doAuth <- 
  function(credentials)
{
  if(all(credentials$username != "" && credentials$password != "")){
    ## username and password were both provided log the use in
    message(.doLogin(credentials))
  }else{
    ## check to see if the "useTk" option is set
    useTk <- .getCache("useTk")
    if(is.null(useTk)){ ## useTk isn't set
      useTk <- .decideTk()
    }
    
    ##initiate login.
    if(useTk){
      message(.doTkLogin(credentials))
    }else{
      message(.doTerminalLogin(credentials))
    }
  }
}

.decideTk <-
  function()
{
  ## if this is a unix terminal, do a terminal login
  if(tolower(.Platform$GUI) == "rstudio"){
    useTk <- FALSE
  }else if(tolower(.Platform$OS.type) == "unix"){
    if(tolower(.Platform$GUI) %in% c("aqua", "x11")){
      ## don't use tk for terminal or for CRAN R GUI
      ## the CRAN R GUI locks up when tk is initialized
      ## if it is not installed properly
      useTk <- FALSE
    }else{
      ## another GUI is being used. check to see if Tk is
      ## installed
      useTk <- .hasTk()
    }
    
  }else{
    ## this is a non OSX/unix OS. Tk installation that comes with
    ## R should work.
    useTk <- .hasTk()
  }
  useTk
}

.doTerminalLogin <-
  function(credentials)
{
  credentials <- .terminalGetCredentials(credentials)
  if(!is.null(credentials))
    .doLogin(credentials)
}

.doTkLogin <-
  function(credentials)
{
  credentials <- tryCatch(
    .tkGetCredentials(credentials),
    error = function(e){
      .setCache("useTk", FALSE)
      return(.doTerminalLogin(credentials))
    }
  )
  if(!is.null(credentials))
    .doLogin(credentials)
}

.doLogin <- 
  function(credentials)
{
  ## constants
  kService <- "/session"
  ## end constants
  
  
  ## get auth service endpoint and prefix from memory cache
  host <- .getAuthEndpointLocation()
  path <- .getAuthEndpointPrefix()
  
  entity <- list()
  entity$email <- credentials$username
  entity$password <- credentials$password
  
  checkBlackList()
  checkLatestVersion()
  
  ## Login and check for success
  response <- synapsePost(uri = kService, 
    entity = entity, 
    service = "AUTH",
    anonymous = TRUE
  )
  
  ## Cache the sessionToken. No need to check validity since it was just created
  sessionToken(response$sessionToken, checkValidity=FALSE)
  userName(credentials$username)
  if(credentials$mode == "hmac"){
    tryCatch(.doHmac(credentials),
      error = function(e){
        synapseLogout(localOnly=TRUE)
        stop(e)
      }
    )
  }

  ## try to fetch user's display name from profile for a proper greeting
  greetingName <- tryCatch({
      synapseGet(uri="/userProfile")$displayName
    },
    error = function(e) {
      credentials$username
    })

  sprintf("Welcome %s!", greetingName)
}

synapseLogout <-
  function(localOnly=FALSE)
{
  ## constants
  kService <- "/session"
  ## end constants
  
  ## get auth service endpoint and prefix from memory cache
  host = .getAuthEndpointLocation()
  path = .getAuthEndpointPrefix()
  
  entity <- list(sessionToken = sessionToken())
  
  if(!localOnly){
    response <- synapseDelete(uri = kService,
      entity = entity,
      service="AUTH"
    )
  }
  hmacSecretKey(NULL)
  sessionToken(NULL)
  .setCache("username", NULL)
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

hmacSecretKey <- 
  function(secretKey)
{
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

.stuffHeaderAuth <-
  function(header)
{
  c(header, sessionToken = sessionToken())
}

# uri is, for example, /repo/v1/dataset
.stuffHeaderHmac <-
  function(header, uri)
{
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

authMode <- 
  function(mode)
{
  kDefaultMode <- "auth"
  if(missing(mode)){
    mode <- .getCache("authMode")
    if(is.null(mode))
      mode <- kDefaultMode
    return(mode)
  }
  .setCache("authMode", mode)
}



