## Methods for running the unit tests suite
##
## Author: Nicole Deflaux <nicole.deflaux@sagebase.org>
###############################################################################

.test <- function(dir=system.file("unitTests", package="synapseClient"), testFileRegexp = "^test_.*\\.R$") {
 ## .doTestConfigureNamespace()
  .runTestSuite(dir=dir, testFileRegexp=testFileRegexp, testFuncRegexp="^unitTest.+", suiteName="unit tests")
##  .undoTestConfigureNamespace()
}

.integrationTest <-
  function(dir=system.file("integrationTests", package="synapseClient"), testFileRegexp="^test_.*\\.R$")
{
##  .doTestConfigureNamespace()
  res <- .runTestSuite(dir=dir, testFileRegexp=testFileRegexp, testFuncRegexp="^integrationTest.+", suiteName="integration tests")
  return(res)
##  .undoTestConfigureNamespace()
}

.runTestSuite <-
  function(dir, testFileRegexp, testFuncRegexp, suiteName)
{

  ## Make sure its okay to run this test suite
  #.testSafety()

  .failure_details <- function(result) {
    res <- result[[1L]]
    if (res$nFail > 0 || res$nErr > 0) {
      Filter(function(x) length(x) > 0,
        lapply(res$sourceFileResults,
          function(fileRes) {
            names(Filter(function(x) x$kind != "success",
                fileRes))
          }))
    } else list()
  }

  require("RUnit", quietly=TRUE) || stop("RUnit package not found")
  RUnit_opts <- getOption("RUnit", list())
  if(synapseClient:::.getCache("debug")) {
    RUnit_opts$verbose <- 10L
    RUnit_opts$silent <- FALSE
  } else {
    RUnit_opts$verbose <- 0L
    RUnit_opts$silent <- TRUE
  }
  RUnit_opts$verbose_fail_msg <- TRUE
  options(RUnit = RUnit_opts)
  suite <- defineTestSuite(name=paste("synapseClient RUnit Test Suite", suiteName),
    dirs=dir,
    testFileRegexp=testFileRegexp,
    testFuncRegexp=testFuncRegexp,
    rngKind="default",
    rngNormalKind="default")
  result <- runTestSuite(suite)
  cat("\n\n")
  printTextProtocol(result, showDetails=FALSE)
  if (length(details <- .failure_details(result)) >0) {
    cat("\nTest files with failing tests\n")
    for (i in seq_along(details)) {
      cat("\n  ", basename(names(details)[[i]]), "\n")
      for (j in seq_along(details[[i]])) {
        cat("    ", details[[i]][[j]], "\n")
        
        # Print out the details of the error
        detailSource <- result[[1]]$sourceFileResults[[names(details)[[i]]]][[details[[i]][[j]]]]
        errorMessage <- detailSource$msg
        errorMessage <- gsub('\n', "", errorMessage)
        width <- 50
        errorMessage <- substring(errorMessage, 
                            seq(1, nchar(errorMessage), width), 
                            seq(width, nchar(errorMessage)+width, width))
        cat(paste("          ", errorMessage, sep=""), sep="\n")
        
        # Print out a stack trace (Note: this may print nothing because R doesn't usually save stack traces)
        stacktrace <- detailSource$traceBack
        cat("        Traceback:", paste("          ->", stacktrace, sep = ""), "", sep="\n")
      }
    }
    cat("\n\n")
  }
  result
}


# Replaces a function in the given namespace
# !!! Do NOT replace any functions in GlobalCache.R !!!
.mock <- function(funcName, replacement, namespace='synapseClient') {
    # For non-exported or non-locked functions, replacement is straightforward
    origFunc <- get(funcName, envir=asNamespace(namespace))
    if (is.null(attr(origFunc, "origDef"))) {
        attr(replacement, "origDef") <- origFunc
    } else {
        # Allow mocks to overwrite previous mocks
        attr(replacement, "origDef") <- attr(origFunc, "origDef")
    }
    assignInNamespace(funcName, replacement, namespace)
    
    # Mark the mocked function for cleanup
    .setCache(.appendMockPrefix(funcName), namespace)
}

.appendMockPrefix <- function(funcName) {
    return(paste("!Mocked_Function->", funcName, sep=""))
}

.getMockedFunction <- function(funcName, namespace='synapseClient') {
    mocked <- get(funcName, envir=asNamespace(namespace))
    if (is.null(attr(mocked, "origDef"))) {
        return(mocked) # Function is not mocked
    } else {
        return(attr(mocked, "origDef"))
    }
}

# Un-replaces the given function in the given namespace
.unmock <- function(funcName, namespace) {
    replaced <- get(funcName, envir=asNamespace(namespace))
    assignInNamespace(funcName, attr(replaced, "origDef"), namespace)
    .deleteCache(.appendMockPrefix(funcName))
}

# Searches the global cache for mocked functions and unmocks them
.unmockAll <- function() {
    mocked <- objects(envir=as.environment.GlobalCache(new("GlobalCache")))
    for (mockKey in mocked) {
        mock.matches <- regexec("!Mocked_Function->(.*)", mockKey)
        mock.matches <- unlist(regmatches(mockKey, mock.matches))
        if (length(mock.matches) != 2) {
            next
        }
        
        funcName <- mock.matches[2]
        namespace <- .getCache(mockKey)
        .unmock(funcName, namespace)
    }
}

.intercept <- function (victimName, funcName, replacement) {
    # To replace a locked function, shim in an environment for cleanup later
    # 'intercept' should be the function name who's call to 'funcName' is being intercepted
    func.env <- environment(get(victimName))
    orig.parent <- parent.env(func.env)
    mock.env <- new.env(parent=orig.parent)
    assign(funcName, replacement, envir=mock.env)
    parent.env(func.env) <- mock.env
    
    # Mark the intercepted function for cleanup
    .setCache(.appendInterceptPrefix(victimName), funcName)
}

.appendInterceptPrefix <- function(victimName) {
    return(paste("!Intercepted_Function->", victimName, sep=""))
}

# Undoes the shim-environment
.unintercept <- function(victimName, funcName) {
    func.env <- environment(get(victimName))
    mock.env <- parent.env(func.env)
    assign(funcName, NULL, envir=mock.env)
    orig.parent <- parent.env(mock.env)
    parent.env(func.env) <- orig.parent
    .deleteCache(.appendInterceptPrefix(victimName))
}

.uninterceptAll <- function() {
    mocked <- objects(envir=as.environment.GlobalCache(new("GlobalCache")))
    for (mockKey in mocked) {
        intercept.matches <- regexec("!Intercepted_Function->(.*)", mockKey)
        intercept.matches <- unlist(regmatches(mockKey, intercept.matches))
        if (length(intercept.matches) != 2) {
            next
        }
        
        victimName <- intercept.matches[2]
        funcName <- .getCache(mockKey)
        .unintercept(victimName, funcName)
    }
}

testHMACSignature <-
  function()
{
  # in this case the secret key has a null
  userId <- "matt.furia@sagebase.org"
  uri <- "/services-repository-0.7-SNAPSHOT/repo/v1/project"
  timeStampString <- "2011-09-28T13:31:16.90-0700"
  base64EncodedSecretKey <- "GX/ZL7HPHOO4MvEUdADASuY8zmdKR10vINnNZ1lPLwkZZI/BYgl+FUyw35/NEhTFB1ZwGVQbVqVAA6w/0nbUYQ=="
  signature<-.generateHMACSignature(uri, timeStampString, userId, base64EncodedSecretKey)
  expected<-"rIi/ut4jdroxisbMsbvV0fuW9eQ="
  if (signature!=expected) stop("Error in testHMACSignature")

  # in this case the HMAC signature has a null
  userId <- "matt.furia@sagebase.org"
  uri <- "/repo/v1/entity/17428/type"
  timeStampString <- "2011-10-07T00:09:40.44-0700"
  base64EncodedSecretKey <- "pDfk2KtmuvwFNKJzOn16ZfIY5qbSDebNFpTPHd6DuGemivMLWCV3tBFny6qGQ3luwXW7Q13IL3SUYC29mXeKdg=="
  signature<-.generateHMACSignature(uri, timeStampString, userId, base64EncodedSecretKey)
  expected<-"tVJCGnmI/8nh+W6CVgAJpv902Ns="
  if (signature!=expected) stop("Error in testHMACSignature")
}
