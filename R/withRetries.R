# execute exponential backoff
# 
# Author: bhoff
#
# fcn - the function to call, retrying on error
# isRetryable - returns true iff the value returned by 'try' indicates an error which can be retried
# finalizeResult - checks value returned by 'try' for errors and constructs the final result of this function
#
withRetries<-function(fcn,
		isRetryable,
		finalizeResult) {
	INITIAL_BACKOFF_SECONDS <- 1
	BACKOFF_MULTIPLIER <- 2 # i.e. the back off time is (initial)*[multiplier^(# retries)]
	
	maxTries<-.getCache("webRequestMaxTries")
	if (is.null(maxTries) || maxTries<1) stop(sprintf("Illegal value for maxTries %d.", maxTries))
	
	backoff<-INITIAL_BACKOFF_SECONDS
	
	for (retry in 1:maxTries) {
		fcnResult<-try(fcn(), silent=T)
		
		if (retry<maxTries && isRetryable(fcnResult)) {
			# retry
			if (!is.null(.getCache("debug")) && .getCache("debug")) {
				if (is(fcnResult, "try-error")) {
					reportableResult<-fcnResult[[1]]
				} else {
					reportableResult<-fcnResult
				}
				message("withRetries: error encountered: ", reportableResult)
			}
			Sys.sleep(backoff)
			backoff <- backoff * BACKOFF_MULTIPLIER
		} else {
			break
		}
	} # end for loop
	finalizeResult(fcnResult)
}

