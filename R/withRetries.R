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
	backoff<-initialBackOffSeconds()
	
	startTime<-Sys.time()
	maxWaitTime<-.getCache("maxWaitDiffTime")
	if (is.null(maxWaitTime)) stop("Missing value for maxWaitDiffTime.")
	
	while (Sys.time()-startTime<maxWaitTime) {
		fcnResult<-try(fcn(), silent=T)
		
		if (isRetryable(fcnResult)) {
			# retry
			if (is(fcnResult, "try-error")) {
				reportableResult<-fcnResult[[1]]
			} else {
				reportableResult<-fcnResult
			}
			maxWaitTimeRemaining<-max(0,maxWaitTime-(Sys.time()-startTime))
			sleeptime<-min(backoff, maxWaitTimeRemaining)
			message(sprintf("Error encountered: %s. Will wait for %s seconds then retry. Press CTRL+C to quit.", 
							reportableResult, sleeptime))
			Sys.sleep(sleeptime)
			backoff <- increaseBackoff(backoff)
		} else {
			break
		}
	} # end for loop
	finalizeResult(fcnResult)
}

initialBackOffSeconds<-function() {0.5} # half second

increaseBackoff<-function(currentBackOffSeconds) {
	BACKOFF_MULTIPLIER <- 2
	MAX_BACKOFF_SECONDS<-30
	min(MAX_BACKOFF_SECONDS, BACKOFF_MULTIPLIER*currentBackOffSeconds)
}

