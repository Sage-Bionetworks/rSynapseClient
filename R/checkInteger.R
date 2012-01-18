checkInteger <- function(x){
        result <- NULL
        for(i in 1:length(x)){
                if(length(x) > 1){
                        xx <- x[[i]]
                }else{
                        xx <- x
                }
                if(any(isS4(xx))){
                        result <- c(result, FALSE)
                }else{
                        suppressWarnings(thisVal <- tryCatch({
                                                xx <- gsub("L$","",xx)
                                                .Machine$double.eps > abs(as.integer(xx) - as.numeric(xx))
                                        },
                                        error = function(e) { rep(FALSE, length(xx)) }
                                )
                        )
			mk <- is.na(thisVal) | is.null(thisVal)
			if(any(mk))
				thisVal[mk] <- FALSE
                        result <- c(result, thisVal)
                }


        }
        result
}
