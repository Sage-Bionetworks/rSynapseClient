# TODO: Add comment
# 
# Author: bhoff
###############################################################################


isSynapseId<-function(s) {
	if (!(class(s)=="character" && length(s)==1 && substr(s, 1, 3)=="syn" && nchar(s)>3)) {
				 F
	} else {
				 # now check whether the suffix is numeric
						 any(grep("^[[:digit:]]*$", substr(s, 4, nchar(s))))
	}
}
