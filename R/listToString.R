# This utility converts 
#				list(foo="a", bar="b") 
#			to the string
#				"foo=a,bar=b"
# 
# Author: brucehoff
###############################################################################


listToString<-function(x) {
  paste(lapply(names(x), function(n,x){sprintf("%s=%s",n, x[[n]])}, x), collapse=",")
}
