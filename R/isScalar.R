#
# check whether a variable is a scalar
#

is.scalar<-function(x) {!is.list(x) && (!is.vector(x) || length(x)<2)}

