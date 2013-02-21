# This is a modification of testFunction
# It's used by copying to a file called testFunction.R in a temp dir

testFunction<-function(x) {
  internalFunction(x)
}

internalFunction<-function(x) {
  x+2
}