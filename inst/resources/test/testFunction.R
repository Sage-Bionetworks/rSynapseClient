# This is a test file used to exerciese synapseExecute
# It feature a function named after the file (testFunction)
# as well as a second function

testFunction<-function(x) {
  internalFunction(x)
}

internalFunction<-function(x) {
  x+1
}