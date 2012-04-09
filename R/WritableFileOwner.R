# TODO: Add comment
# 
# Author: furia
###############################################################################

setRefClass(
  "WritableFileOwner",
  contains = c("ReadOnlyFileOwner", "VIRTUAL"),
  methods = list(
    initialize = function(){
      .self$initFields(cacheDir=tempfile())
      dir.create(.self$cacheDir)
    }
  )
)
