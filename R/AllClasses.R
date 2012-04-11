# Class Definitions
# 
# Author: furia
###############################################################################

## the global cache is a singleton
setClass(
  Class = "GlobalCache",
  representation = representation(env = "environment"),
  prototype = prototype(
      env = new.env(parent=emptyenv())
  )
)

setClass(
  "Attributes",
  representation = representation(
    attributeMap = "list"
  )
)

