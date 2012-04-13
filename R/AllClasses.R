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

## a file cache factory makes sure that all in-memory copies
## of a file cache object hold a reference to the same copy
setClass(
  Class = "FileCacheFactory",
  representation = representation(env = "environment"),
  prototype = new.env(parent = emptyenv())
)

setClass(
  "Attributes",
  representation = representation(
    attributeMap = "list"
  )
)

