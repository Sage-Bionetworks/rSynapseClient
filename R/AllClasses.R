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
    Class = "Attributes",
    representation = representation(
        attributeMap = "list"
    )
)

setClass(
    Class = "PropertyStore",
    representation = representation(
        stringAnnotations = "list",
        doubleAnnotations = "list",
        longAnnotations = "list",
        dateAnnotations = "list"
    ),
    prototype = prototype(
        stringAnnotations = emptyNamedList,
        doubleAnnotations = emptyNamedList,
        longAnnotations = emptyNamedList,
        dateAnnotations = emptyNamedList
    )
)



