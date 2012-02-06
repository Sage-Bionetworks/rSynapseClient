# TODO: Add comment
# 
# Author: furia
###############################################################################

setMethod(
		f = "attach",
		signature = "LocationOwner",
		definition = function (what, pos = 2, name, warn.conflicts = TRUE) {
			if(missing(name))
				name = getPackageName(what@location@objects)
			what <- what@location@objects
			attach (what, pos = pos, name = name, warn.conflicts) 
		}
)
