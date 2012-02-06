# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
		f = "detach",
		signature = "LocationOwner",
		definition = function (name, pos = 2, unload = FALSE, character.only = FALSE, 
				force = FALSE) {
			pkgName <- getPackageName(name@location@objects)
			detach(name=pkgName, pos = pos, unload = unload, character.only = TRUE) 
		}
)

