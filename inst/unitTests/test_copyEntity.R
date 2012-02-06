# Test copy entity method
# 
# Author: Matt Furia
###############################################################################

unitTestCopyLoadedObjects <- 
		function()
{
	layer <- new(Class="Layer")
	checkEquals(length(objects(layer@location@objects)), 0)
	
	layer@location@objects$boo <- "blah"
	
	attachedCopy <- layer
	checkEquals(length(objects(attachedCopy@location@objects)), 1)
	
	newLayer <- new(Class="Layer")
	checkEquals(length(objects(newLayer@location@objects)), 0)
	
	layer@location@objects$foo <- "bar"
	checkTrue(length(objects(layer@location@objects)) == 2)
	checkTrue(length(objects(attachedCopy@location@objects)) == 2)
	checkTrue(all(objects(attachedCopy@location@objects) == objects(layer@location@objects)))
	checkEquals(layer@location@objects$foo, attachedCopy@location@objects$foo)
	checkEquals(layer@location@objects$boo, attachedCopy@location@objects$boo)
	
	detachedCopy <- copyEntity(layer)
	checkTrue(length(objects(detachedCopy@location@objects)) == 2)
	checkTrue(all(objects(detachedCopy@location@objects) == objects(layer@location@objects)))
	checkEquals(layer@location@objects$foo, detachedCopy@location@objects$foo)
	
	layer@location@objects$bar <- "foo"
	checkTrue(length(objects(layer@location@objects)) == 3)
	checkTrue(length(objects(attachedCopy@location@objects)) == 3)
	checkTrue(length(objects(detachedCopy@location@objects)) == 2)
	
}

unitTestCopyLocation <-
		function()
{
	layer <- new(Class="Layer")
	checkEquals(length(layer@location@files), 0)
	
	layer@location@files[1] <- "/fakeDir/fakefile.txt"
	checkEquals(length(layer@location@files), 1)
	
	copy <- copyEntity(layer)
	checkEquals(length(copy@location@files), 1)
	checkTrue(all(copy@location@files == layer@location@files))
}
