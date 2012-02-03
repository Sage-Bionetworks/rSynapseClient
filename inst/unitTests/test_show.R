unitTestShowMulipleClasses <-
	function()
{
	layer <- Layer(list(name='foobar',type='C'))
	show(layer)
	addObject(layer, list(), "aList")
	show(layer)
	addObject(layer, 1, "anInteger")
	show(layer)

	listInteger <- list()
	class(listInteger) <- c('list','integer')
	addObject(layer, listInteger, unlist=FALSE)
	show(layer)
}
