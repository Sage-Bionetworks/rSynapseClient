unitTestDollarSignAccessor <-
	function()
{
	obj <- new("SynapseProperties")
	obj$foo <- "bar"
	checkEquals(obj$foo, "bar")
}

unitTestBracketAccessor <-
	function()
{
	obj <- new("SynapseProperties")
	obj$foo <- "bar"
	checkEquals(obj['foo'], list(foo="bar"))
	checkEquals(names(obj['foo']), 'foo')
	checkEquals(class(obj['foo']), 'list')
}

unitTestDoubleBracketAccessor <-
	function()
{
	obj <- new("SynapseProperties")
	obj$foo <- "bar"
	checkEquals(obj[['foo']], "bar")
	checkEquals(class(obj[['foo']]), 'character')
}

unitTestDollarSignReplacement <-
	function()
{
	obj <- new("SynapseProperties")
	obj$foo <- "bar"
	checkEquals(obj$foo, "bar")
	obj$foo <- "goo"
	checkEquals(obj$foo, "goo")
	obj$blah <- 'gah'
	checkEquals(obj$blah, "gah")
}

unitTestBracketReplacement <-
	function()
{
	obj <- new("SynapseProperties")
	obj['foo'] <- "bar"
	checkEquals(obj['foo'], list(foo="bar"))
	checkEquals(names(obj['foo']), 'foo')
	checkEquals(class(obj['foo']), 'list')
}

unitTestDoubleBracketReplacement <-
	function()
{
	obj <- new("SynapseProperties")
	obj[['foo']] <- "bar"
	checkEquals(obj['foo'], list(foo="bar"))
	checkEquals(names(obj['foo']), 'foo')
	checkEquals(class(obj['foo']), 'list')
	checkEquals(obj$foo, "bar")
}

unitTestSetPropertyValueReplacement <-
	function()
{
	obj <- new("SynapseProperties")
	propertyValue(obj, "foo") <- "bar"
	checkEquals(obj$foo, "bar")
}

unitTestPropertyValue <-
	function()
{
	obj <- new("SynapseProperties")
	propertyValue(obj, "foo") <- "bar"
	checkEquals(propertyValue(obj, "foo"), "bar")
}

unitTestPropertyValues <-
	function()
{
	obj <- new("SynapseProperties")
	propertyValue(obj, "foo") <- "bar"
	checkEquals(propertyValues(obj), "bar")
}

unitTestPropertyNames <-
	function()
{
	obj <- new("SynapseProperties")
	propertyValue(obj, "foo") <- "bar"
	checkEquals(propertyNames(obj), "foo")
}

unitTestAsList <-
	function()
{
	obj <- new("SynapseProperties")
	propertyValue(obj, "foo") <- "bar"
	propertyValue(obj, "goo") <- "boo"
	ans <- as.list.SynapseProperties(obj)
	checkEquals(class(ans), "list")
	checkTrue(all(names(ans) %in% c('foo','goo')))
	checkTrue(all(c('foo','goo') %in% names(ans)))
	checkEquals(length(ans), 2L)
	checkEquals(ans$foo, obj$foo)
	checkEquals(ans$goo, obj$goo)
}

unitTestNames <-
	function()
{
	obj <- new("SynapseProperties")
	propertyValue(obj, "foo") <- "bar"
	propertyValue(obj, "goo") <- "boo"
	
	checkEquals(length(names(obj)), 2L)
	checkTrue(all(names(obj) %in% c('foo','goo')))
	checkTrue(all(c('foo','goo') %in% names(obj)))
}

unitTestConstructorNoArg <-
	function()
{
	obj <- SynapseProperties()
	checkEquals(names(obj), character())
}

unitTestConstructorList <-
	function()
{
	obj <- SynapseProperties(list(foo="character", boo="integer", goo="numeric"))
	checkTrue(all(names(obj) %in% c('foo','boo','goo')))
	checkTrue(all(c('foo','boo','goo') %in% names(obj)))

	checkEquals(obj[['foo']], NULL)
	checkEquals(obj[['boo']], NULL)
	checkEquals(obj[['goo']], NULL)
}

unitTestDeleteProperty <-
	function()
{
	obj <- new("SynapseProperties")
	propertyValue(obj, "foo") <- "bar"
	obj <- deleteProperty(obj, "foo")
	checkEquals(length(propertyNames(obj)), 0L)
}

unitTestSetPropertyNull <-
	function()
{
	## this should be the equivilant of calling deleteProperty
	obj <- new("SynapseProperties")
	propertyValue(obj, "foo") <- "bar"
	obj$foo <- NULL
	checkEquals(length(propertyNames(obj)), 0L)
}


