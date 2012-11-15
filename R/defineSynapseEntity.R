defineSynapseEntity <- 
	function(type, where = parent.frame())
{
	tt <- sprintf("resources/schema/%s.json",gsub("[\\.]", "/", type$entityType))
	tt <- fromJSON(system.file(tt, package="synapseClient"))

	contains <- gsub(".+[\\.]", "", tt$implements)

	##mfuria. 14-Nov-2012
	##huge hack. prevent code entities from persisting binaries
	##eventually need to fix this.
	if(tt$title == "Code"){
		contans <- "SynapseLocationOwner"
	}

	setClass(
	    Class = tt$title,
	    contains = contains,
	    representation = representation(
	        properties = "list",
	        propertyNames = "character"
	    ),
	    prototype = prototype(
	        properties = emptyNamedList,
	        propertyNames = names(tt$properties),
	        synapseEntityKind = type$entityType
	    ), where = where
	)
}