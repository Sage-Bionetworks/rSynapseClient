setMethod(
	f = "getAnnotations",
	signature = "character",
	definition = function(entity){
		uri <- .generateAnnotationsUri(entity)
		SynapseAnnotations(synapseGet(uri))
	}
)