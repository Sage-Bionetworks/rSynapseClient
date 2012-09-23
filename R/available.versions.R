setMethod(
	f = "available.versions",
	signature = "character",
	definition = function(object){
		if(is.null(object) || object == "")
			return(NULL)
		.jsonListToDataFrame(synapseGet(sprintf("/entity/%s/version", object))$results)
	}
)
