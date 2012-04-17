# TODO: Add comment
# 
# Author: furia
###############################################################################

setMethod(
  f = "getEntityInstance",
  signature = signature("list"),
  definition = function(entity)
  {
    class <- switch(entity$entityType,
      org.sagebionetworks.repo.model.Study = "Study",
      org.sagebionetworks.repo.model.Data = "Data",
      org.sagebionetworks.repo.model.Project = "Project",
      org.sagebionetworks.repo.model.Analysis = "Analysis",
      org.sagebionetworks.repo.model.Step = "Step",
      org.sagebionetworks.repo.model.Code = "Code",
      org.sagebionetworks.repo.model.Link = "Link",
      "SynapseEntity"
    )
    
    if(class == "SynapseEntity"){
      if(!is.null(entity$locations) && length(entity$locations) > 0)
        class <- "SynapseLocationOwnerWithObjects"
    }
    
    ## call the appropriate constructor and pass the list
    ## representation of the entity
    ee <- do.call(class, list(entity = entity))
    ee@synapseWebUrl <- .buildSynapseUrl(propertyValue(ee, "id"))
    ee
  }
)
