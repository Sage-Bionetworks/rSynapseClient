# objects that users will interact with. can the generation of these class
# definitions be automated?
# 
# Author: furia
###############################################################################


setClass(
  "Step",
  contains = "SynapseEntity",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Step"
  )
)

setClass(
  "Analysis",
  contains = "SynapseEntity",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Analysis"
  )
)


setClass(
  "Project",
  contains = "SynapseEntity",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Project"
  )
)

setClass(
  "Link",
  contains = "SynapseEntity",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Link"
  )
)

setClass(
  "Code",
  contains="SynapseLocationOwner",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Code"
  )
)

setClass(
  "Study",
  contains = "SynapseLocationOwnerWithObjects",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Study"
  )
)

setClass(
  "Data",
  contains = "SynapseLocationOwnerWithObjects",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Data"
  
  )
)

setClass(
  "Study",
  contains = "SynapseLocationOwnerWithObjects",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Study"
  )
)

setClass(
  Class = "ExpressionData",
  contains = "SynapseLocationOwnerWithObjects",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.ExpressionData"
  )
)

setClass(
  Class = "RObject",
  contains = "SynapseLocationOwnerWithObjects",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.RObject"
  )
)


setClass(
  Class = "PhenotypeData",
  contains = "SynapseLocationOwnerWithObjects",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.PhenotypeData"
  )
)

setClass(
  Class = "Folder",
  contains = "SynapseEntity",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Folder"
  )
)

setClass(
  Class = "Preview",
  contains = "SynapseEntity",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Preview"
  )
)

setClass(
  Class = "GithubCode",
  contains = "Code"
)





