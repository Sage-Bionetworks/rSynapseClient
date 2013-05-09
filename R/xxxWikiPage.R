# Class definition and methods for WikiPages
# 
# Author: brucehoff
###############################################################################

initializeWikiProperties<-function() {
  synapseType<-"org.sagebionetworks.repo.model.wiki.WikiPage"
  properties<-SynapseProperties(getEffectivePropertyTypes(synapseType))
  properties
}


setClass(
  Class = "WikiPage",
  contains = "SimplePropertyOwner",
  representation = representation(
    # fields:
    createUri = "character",
    # filePaths: full paths to local files.
    filePaths = "list",
    # fileHandle (generated from JSON schema, empty before entity is created)
    fileHandles = "list"
  ),
  # This is modeled after defineEntityClass in AAAschema
  prototype = prototype(
    properties = initializeFileProperties()
  )
)

##
## WikiPage constructor
WikiPage<-function(parent, title, markdown, files) {
  wikiPage <- new("WikiPage")
  if (missing(parent)) {
    stop("parent is required.")
  }
  wikiPage@createUri<-childWikiUri(parent)
  if (!missing(files)) wikiPage@files<-files
  if (!missing(title)) propertyValue(wikiPage, "title")<-title
  if (!missing(markdown)) propertyValue(wikiPage, "markdown")<-markdown
  wikiPage
}

childWikiUri<-function(parent) {
  if (is(parent, "Entity")) {
    sprintf("/entity/%s/wiki", propertyValue(parent, "id"))
  } else if (is(parent, "Evaluation")){
    sprintf("/evaluation/%s/wiki", propertyValue(parent, "id"))
  } else {
    stop(sprintf("Unsupported %s", class(parent)))
  }
}

synCreateWiki<-function(wikiPage) {
  createUri<-childWikiUri(wikiPage@parent)  # TODO add parent field to wiki
  # TODO add 'files' slot to WikiPage
  # TODO convert 'files' list to file handles
  result<-WikiPage(synRestPOST(createUri, wikiPage))
  result@updateUri<-sprintf("%s/%s", createUri, propertyValue(result, "id"))
}

synGetWiki<-function(parent) {
  getUri<-childWikiUri(parent)
  result<-WikiPage(synRestGET(getUri))
  result@updateUri<-sprintf("%s/%s", getUri, propertyValue(result, "id"))
  
}



