# Class definition and methods for WikiPage
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
    updateUri = "character",
    # filePaths: full paths to local files.
    attachments = "list"
  ),
  # This is modeled after defineEntityClass in AAAschema
  prototype = prototype(
    properties = initializeWikiProperties()
  )
)

##
## WikiPage constructor
WikiPage<-function(owner, title, markdown, attachments, parentWikiId, fileHandles) {
  wikiPage <- new("WikiPage")
  if (missing(owner)) {
    stop("owner is required.")
  }
  wikiPage@createUri<-createWikiUri(owner)
  if (!missing(attachments)) wikiPage@attachments<-attachments
  if (!missing(fileHandles)) propertyValue(wikiPage, "attachmentFileHandleIds")<-fileHandles
  if (!missing(title)) propertyValue(wikiPage, "title")<-title
  if (!missing(markdown)) propertyValue(wikiPage, "markdown")<-markdown
  if (!missing(parentWikiId)) propertyValue(wikiPage, "parentWikiId")<-parentWikiId
  wikiPage
}

createWikiUri<-function(parent) {
  if (is(parent, "Entity")) {
    sprintf("/entity/%s/wiki", propertyValue(parent, "id"))
  } else if (is(parent, "Evaluation")){
    sprintf("/evaluation/%s/wiki", propertyValue(parent, "id"))
  } else {
    stop(sprintf("Cannot construct a WikiPage for a %s object", class(parent)))
  }
}

populateWikiPage<-function(createUri, listContent) {
  result<-new("WikiPage")
  result@createUri<-createUri
  result@updateUri<-sprintf("%s/%s", createUri, listContent$id)
  # initialize attachmentFileHandleIds to be an empty list
  propertyValues(result)<-listContent
  if (length(propertyValue(result, "attachmentFileHandleIds")) == 0) {
    propertyValue(result, "attachmentFileHandleIds")<-list()
  }
  result
}

synCreateWiki<-function(wikiPage) {
  createUri<-wikiPage@createUri
  # convert 'attachments' list to file handles
  fileHandleIdList<-propertyValue(wikiPage, "attachmentFileHandleIds")
  if (is.null(fileHandleIdList)) fileHandleIdList <- list()
  for (attachment in wikiPage@attachments) {
    fileHandle<-uploadAndAddToCacheMap(attachment)
    fileHandleIdList[[length(fileHandleIdList)+1]]<-fileHandle$id
  }
  propertyValue(wikiPage, "attachmentFileHandleIds")<-fileHandleIdList
  listResult<-synRestPOST(createUri, wikiPage)
  populateWikiPage(createUri, listResult)
}

# if id is null then we get the root page
synGetWiki<-function(parent, id) {
  createUri<-createWikiUri(parent)
  if (missing(id)) {
    getUri<-createUri
  } else {
    getUri<-sprintf("%s/%s", createUri, id)
  }
  populateWikiPage(createUri, synRestGET(getUri))
}

synUpdateWiki<-function(wikiPage) {
  listResult<-synRestPUT(wikiPage@updateUri, wikiPage)
  populateWikiPage(wikiPage@createUri, listResult)
}

wikiHeadersUri<-function(parent) {
  if (is(parent, "Entity")) {
    sprintf("/entity/%s/wikiheadertree", propertyValue(parent, "id"))
  } else if (is(parent, "Evaluation")){
    sprintf("/evaluation/%s/wikiheadertree", propertyValue(parent, "id"))
  } else {
    stop(sprintf("Unsupported %s", class(parent)))
  }
}

synGetWikiHeaders<-function(parent) {
  uri<-wikiHeadersUri(parent)
  result<-list()
  for (entry in synRestGET(uri)$results) {
    result[[length(result)+1]]<-createS4ObjectFromList(as.list(entry), "WikiHeader")
  }
  result
}
