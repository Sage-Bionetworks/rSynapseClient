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

# convert 'attachments' list to file handles
uploadWikiAttachments<-function(wikiPage) {
	fileHandleIdList<-propertyValue(wikiPage, "attachmentFileHandleIds")
	if (is.null(fileHandleIdList)) fileHandleIdList <- list()
	for (attachment in wikiPage@attachments) {
		fileHandle<-uploadAndAddToCacheMap(attachment, S3UploadDestination())
		fileHandleIdList<-append(fileHandleIdList, fileHandle$id)
	}
	if (is(fileHandleIdList, "character") && length(fileHandleIdList)==1) {
		fileHandleIdList<-list(fileHandleIdList)
	}
	propertyValue(wikiPage, "attachmentFileHandleIds")<-fileHandleIdList
	wikiPage
}

synCreateWiki<-function(wikiPage) {
  createUri<-wikiPage@createUri
	wikiPage<-uploadWikiAttachments(wikiPage)
  listResult<-synRestPOST(createUri, wikiPage)
  populateWikiPage(createUri, listResult)
}

synUpdateWiki<-function(wikiPage) {
	wikiPage<-uploadWikiAttachments(wikiPage)
	listResult<-synRestPUT(wikiPage@updateUri, wikiPage)
	populateWikiPage(wikiPage@createUri, listResult)
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

setMethod(
  f = "synStore",
  signature = "WikiPage",
  definition = function(entity) {
    if (is.null(propertyValue(entity, "id"))) {
      synCreateWiki(entity)
    } else {
      synUpdateWiki(entity)
    }
  }
)

setMethod(
  f = "synDelete",
  signature = "WikiPage",
  definition = function(entity) {
    synRestDELETE(entity@updateUri)
  }
)

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
