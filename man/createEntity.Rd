\name{createEntity}
\alias{createEntity}
\alias{getEntity}
\alias{deleteEntity}
\alias{updateEntity}
\alias{storeEntity}
\alias{storeEntityFiles}
\alias{storeEntityObjects}
\alias{downloadEntity}
\alias{loadEntity}
\alias{getParentEntity}
\title{
Entity CRUD Operations
}
\description{
Functions for creating, retrieving, updating and deleting entities in Synapse.
}
\usage{
createEntity(entity)
getEntity(entity)
deleteEntity(entity)
updateEntity(entity)
storeEntity(entity)
downloadEntity(entity)
loadEntity(entity)
storeEntityFiles(entity)
storeEntityObjects(entity)
getParentEntity(entity)
}
\arguments{
  \item{entity}{
The entity being operated on. Depending on the function can be either an id (represented as a character or number) or
a object whose class inherits from SynapseEntity. (see Details)
}
}
\details{
These methods represent the main API to Synapse entities. The exact behavior of each method may vary depending on the entity
type passed, but the generic behavior of each method is described below. 

createEntity
This method creates an entity in Synapse. This method will only store the properties and annotations of the entity
and will not store objects or files owned by entities that are capable of owning them. If storage of objects and/or files
is desired upon creation of the entity, storeEnity, storeEntityObjects or storeEntityFiles should be used insted, depending
on the desired action.

Note that Synapse requires that entity names be unique in their context, meaning that two entities with the same parent
 		cannot have the same name.  If the user attempts to create an entity with the same name as an entity that already exists
with the same parent, create entity will throw an error and the entity will not be persisted in Synapse.


getEntity
Retrieves an entity from Synapse. This method only retrieves the annotations and properties of an entity and wil not download
files and/or objectes owned by entities that are capable of owning them. To retrieve files associated with an entity use the
downloadEntity method. To load objects owned by the entity into the "objects" slot, use the loadEntity method.


updateEntity
Update the annotations and properties of an existing Synapse entity. All properties and annotations that are stored in
Synapse will be replaced by the properties and annotations of the entity passed to this method.

This method requires that the passed entity has a valid Synapse entity ID as it's "id" property and will generate an
error otherwise. 

Also note that for entities capable of owning files and/or objects, this method will not affect the status of these elements.
To persist objects or files, use the storeEntityObjects or storeEnityFiles methods, respectively. To store all properties,
annotations, files and objects in a single method call, use the storeEntity method		


getParentEntity
Get the parent entity of the entity being passes. This method returns only the properties and annotations of the parent entity
and it's behavior can be reproduced by calling getEntity(propertyValue(entity, "parentId"))


storeEntity
In general, storeEntity will persist all properties, annotaitons, objects and files owned by the passed entity. However,
the actual data persisted by this method is dependent on the entity being passed. For objects that are not capable of owning
files or objects, storeEntity persists only annotations and properties.

For entities that already exist in Synapse (i.e. have a valid "id" property), calling storeEntity is equivalent to a calling
updateEntity, followed by a call to storeEntityObjects and storeEntity files (if applicable). For entities that do not exist
in Synapse (i.e. have a NULL "id" property), the call to updateEntity is replaced by a call to createEntity.	


storeEntityFiles
Store the files owned by an entity. Specifically, all files listed in entity$files will be zipped up and uploaded to Synapse.
Calling this method will not persist local changes to the entities properties and annotations.


storeEntityObjects
Store the objects owned by an entity. It is important to note that this method will only store objects added to the entity
using the addObject method. Objects that are added to the environment in the "objects" slot via other means will not be 
stored. Calling this method will not persist local changes to the entities properties and annotations.


downloadEntity
Download the files owned by an entity, including the binary files holding R objects added via the addObject method and stored
via the storeEntityObjects or storeEntity methods. Note that this method only downloads the data and does not load any objects
into the environment in the "objects" slot. 

Files retrieved by downloadEntity are stored in the cache directory (see entity$cacheDir or synapseCacheDir()). To preserve
bandwidth, the data are only downloaded from the server if the files do not already exist in the cache or if the zip file checksum
has changed since the last download.

Note that calling downloadEntity for an entity type that is not capable of owning objects or files is equivalent to calling getEntity.


loadEntity



}
\value{
for getParentEntity returns the parent of the entity provided.

for deleteEntity, NULL.

All other methods return a copy of the entity after performing the desired action.
}
\author{
Matt Furia
}
\note{
%%  ~~further notes~~
}

\seealso{
\code{\link{SynapseEntity}}
}
