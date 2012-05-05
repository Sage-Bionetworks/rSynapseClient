
library(synapseClient)
synapseLogin()

## turn on automatic provenance
startStep()


## this is just to show what's happening under the covers, you don't need to call this
onWeb(getStep())


## load some clinical data
clinicalData <- getEntity('syn48337')
# --> refresh the Step web UI page and see how it changed, an input reference was added 
# TODO web UI does not currently show input links, for now look at it via R
str(getStep())


## load some expression data
expressionData <- getEntity('syn48344')
# --> refresh the Step web UI page and see how it changed, an input reference was added 
# TODO web UI does not currently show input links, for now look at it via R
str(getStep())


####
# Do some science here
# . . .
# and get some interesting results worth saving in Synapse
####

## Create a project for results
myName <- paste("Your Name Here",  gsub(':', '_', date()))
project <- Project(list(
				name=paste("Analysis Results - ", myName)
		))
project <- createEntity(project)


## Create a study for results
study <- Study(list(
				name="Analysis Plots",
				parentId=propertyValue(project, "id")
		))
study <- createEntity(study)


## Create a Graph
outputFile <- "mygraph.jpg"
jpeg(outputFile)
attach(mtcars)
plot(wt, mpg) 
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
dev.off()


# Store the resulting graph in Synapse
outputData <- Data(list(
				name="Output Results",
				type="M", 
				parentId=propertyValue(study, "id")))
outputData <- addFile(outputData, outputFile)
outputData <- storeEntity(outputData)

# --> refresh the Step web UI page and see how it changed, an output reference was added 
# TODO web UI does not currently show output links, for now look at it via R
str(getStep())


## I've decided that my results are decent enough that I want to share what I did with my colleagues
analysis <- Analysis(list(description="a nice description of my analysis here . . .", 
				name="myFirstAnalysis",
				parentId=propertyValue(project, "id")))
analysis <- createEntity(analysis)


## Let's look at my analysis on the webUI, I can see that it currently holds on step
onWeb(analysis)

# --> refresh the Step web UI page and see how the step changed, the parent of the step is now the analysis I just created
# TODO web UI does not currently show output links, for now look at it via R
str(getStep())


## q() will also stop the step
stoppedStep <- stopStep()
# --> refresh the Step web UI page and see how it changed, environmentDescriptors were filled in using sessionInfo() and endDate was set
# TODO web UI does not currently show environment descriptors, for now look at it via R
str(stoppedStep)