library(synapseClient)

# There are many more studies in there than just the TCGA ones, but we are only interested in TCGA data
studies <- synapseQuery(paste('select * from study where study.repository == "TCGA"', sep=""))
dim(studies)
studies[, 'study.name']

# We want to work with Glioblastoma data
glioblastomaStudyId <- studies$study.id[grepl('TCGA Glioblastoma', studies$study.name )][1]
onWeb(glioblastomaStudyId)

# Query for the the level_3 data for study "Glioblastoma TCGA"
data <- synapseQuery(paste('select * from expressiondata where tcgaLevel == "level_3" and parentId == "', glioblastomaStudyId, '"', sep=''))
dim(data)
names(data)
head(data$expressiondata.name)

agilentDataId <- data$expressiondata.id[grepl("unc.edu_GBM.AgilentG4502A_07_2.Level_3.4.0.0", data$expressiondata.name)]
onWeb(agilentDataId)

agilentData <- loadEntity(agilentDataId)
agilentData
agilentData$cacheDir
agilentData$files

# Get the clinical phenotypedata for study "Glioblastoma TCGA"
clinicalLayers <- synapseQuery(paste('select * from phenotypedata where phenotypedata.parentId == "', glioblastomaStudyId, '"', sep=''))
dim(clinicalLayers)
clinicalLayers$phenotypedata.name
clinicalData <- loadEntity(clinicalLayers[2, 'phenotypedata.id'])
clinicalData
clinicalData$cacheDir
clinicalData$files


