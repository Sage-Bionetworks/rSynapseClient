## Annotation S4 class unit tests
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.setUp <- 
  function()
{
  synapseClient:::.setCache("datasetJSON", "{\"name\":\"Mouse Cultured Bone marrow derived Macrophage\",\"annotations\":\"/repo/v1/dataset/3773/annotations\",\"id\":\"3773\",\"version\":\"1.0.0\",\"description\":\"A powerful way to identify genes for complex traits it to combine genetic and genomic methods. Many trait quantitative trait loci (QTLs) for complex traits are sex specific, but the reason for this is not well understood. RNA was prepared from bone marrow derived macrophages of 93 female and 114 male F(2) mice derived from a strain intercross between apoE-deficient mice on the AKR and DBA/2 genetic backgrounds, and was subjected to transcriptome profiling using microarrays. A high density genome scan was performed using a mouse SNP chip, and expression QTLs (eQTLs) were located for expressed transcripts. Using suggestive and significant LOD score cutoffs of 3.0 and 4.3, respectively, thousands of eQTLs in the female and male cohorts were identified. At the suggestive LOD threshold the majority of the eQTLs were trans eQTLs, mapping unlinked to the position of the gene. Cis eQTLs, which mapped to the location of the gene, had much higher LOD scores than trans eQTLs, indicating their more direct effect on gene expression. The majority of cis eQTLs were common to both males and females, but only approximately 1% of the trans eQTLs were shared by both sexes. At the significant LOD threshold, the majority of eQTLs were cis eQTLs, which were mostly sex-shared, while the trans eQTLs were overwhelmingly sex-specific. Pooling the male and female data, 31% of expressed transcripts were expressed at different levels in males vs. females after correction for multiple testing.These studies demonstrate a large sex effect on gene expression and trans regulation, under conditions where male and female derived cells were cultured ex vivo and thus without the influence of endogenous sex steroids. These data suggest that eQTL data from male and female mice should be analyzed separately, as many effects, such as trans regulation are sex specific. \",\"status\":\"Future\",\"creationDate\":1312573633063,\"parentId\":\"3731\",\"etag\":\"3\",\"eulaId\":\"3732\",\"uri\":\"/repo/v1/dataset/3773\",\"creator\":\"Smith\",\"accessControlList\":\"/repo/v1/dataset/3773/acl\",\"locations\":\"/repo/v1/dataset/3773/location\",\"releaseDate\":1292025600000,\"hasExpressionData\":true,\"hasGeneticData\":true,\"hasClinicalData\":false,\"layers\":\"/repo/v1/dataset/3773/layer\"}")
  synapseClient:::.setCache("datasetAnnotationJSON", "{\"id\":\"3773\",\"creationDate\":1312573633063,\"etag\":\"3\",\"stringAnnotations\":{\"status\":[\"Future\"],\"eulaId\":[\"3732\"],\"Posting_Restriction\":[\"unspecified\"],\"citation\":[\"Sex specific gene regulation and expression QTLs in mouse macrophages from a strain intercross. Bhasin JM, Chakrabarti E, Peng DQ, Kulkarni A, Chen X, Smith JD. PLoS One. 2008 Jan 16;3(1):e1435. \"],\"Disease\":[\"Healthy\"],\"QC_statistician\":[\"\"],\"Species\":[\"Mouse\"],\"version\":[\"1.0.0\"],\"Internal_Name\":[\"Cleveland_Macrophages\"],\"Tissue_Tumor\":[\"Marcrophage\"],\"uri\":[\"/repo/v1/dataset/3773\"],\"Type\":[\"Other\"],\"Institution\":[\"Cleaveland Clinic\"],\"curator\":[\"\"]},\"longAnnotations\":{\"number_of_downloads\":[66],\"number_of_followers\":[29],\"Number_of_Samples\":[207],\"pubmed_id\":[15121029]},\"dateAnnotations\":{\"releaseDate\":[1292025600000],\"last_modified_date\":[1368144000000]},\"doubleAnnotations\":{},\"blobAnnotations\":{},\"uri\":\"/repo/v1/dataset/3773/annotations\"}")
}

.tearDown <- 
  function()
{
  synapseClient:::.deleteCache("datasetJSON")
  synapseClient:::.deleteCache("datasetAnnotationJSON")
}

unitTestConstructor <- 
  function()
{
  
  annotations <- synapseClient:::SynapseAnnotations(entity = as.list(RJSONIO::fromJSON(synapseClient:::.getCache("datasetAnnotationJSON"))))
  
  stop("fill in some tests to verify that the constructor works")
}

unitTestPopulateFromJSON <-
  function()
{
  ## test .populateSlotsFromEntity
  stop("not yet implemented")
}

unitTestPopulateFromList <-
  function()
{
  ## test .populateSlotsFromEntity
  stop("not yet implemented")
}

unitTestExtractToJSON <-
  function()
{
  ## test .extractEntityFromSlots
  stop("not yet implemented")
}