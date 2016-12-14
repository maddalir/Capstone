rm(list=ls())
setwd("/Users/rajmaddali/GitHub/Capstone/")
#source_dir = "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample01/raw"
#dest_dir = "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample01/reg_words"


source_dir = "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample10/raw"
dest_dir = "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample10/reg_words"
source("/Users/rajmaddali/GitHub/Capstone/common.R")
source("/Users/rajmaddali/GitHub/Capstone/runQuanteda.R")
source("/Users/rajmaddali/GitHub/Capstone/runPredict.R")
loadPackages()
createCleanDoc(source_dir,dest_dir,useStopWords=FALSE) 
create_grams(dest_dir)
g_rg <- loadDT(dest_dir)
g_rg_hash <- createBaseHash(g_rg)
g_rg_rhash <- createReverseHash(g_rg)
save(g_rg_hash,file="g_rg_hash.Robj")
save(g_rg_rhash,file="g_rg_rhash.Robj")
compressTables(dest_dir,4,g_rg,g_rg_hash)

source("/Users/rajmaddali/GitHub/Capstone/commonKN.R")
g_rg <- loadDTC(dest_dir)

g_rg <- unigramPkn(g_rg) 
g_rg <- bigramPkn(g_rg) 
g_rg <- trigramPkn(g_rg) 
g_rg2 <- compressAgain(dest_dir,g_rg)

setwd("/Users/rajmaddali/GitHub/Capstone")
rm(list=ls())
source("/Users/rajmaddali/GitHub/Capstone/common.R")
source("/Users/rajmaddali/GitHub/Capstone/commonKN.R")
source("/Users/rajmaddali/GitHub/Capstone/runPredict.R")
dest_dir = "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample10/reg_words"

g_rg <- loadDTCA(dest_dir)

load(file="g_rg_hash.Robj")
load(file="g_rg_rhash.Robj")
smoothingAlg("can we",g_rg_hash,g_rg_rhash,g_rg) 


