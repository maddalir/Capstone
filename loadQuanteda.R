# Read Quanteda
loadQuanteda <- function() {
  # read df
  cdirname <- "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample10/reg_words"
  setwd(cdirname)
  
  n1_df <- readRDS("n1.Rda")
  n1_df_s <- n1_df[order(-n1_df$Freq),]
  
  n2_df <- readRDS("n2.Rda")
  n2_df_s <- n2_df[order(-n2_df$Freq),]
  
  n3_df <- readRDS("n3.Rda")
  n3_df_s <- n3_df[order(-n3_df$Freq),]
  
  n4_df <- readRDS("n4.Rda")
  n4_df_s <- n4_df[order(-n4_df$Freq),]
  
  rm(n1_df)
  rm(n2_df)
  rm(n3_df)
  rm(n4_df)
  
  cdirname <- "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample10/sw_words"
  setwd(cdirname)
  
  n1_sw_df <- readRDS("n1.Rda")
  n1_sw_df_s <- n1_sw_df[order(-n1_sw_df$Freq),]
  
  n2_sw_df <- readRDS("n2.Rda")
  n2_sw_df_s <- n2_sw_df[order(-n2_sw_df$Freq),]
  
  n3_sw_df <- readRDS("n3.Rda")
  n3_sw_df_s <- n3_sw_df[order(-n3_sw_df$Freq),]
  
  n4_sw_df <- readRDS("n4.Rda")
  n4_sw_df_s <- n4_sw_df[order(-n4_sw_df$Freq),]
  
  rm(n1_sw_df)
  rm(n2_sw_df)
  rm(n3_sw_df)
  rm(n4_sw_df)
  
  n2_df_s <- data.table(n2_df_s)
  n3_df_s <- data.table(n3_df_s)
  n4_df_s <- data.table(n4_df_s)
  n1_df_s <- data.table(n1_df_s)

  n1_sw_df_s <- data.table(n1_sw_df_s)
  n2_sw_df_s <- data.table(n2_sw_df_s)
  n3_sw_df_s <- data.table(n3_sw_df_s)
  n4_sw_df_s <- data.table(n4_sw_df_s)
  
  n2_df_s$tokens <- as.character(n2_df_s$dfm_n2)
  n1_df_s$tokens <- as.character(n1_df_s$dfm_n1)
  n3_df_s$tokens <- as.character(n3_df_s$dfm_n3)
  n4_df_s$tokens <- as.character(n4_df_s$dfm_n4)

  n1_df_s$Freq <- as.integer(n1_df_s$Freq)
  n2_df_s$Freq <- as.integer(n2_df_s$Freq)
  n3_df_s$Freq <- as.integer(n3_df_s$Freq)
  n4_df_s$Freq <- as.integer(n4_df_s$Freq)
  
  n2_sw_df_s$tokens <- as.character(n2_sw_df_s$dfm_n2)
  n1_sw_df_s$tokens <- as.character(n1_sw_df_s$dfm_n1)
  n3_sw_df_s$tokens <- as.character(n3_sw_df_s$dfm_n3)
  n4_sw_df_s$tokens <- as.character(n4_sw_df_s$dfm_n4)
  
  # This has more phrases
  n1_sw_df_s$Freq <- as.integer(n1_sw_df_s$Freq)
  n2_sw_df_s$Freq <- as.integer(n2_sw_df_s$Freq)
  n3_sw_df_s$Freq <- as.integer(n3_sw_df_s$Freq)
  n4_sw_df_s$Freq <- as.integer(n4_sw_df_s$Freq)
  

  library(tuples)
  gramsDT <- tuple(n1_df_s,n2_df_s,n3_df_s,n4_df_s)
  gramsDT[[2]] <- gramsDT[[2]][gramsDT[[2]]$Freq >5]
  gramsDT[[3]] <- gramsDT[[3]][gramsDT[[3]]$Freq >5]
  gramsDT[[4]] <- gramsDT[[4]][gramsDT[[4]]$Freq >5]
  
  sw_gramsDT[[2]] <- sw_gramsDT[[2]][sw_gramsDT[[2]]$Freq >5]
  sw_gramsDT[[3]] <- sw_gramsDT[[3]][sw_gramsDT[[3]]$Freq >5]
  sw_gramsDT[[4]] <- sw_gramsDT[[4]][sw_gramsDT[[4]]$Freq >5]
  
  sw_gramsDT[[1]]$dfm_n1 <- NULL
  sw_gramsDT[[2]]$dfm_n2 <- NULL
  sw_gramsDT[[3]]$dfm_n3 <- NULL
  sw_gramsDT[[4]]$dfm_n4 <- NULL
  
  gramsDT[[1]]$dfm_n1 <- NULL
  gramsDT[[2]]$dfm_n2 <- NULL
  gramsDT[[3]]$dfm_n3 <- NULL
  gramsDT[[4]]$dfm_n4 <- NULL

  # read df
  cdirname <- "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample10/reg_words"
  setwd(cdirname)
  saveRDS(gramsDT[[1]],"g1.rds")
  saveRDS(gramsDT[[2]],"g2.rds")
  saveRDS(gramsDT[[3]],"g3.rds")
  saveRDS(gramsDT[[4]],"g4.rds")
  
  # read df
  cdirname <- "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample10/sw_words"
  setwd(cdirname)
  saveRDS(sw_gramsDT[[1]],"sw_g1.rds")
  saveRDS(sw_gramsDT[[2]],"sw_g2.rds")
  saveRDS(sw_gramsDT[[3]],"sw_g3.rds")
  saveRDS(sw_gramsDT[[4]],"sw_g4.rds")
}

loadDT <- function() {
  #rm(list=ls())
  
  cdirname <- "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample10/reg_words"
  setwd(cdirname)
  g1 <- readRDS("g1.rds")
  g2 <- readRDS("g2.rds")
  g3 <- readRDS("g3.rds")
  g4 <- readRDS("g4.rds")

  cdirname <- "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample10/sw_words"
  setwd(cdirname)
  sw_g1 <- readRDS("sw_g1.rds")
  sw_g2 <- readRDS("sw_g2.rds")
  sw_g3 <- readRDS("sw_g3.rds")
  sw_g4 <- readRDS("sw_g4.rds")

  library(tuple)
  library(sets)
  g <- tuple(g1,g2,g3,g4)
  sw_g <- tuple(sw_g1,sw_g2,sw_g3,sw_g4)
  g1
}  
  

