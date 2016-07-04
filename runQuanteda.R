library(tm)   
library(utils)
library(R.utils)
library(hash)
library(tuple)


#cd /Users/rajmaddali/GitHub/Capstone/final/en_US/
#mkdir /Users/rajmaddali/GitHub/Capstone/final/en_US/sample1
#cd /Users/rajmaddali/GitHub/Capstone/final/en_US/sample1
#mkdir /Users/rajmaddali/GitHub/Capstone/final/en_US/sample1/reg_words
#mkdir /Users/rajmaddali/GitHub/Capstone/final/en_US/sample1/sw_words

#gshuf -n 110000 en_US.twitter.txt > sample1/en_US.twitter.txt
#gshuf -n 9000 en_US.blogs.txt > sample1/en_US.blogs.txt
#gshuf -n 11000 en_US.news.txt > sample1/en_US.news.txt

# Cleanup
# Create the toSpace content transformer
createCleanDoc <- function(source_dir,dest_dir,docs,useStopWords=TRUE) {

  library(tm)
  docs <- Corpus(DirSource(source_dir))
  summary(docs)
  
  toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
  toEnglish <- content_transformer(function(x) { return (iconv(x, "latin1", "ASCII", sub=""))})
  
  docs <- tm_map(docs, toEnglish)
  docs <- tm_map(docs, toSpace, ",")
  
  docs <- tm_map(docs, toSpace, "-")
  docs <- tm_map(docs, toSpace, ":")
  docs <- tm_map(docs, toSpace, '"')
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, tolower) 
  docs <- tm_map(docs, removeNumbers)   
  docs <- tm_map(docs, stripWhitespace)

  if (useStopWords == TRUE)
    docs <- tm_map(docs, removeWords, stopwords("english"))   # *Removing "stopwords" 
  docs <- tm_map(docs, PlainTextDocument)  
  dtm_matrix <- sapply(c(1,2,3),function(x) {as.matrix(docs[[x]][1][[1]])})

  dest_file <- "dtm.txt"
  dest_file_w_path = file.path(dest_dir, dest_file)
  lapply(dtm_matrix, write, file=dest_file_w_path, append=T)
}

# read df
create_grams <- function(source_dir) {
  # Use Quanteda to load the documents created by tm package
  library(quanteda)
  library(data.table)
  setwd(source_dir)
  
  dtm_file <- textfile("./dtm.txt")
  dfm_corpus <- corpus(dtm_file)
  # Table gives us Var1....Freq
  # For ngram=2 Var1 = n1_n2
  n1_df <- as.data.frame(table(tokenize(dfm_corpus,ngrams=1)))
  n2_df <- as.data.frame(table(tokenize(dfm_corpus,ngrams=2)))
  n3_df <- as.data.frame(table(tokenize(dfm_corpus,ngrams=3)))
  n4_df <- as.data.frame(table(tokenize(dfm_corpus,ngrams=4)))
  
  n1_df_s <- data.table(n1_df[order(-n1_df$Freq),])  
  n2_df_s <- data.table(n2_df[order(-n2_df$Freq),])
  n3_df_s <- data.table(n3_df[order(-n3_df$Freq),])  
  n4_df_s <- data.table(n4_df[order(-n4_df$Freq),])

  n2_df_s$tokens <- as.character(n2_df_s$Var1)
  n1_df_s$tokens <- as.character(n1_df_s$Var1)
  n3_df_s$tokens <- as.character(n3_df_s$Var1)
  n4_df_s$tokens <- as.character(n4_df_s$Var1)
  
  n1_df_s$Freq <- as.integer(n1_df_s$Freq)
  n2_df_s$Freq <- as.integer(n2_df_s$Freq)
  n3_df_s$Freq <- as.integer(n3_df_s$Freq)
  n4_df_s$Freq <- as.integer(n4_df_s$Freq)

  n1_df_s$Var1 <- NULL
  n2_df_s$Var1 <- NULL
  n3_df_s$Var1 <- NULL
  n4_df_s$Var1 <- NULL
  
  saveRDS(n1_df_s,"g1.rds")
  saveRDS(n2_df_s,"g2.rds")
  saveRDS(n3_df_s,"g3.rds")
  saveRDS(n4_df_s,"g4.rds")  
  
}

loadDT <- function(source_dir) {
  #rm(list=ls())
  
  setwd(source_dir)
  g1 <- readRDS("g1.rds")
  g2 <- readRDS("g2.rds")
  g3 <- readRDS("g3.rds")
  g4 <- readRDS("g4.rds")
  
  library(sets)
  # SW has more phrases
  library(hash)
  g1$Row <- c(1:dim(g1)[1])
  g <- tuple(g1,g2,g3,g4)
  g
}  


createBaseHash <- function(ngram_tuples) {
  
  # SW has more phrases
  library(hash)
  hashed_ngram1 <- hash(keys=ngram_tuples[[1]]$tokens,values=ngram_tuples[[1]]$Row)  
  hashed_ngram1
  
}

createReverseHash <- function(ngram_tuples) {
  
  # SW has more phrases
  library(hash)
  hashed_ngram1 <- hash(keys=ngram_tuples[[1]]$Row,values=ngram_tuples[[1]]$tokens)  
  hashed_ngram1
  
}

compressTables <- function(dest_dir,offset,tuples,hash) {
   # create columnnames
   # create offset number of
   for (n in 1:offset) {
     #dt = tuples[[offset]]
     new_columns <- unlist(lapply(list(1:n),function(x) {paste("token",x,sep="")}))
     print(new_columns)
     library(data.table)
     for (i in 1:n) { tuples[[n]][,new_columns[i] := unlist( sapply(tuples[[n]]$tokens,function(x) hashit(strsplit(x,"_")[[1]][i],hash)))];}
   }
   setwd(dest_dir)
   for (n in 2:offset) {
     tuples[[n]]$tokens <- NULL
   }
   saveRDS(tuples[[1]],"g1c.rds")
   saveRDS(tuples[[2]],"g2c.rds")
   saveRDS(tuples[[3]],"g3c.rds")
   saveRDS(tuples[[4]],"g4c.rds")  
}


# Sample of 10 percent from the texts
# source_dir = "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample1"
# dest_dir = "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample1/sw_words"
# createCleanDoc(source_dir,dest_dir,docs,useStopWords=TRUE) 
# dest_dir = "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample1/reg_words"
# createCleanDoc(source_dir,dest_dir,docs,useStopWords=FALSE)
# 
# dest_dir = "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample1/sw_words"
# create_grams(dest_dir)
# dest_dir = "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample1/reg_words"
# create_grams(dest_dir)

dest_dir = "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample1/sw_words"
g_sw <- loadDT(dest_dir)
dest_dir = "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample1/reg_words"
g_rg <- loadDT(dest_dir)

g_sw_hash <- createBaseHash(g_sw)
g_rg_hash <- createBaseHash(g_rg)
g_sw_rhash <- createReverseHash(g_sw)
g_rg_rhash <- createReverseHash(g_rg)

dest_dir = "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample1/sw_words"
compressTables(dest_dir,4,g_sw,g_sw_hash) 
dest_dir = "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample1/reg_words"
compressTables(dest_dir,4,g_rg,g_rg_hash)

