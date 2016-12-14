library(tm)   
library(utils)
library(R.utils)
library(hash)
library(tuple)
library(quanteda)
library(data.table)
library(sets)


# Cleanup
# Create the toSpace content transformer
createCleanDoc <- function(source_dir,dest_dir,useStopWords=TRUE) {

  setwd(dest_dir)
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
  setwd(source_dir)
  
  dtm_file <- textfile("./dtm.txt")
  dfm_corpus <- corpus(dtm_file)
  # Table gives us Var1....Freq
  # For ngram=2 Var1 = n1_n2
  
  # Filter all words with Freq == 1
  n1_df <- as.data.frame(table(tokenize(dfm_corpus,ngrams=1)))
  
  # Filter all ngrams > 2 with words not in n1_df
  n2_df <- as.data.frame(table(tokenize(dfm_corpus,ngrams=2)))
  n3_df <- as.data.frame(table(tokenize(dfm_corpus,ngrams=3)))
  n4_df <- as.data.frame(table(tokenize(dfm_corpus,ngrams=4)))
  
  n2_df$tokens <- as.character(n2_df$Var1)
  n1_df$tokens <- as.character(n1_df$Var1)
  n3_df$tokens <- as.character(n3_df$Var1)
  n4_df$tokens <- as.character(n4_df$Var1)
  
  n1_df$Var1 <- NULL
  n2_df$Var1 <- NULL
  n3_df$Var1 <- NULL
  n4_df$Var1 <- NULL
  
  
  n1_df$Freq <- as.integer(n1_df$Freq)
  n2_df$Freq <- as.integer(n2_df$Freq)
  n3_df$Freq <- as.integer(n3_df$Freq)
  n4_df$Freq <- as.integer(n4_df$Freq)
  
  n1_df <- data.table(n1_df)
  n2_df <- data.table(n2_df)
  n3_df <- data.table(n3_df)
  n4_df <- data.table(n4_df)
  
  # split words
  new_columns <- unlist(lapply(list(1:4),function(x) {paste("token",x,sep="")}))
  for (i in 1:1) { n1_df[,new_columns[i] := unlist(sapply(n1_df$tokens,function(x) strsplit(x,"_")[[1]][i]))]}  
  for (i in 1:2) { n2_df[,new_columns[i] := unlist(sapply(n2_df$tokens,function(x) strsplit(x,"_")[[1]][i]))]}
  for (i in 1:3) { n3_df[,new_columns[i] := unlist(sapply(n3_df$tokens,function(x) strsplit(x,"_")[[1]][i]))]}
  for (i in 1:4) { n4_df[,new_columns[i] := unlist(sapply(n4_df$tokens,function(x) strsplit(x,"_")[[1]][i]))]}
  
  #n1_df <- n1_df[!((!grep("(\\w)\\1{4,}", n1_df$tokens)) | (n1_df$Freq == 1)),]
  n1_df <- n1_df[!((!grep("(\\w)\\1{4,}", n1_df$tokens)) ),]
  n1_df <- n1_df[n1_df$Freq >= 5,]
  n2_df <- n2_df[n2_df$token1 %in% n1_df$tokens,] 
  n2_df <- n2_df[n2_df$token2 %in% n1_df$tokens,]
  n3_df <- n3_df[n3_df$token1 %in% n1_df$tokens,]
  n3_df <- n3_df[n3_df$token2 %in% n1_df$tokens,]
  n3_df <- n3_df[n3_df$token3 %in% n1_df$tokens,]
  n4_df <- n4_df[n4_df$token1 %in% n1_df$tokens,]
  n4_df <- n4_df[n4_df$token2 %in% n1_df$tokens,] 
  n4_df <- n4_df[n4_df$token3 %in% n1_df$tokens,]
  n4_df <- n4_df[n4_df$token4 %in% n1_df$tokens,]
  
  n1_df_s <- data.table(n1_df[order(-n1_df$Freq),])  
  n2_df_s <- data.table(n2_df[order(-n2_df$Freq),])
  n3_df_s <- data.table(n3_df[order(-n3_df$Freq),])  
  n4_df_s <- data.table(n4_df[order(-n4_df$Freq),])
  
  #Filtered by those that have been removed
  
  saveRDS(n1_df_s,"g1.rds")
  saveRDS(n2_df_s,"g2.rds")
  saveRDS(n3_df_s,"g3.rds")
  saveRDS(n4_df_s,"g4.rds")  
  
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

uncompressTables <- function(offset,tuples,hash) {
  # create columnnames
  # create offset number of
  for (n in 2:offset) {
    col <- unlist(lapply(list(1:n),function(x) {paste("token",x,sep="")}))
    for (i in 1:n) {  tuples[[n]][[col[i]]] = unlist(sapply(tuples[[n]][[col[i]]],function(x) unhashit(x,hash)))}
    print(n)
  }
  tuples
}

compressTables <- function(dest_dir,offset,tuples,hash) {
  # create columnnames
  # create offset number of
  for (n in 1:offset) {
    #dt = tuples[[offset]]
    col <- unlist(lapply(list(1:n),function(x) {paste("token",x,sep="")}))
    print(col)
    library(data.table)
    #for (i in 1:n) { tuples[[n]][,new_columns[i] := unlist( sapply(tuples[[n]]$tokens,function(x) hashit(strsplit(x,"_")[[1]][i],hash)))];}
    #for (i in 1:n) {  tuples[[n]][[col[i]]] = hashit(tuples[[n]][[col[i]]],hash);}
    # Double brackets creates a column
    for (i in 1:n) {  tuples[[n]][[col[i]]] = unlist(sapply(tuples[[n]][[col[i]]],function(x) hashit(x,hash)))}
    print(n)
  }
  print("Done")
  setwd(dest_dir)
  for (n in 2:offset) {
    tuples[[n]]$tokens <- NULL
  }
  saveRDS(tuples[[1]],"g1c.rds")
  saveRDS(tuples[[2]],"g2c.rds")
  saveRDS(tuples[[3]],"g3c.rds")
  saveRDS(tuples[[4]],"g4c.rds")  
  
  tuples
}

compressAgain <- function(dest_dir,tuples) {

  setwd(dest_dir)
  tuples[[1]] <- tuples[[1]][,c("Freq","tokens","token1","pkn"),with=FALSE]
  tuples[[2]] <- tuples[[2]][,c("Freq","token1","token2","pkn"),with=FALSE]
  tuples[[3]] <- tuples[[3]][,c("Freq","token1","token2","token3"),with=FALSE]
  tuples[[4]] <- tuples[[4]][,c("Freq","token1","token2","token3","token4"),with=FALSE]
  saveRDS(tuples[[1]],"g1ca.rds")
  saveRDS(tuples[[2]],"g2ca.rds")
  saveRDS(tuples[[3]],"g3ca.rds")
  saveRDS(tuples[[4]],"g4ca.rds")  
  
  tuples
  
}
