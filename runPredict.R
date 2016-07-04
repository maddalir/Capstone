#https://github.com/maxbane/simplegoodturing/blob/master/sgt.py
#http://www.cs.uccs.edu/~jkalita/work/cs589/2010/4Ngrams2.pdf

cleanWord <- function(word) {
  # remove profanity
  # remove characters
  tolower(gsub("[?.;!¡¿·']", "", word))
}

splitNGram <- function(token_string) {
  strsplit(token_string,"_")  
}

hashit <- function(charstring,hash) {
  result <- NULL
  if (!has.key(charstring,hash)) {
    9999999
  }
  else {
    result <- hash[charstring]
    unname(values(result))
  }
}

unhashit <- function(hashint,rhash) {
  unname(values(rhash[as.character(hashint)]) ) 
}

# searchString is space delimited
listOfNums <- function(searchString,hash) {
  string_list <- strsplit(searchString," ")  
  #print(string_list)
  new_string_list <- sapply(string_list,cleanWord)
  #print(new_string_list)
  hash_list <- sapply(new_string_list,hashit,hash)
  unname(hash_list)
}

# http://www.cs.cornell.edu/courses/cs4740/2014sp/lectures/smoothing+backoff.pdf
# https://class.coursera.org/nlp/lecture
katz_backoff <- function(searchString,hash,tuples) {
  hash_tokens <-listOfNums(searchString,hash)
  #print(hash_tokens)
  backoff_token_count = length(hash_tokens)
  #print(backoff_token_count)    

  multiple_vectors <-data.frame(lapply(hash_tokens,function(each_hash_token) as.vector(each_hash_token)))
  #print(head(multiple_vectors))
  search_df <- as.data.frame(multiple_vectors)
  #print(head(search_df))
  # Create Column Names
  columnsToSearch <- unlist(lapply(list(1:backoff_token_count),function(x) {paste("token",x,sep="")}))
  colnames(search_df) <- columnsToSearch
  join_dt <- as.data.table(search_df)
  #print(join_dt)
  
  # find token length to a maximum of 3
  # if 3 we use 4 to search for all matches
  dt <- tuples[[backoff_token_count+1]]
  # perform the join using the merge function
  Result <- dt[join_dt,on=columnsToSearch][1:3,]
  #print(class(Result))
  #print(Result)
  Result
}

# 3-2-1
call_katz <- function(searchString,hash,tuples) {
  library(tuple)
  result <- c()
  print(searchString)
  hash_tokens <-listOfNums(searchString,hash)
  # starting token count  
  
  hash_token_count = length(hash_tokens)
  # Adjust the backoff
  # by manipulating the string to the last three tokens
  #a_b_c_d -> b_c_d
  if (hash_token_count > 3) {
    hash_minus = hash_token_count-3
    for (i in 1:(hash_minus)) {
        searchString <- substr(searchString,regexpr(" ",searchString)+1,nchar(searchString))
    }
    hash_token_count = hash_token_count -hash_minus
  }
  
  for (tokencount in hash_token_count:1) {
    print(searchString)
    print(tokencount)
    resultDT <- katz_backoff(searchString,hash,tuples)
    print(class(resultDT))
    print(resultDT)
    result[[tokencount]] =resultDT
    searchString <- substr(searchString,regexpr(" ",searchString)+1,nchar(searchString))
  }
  result
}


smoothingAlg <- function(searchString,hash,rhash,tuples) {
  # i ate chinese food"
  # find all bigrams with "japanese food" that don;t exist as a 4 gram
  results <- call_katz(searchString,hash,tuples)

  print(paste(as.character(tups[[3]][1]$Freq*.7),unhashit(results[[3]][1]$token4,rhash)))
  print(paste(as.character(tups[[2]][1]$Freq*.2),unhashit(results[[2]][1]$token3,rhash)))
  print(paste(as.character(tups[[1]][1]$Freq*.1),unhashit(results[[1]][1]$token2,rhash)))
  result_list <- c(
    tuples[[3]][1]$Freq*.7,tuples[[2]][1]$Freq*.2,tuples[[1]][1]$Freq*.1,
    unhashit(results[[3]][1]$token4,rhash),unhashit(results[[2]][1]$token3,rhash),unhashit(results[[1]][1]$token2,rhash))
    
  print(result_list)  
  result <- matrix(result_list,ncol=2)
  result <- as.data.table(result)
  names(result) <- c("Score","Word")
  result$Score <- as.double(result$Score)
  result <-result[order(Score)]
  print(result)
  result
}

loadDTC <- function(source_dir) {
  #rm(list=ls())
  
  setwd(source_dir)
  g1 <- readRDS("g1c.rds")
  g2 <- readRDS("g2c.rds")
  g3 <- readRDS("g3c.rds")
  g4 <- readRDS("g4c.rds")
  
  library(sets)
  # SW has more phrases
  library(hash)
  g1$Row <- c(1:dim(g1)[1])
  g <- tuple(g1,g2,g3,g4)
  g
}  

dest_dir = "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample1/reg_words"
g_sw <- loadDTC(dest_dir)

