#https://github.com/maxbane/simplegoodturing/blob/master/sgt.py
#http://www.cs.uccs.edu/~jkalita/work/cs589/2010/4Ngrams2.pdf

library(tuple)

contextSearch <- function(searchString,hash,rhash,tuples) {
  # we can assume that a max of 2 or 3 words are issues
  hash_tokens <-listOfNums(searchString,hash)
  print(hash_tokens)
  backoff_token_count = length(hash_tokens)
  #print(backoff_token_count)    
  hash_tokens = hash_tokens[2:backoff_token_count]
  if (backoff_token_count  < 2) {
    backoff_token_count =2
  }
  #print(backoff_token_count)
  #print(hash_tokens)
  #print("complete 1")
  
  multiple_vectors <-data.frame(lapply(hash_tokens,function(each_hash_token) as.vector(each_hash_token)))
  print(multiple_vectors)
  search_df <- as.data.frame(multiple_vectors)
  print(search_df)
  # Create Column Names
  columnsToSearch <- unlist(lapply(list(2:backoff_token_count),function(x) {paste("token",x,sep="")}))
  print(columnsToSearch)
  colnames(search_df) <- columnsToSearch
  print(search_df)
  join_dt <- data.table(search_df)
  print(join_dt)
  
  # find token length to a maximum of 3
  # if 3 we use 4 to search for all matches
  #dt <- tuples[[backoff_token_count+1]]
  dt <- tuples[[backoff_token_count]]
  # perform the join using the merge function
  Result <- dt[join_dt,on=columnsToSearch]
  Result
}

# Simple Good-Turing smoothing function
SGT <- function(tf) {
  
  Nr <- table(tf)
  r <- as.integer(names(Nr))
  len <- length(r)
  if(len<=2) return(list(p0=1,pr=NA))
  Nr1 <-  Nr[match(r+1,r)]
  Nr1[is.na(Nr1)] <- 0
  print("Nr1")
  print(Nr1)
  
  q <- c(0,r[-len])
  t <- c(r[-1],2*r[len]-q[len])
  Zr <- Nr/0.5/(t-q)
  print(q)  
  print(t)
  print("Zr")
  print(Zr)
  
  df <- data.frame(r,Zr)
  print("DF")
  print(df)
  fit <- lm(log(Zr)~log(r),data=df)
  Sr <- exp(predict(fit,df))
  Sr1 <- exp(predict(fit,transform(df,r=r+1)))
  
  x <- (r+1)*Nr1/Nr
  y <- (r+1)*Sr1/Sr
  rStar <- ifelse(cummax(abs(x-y) > 1.65*sqrt((1+r)*x/Nr*(1+Nr1/Nr))),y,x)
  
  N <- sum(Nr*r)
  p0 <- Nr[1]/N
  pr <- rStar/N
  pr <- (1-p0)*pr/sum(pr*Nr)
  Dr <- round((r-rStar)*Nr)
  
  #par(mfrow=c(1,2))
  #plot(r,Nr,log='xy')
  #plot(r,Zr,log='xy')
  #lines(r,Sr,col='red',lwd=2)
  
  list(r=r,Nr=Nr,rStar=rStar,pr=pr,Dr=Dr,p0=p0)
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
  join_dt <- data.table(search_df)
  #print(join_dt)
  
  # find token length to a maximum of 3
  # if 3 we use 4 to search for all matches
  dt <- tuples[[backoff_token_count+1]]
  # perform join using the merge function
  Result <- dt[join_dt,on=columnsToSearch,nomatch=0][1:30,]
  Result = Result[!is.na(Result$Freq),]  
  
  Result
}

# 3-2-1
call_katz <- function(searchString,hash,tuples) {
  searchString <- trim(searchString)
  result <- c()
  #print(searchString)
  hash_tokens <-listOfNums(searchString,hash)
  # starting token count  
  
  hash_token_count = length(hash_tokens)
  # Adjust the backoff
  # by manipulating the string to the last three tokens
  #a_b_c_d -> b_c_d
  if (hash_token_count > 3) {
    hash_minus = hash_token_count-3
    for (i in 1:(hash_minus)) {
        searchString <- substr(trim(searchString),regexpr("\\W+",searchString)+1,nchar(searchString))
    }
    hash_token_count = hash_token_count -hash_minus
  }
  
  for (tokencount in hash_token_count:1) {
    #b_c_d
    #c_d
    #d
    # Obtain the BackOff probability from katz_backoff
    resultDT <- katz_backoff(searchString,hash,tuples)
    result[[tokencount]] =resultDT
    searchString <- substr(trim(searchString),regexpr("\\W+",searchString)+1,nchar(searchString))
  }
  result
}

smoothingAlg <- function(searchString,hash,rhash,tuples) {
  # i ate chinese food"
  # find all bigrams with "japanese food" that don;t exist as a 4 gram
  # Count of Bigram word
  unigram <- tuples[[1]]
  bigram <- tuples[[2]]
  trigram <- tuples[[3]]
  
  results <- call_katz(searchString,hash,tuples)
  list1 <- list()
  list_counter = 0
  print("Start")
  d3 = NA
  l = length(results)
  if (l >= 1) {
    unigram_results = results[[1]]
    if (dim(unigram_results)[1]) {
      list2 <- list(unigram,unigram_results,trigram)
      unigram_results<- bigramPkn(list2)[[2]]
      
      unigram_results$ReFreq <- unigram_results$Freq*.01
      unigram_results$Unhash <- unhashit(unigram_results$token2,rhash)
      new_result1 <- unigram_results[,.(pkn,Unhash)] 
      print(new_result1)
      
      list_counter = list_counter+1
      list1[[list_counter]] <- new_result1
      print("Done Unigram")
    }
  }
  
  if (l >=2) {
    bigram_results = results[[2]]

    if (dim(bigram_results)[1]) {
      
      list2 <- list(unigram,bigram,bigram_results)
      bigram_results<- trigramPkn(list2)[[3]]
      bigram_results$Unhash <- unhashit(bigram_results$token3,rhash)
      
      new_result2 <- bigram_results[,.(pkn,Unhash)] 
      print(new_result2)
      
      list_counter = list_counter+1
      list1[[list_counter]] <- new_result2
      print("Done Bigram")
    }
  }
  
  
  if (l == 3) {
    trigram_results <- results[[3]]
    if (dim(trigram_results)[1]) {
      
      trigram_results$ReFreq <- trigram_results$Freq*.9
      trigram_results$Unhash <- unhashit(trigram_results$token4,rhash)
      trigram_results$pkn <- trigram_results$ReFreq + 1000
      
      new_result3 <- trigram_results[,.(pkn,Unhash)]           
      list_counter = list_counter+1
      list1[[list_counter]] <- new_result3
  
      print(new_result3)
      print("Trigram")
    }
  }
  
  #print(merge(list1[[1]],list1[[2]]),all=TRUE)
  list1 <- data.table(do.call("rbind",list1))
  setkey(list1,"Unhash")
  print("Pre Unique")
  print(list1)
  list1 <- unique(list1)
  setorder(list1,-pkn)
  print("Unique")
  print(list1)
  
  #setorder(list1,-pkn)
  print(list1$Unhash)
  #list1
  list1$Unhash
}


