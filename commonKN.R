
# No of words that match the selection criteria
N1plus <- function(token,tuple,col) {
  # else the filter doesn't seem to work
  ctoken <- token
  #sub1 <- sum(tabulate(tuple[tuple[[col]] == ctoken,]$Freq))
  sub1 <- dim(tuple[tuple[[col]] == ctoken,])[[1]]
  sub1
}

# A routine to use a token to determine its count of counts 
# Each token 
NCalc <- function(tuple,token) {
  
  d1 <- data.table(cbind(c(unique(tuple[[token]])),
                            c(unlist(lapply(unique(tuple[[token]]),function(x) N1plus(x,tuple,token))))))
  d1                          
}

#Chinese food - chinese food *
#Count of n-grams that meet this criteria
N2plus1 <- function(var1,var2,token1,token2,tuple) {
  ctoken1 <- token1
  ctoken2 <- token2
  sub1 <- dim(tuple[(tuple[[var1]] == as.integer(ctoken1)) & (tuple[[var2]] == as.integer(ctoken2)) ,])[[1]]
  sub1
}

N2Calc <- function(tuple,token1,token2) {
  
  d1 <-  data.table(unique(tuple[,.SD,.SDcols=c(token1,token2)]))
  d2 <-  data.table(as.numeric(apply(unique(tuple[,.SD,.SDcols=c(token1,token2)]),1,function(x) N2plus1(token1,token2,x[1],x[2],tuple))))
  d1$NewFreq <- d2
  d1
}

trigramPkn <- function(g_rg) {
  # Count of Bigram word
  unigram <- g_rg[[1]]
  bigram <- g_rg[[2]]
  trigram <- g_rg[[3]]
  
  # Firstword count
  #print(trigram)  
  
  trigram <- trigram[bigram,on=c("token1"="token1","token2"="token2"),nomatch=0]
  #trigram <- trigram[, !c("i.tokens","FreqWord1","N1plus", "N1plus2", "SumOfFreqBins", "D"), with=FALSE]
  setnames(trigram,"i.Freq","FreqWordFirstTwo")
  setnames(trigram,"pkn","BigramPkn")  
  #print(trigram)  
  # count of counts
  N <- data.table(tabulate(g_rg[[2]]$Freq))
  N <- N[N$V1 > 0,]
  D <- N[1]/ (N[1] +2*N[2])
  #print(trigram)
  #print(N)
  print(D)
  
  # Count of different types of occurances - Starting First word * count bins in all bigram
  # First Word Count
  N1_dt <- N2Calc(trigram,"token1","token2")
  colnames(N1_dt) <- c("token1","token2","N2plus")
  trigram <- trigram[N1_dt,on=c("token1","token2"),nomatch=0]
  #print(trigram)
  
    
  trigram$pkn <-  max(trigram$Freq-.7,0)/trigram$FreqWordFirstTwo + 
    (.7/trigram$FreqWordFirstTwo) * trigram$N2plus * trigram$BigramPkn
  
  print(trigram)
  g_rg[[3]] <- trigram
  g_rg
  
}

bigramPkn <- function(g_rg) {
  # Count of Bigram word
  unigram <- g_rg[[1]]
  bigram <- g_rg[[2]]
  # Join to obtain  Count of First Word
  bigram <- bigram[unigram,on=c("token1"="token1"),nomatch=0]
  setnames(bigram,"i.Freq","FreqWord1")
  #print("unigram join")
  #print(bigram)

  # count of counts
  N <- data.table(tabulate(g_rg[[2]]$Freq))
  N <- N[N$V1 > 0,]
  
  #print(dim(N))
  # Count of different types of occurances - Starting First word * count bins in all bigram
  N1_dt <- NCalc(bigram,"token1")
  colnames(N1_dt) <- c("token1","N1plus")
  bigram <- bigram[N1_dt,on=c("token1"),nomatch=0]
  #print(bigram)
  
  N2_dt <- NCalc(bigram,"token2")
  colnames(N2_dt) <- c("token2","N1plus2")
  bigram <- bigram[N2_dt,on=c("token2"),nomatch=0]
  #print(bigram)
  
  # Number of words in the unigram table
  CountOfBins <- dim(N)[1]
  # D <- N1/N1 + 2(N2)
  #N1 - No. of bi-grams that occur only once
  #N1 - No. of bi-grams that occur only twice
  bigram$D <- .70
  
  bigram$pkn <- max(bigram$Freq-bigram$D,0)/bigram$FreqWord1 
                + bigram$D/bigram$FreqWord1 * bigram$N1plus*bigram$N1plus2/CountOfBins
  
  #print("final")
  #print(bigram)
  g_rg[[2]] <- bigram
  g_rg
}

unigramPkn <- function(tuples) {
  # unigram Pkn
  tuples[[1]]$pkn <- tuples[[1]]$Freq/sum(g_rg[[1]]$Freq)
  tuples
}

