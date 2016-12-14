
Kneser-Ney Smoothing Example
Mario R. MelchioriPrediction Model · 8 days ago · Edited
The code follows the example from Appendices of Implementation of Modified Kneser-Ney Smoothing on Top of Generalized Language Models for Next Word Prediction available here .

Data are available here (table 5 - page 31) . Download the files to your working directory.

The output is compared with Table 9 - page 35. The code gets the value calculated with not higher precision. ( 0.2658 instead of 0.2408). See page 38.

Other example: See page 15, equation (46)

On page 38 where it says (25/(25+2*2))/2 should say (25/(25+2*3))/2

The code tries to be a simplified version of the Kneser-Ney smoothing algorithm, to explain as it works, because of this, it is not elegant nor efficient. I hope it helps.

# download data

unigram <- read.csv("unigram.csv",
                    header=FALSE, stringsAsFactors=FALSE,strip.white =TRUE) 
bigram <- read.csv("bigram.csv", 
                   header=FALSE, stringsAsFactors=FALSE,strip.white =TRUE)
trigram <- read.csv("trigram.csv", 
                    header=FALSE, stringsAsFactors=FALSE,strip.white =TRUE)

# put columns names, delete rows names
colnames(unigram)= c("word","freq")
rownames(unigram) <- NULL
colnames(bigram)= c("word","freq")
rownames(bigram) <- NULL
colnames(trigram)= c("word","freq")
rownames(trigram) <- NULL

# unigram Pkn
query = unigram$word
unigram$Pkn <-unigram$freq[which(unigram$word == query)]/(sum(unigram$freq)-3)
unigram$Pkn[1]<- 0 

# bigram Pkn

bigram$Pkn <- 0
for (i in 1:nrow(bigram)){
  query2 = bigram$word [i]
  
  # Count of Bigram word
  A <- bigram$freq[which(bigram$word == query2)]

  # First word in Bigram
  query1= unlist(strsplit(query2, " "))[1]
  
  # Count of Unigram for first word
  B <- unigram$freq[which(unigram$word == query1)]

  # Count of Counts To calc D
  N <- tabulate(bigram$freq)
  # Words that appear one and twice
  D <- N[1]/ (N[1] +2*N[2])

  # Count of different types of occurances - Starting First word * count bins in all bigram
  N1plus1 <-  sum(tabulate(bigram$freq[grep(paste("^",query1,sep=""),bigram$word)]))
  
  query3 <- unlist(strsplit(query2, " "))[2]
  
  # Count of different types of occurances - Ending with Second word * count bins in all bigrams 
  N1plus2 <- sum(tabulate(bigram$freq[grep(paste(query3,"$",sep=""),bigram$word)]))
  
  # Count of Occurances
  N1plus3 <- sum(N)
  
  Pkn <- max(A-D,0)/ B + D/B * N1plus1*N1plus2/N1plus3
  bigram$Pkn[i] <- Pkn
}

# trigram Pkn 

trigram$Pkn <- 0
for (i in 1:nrow(trigram)){
  query3 = trigram$word [i]
  A <- trigram$freq[which(trigram$word == query3)]
  query2 <- paste(unlist(strsplit(query3, " "))[1],unlist(strsplit(query3, " "))[2])
  B <- bigram$freq[which(bigram$word == query2)]

  # Count of Counts To calc D  
  N <- tabulate(trigram$freq)
  D <- N[1]/ (N[1] +2*N[2])
  N1plus <- sum(tabulate(trigram$freq[grep(paste("^",query2,sep=""),trigram$word)]))
  
  
  query1 <- unlist(strsplit(query3, " "))[2]
  query2 <- paste(query1,unlist(strsplit(query3, " "))[3])
  query4 <- unlist(strsplit(query3, " "))[3]

  Aa <- bigram$freq[which(bigram$word == query2)]
  # Unigram Frequency of Second Word
  Bb <- unigram$freq[which(unigram$word == query1)]
  Nn <- tabulate(bigram$freq)
  Dd <- Nn[1]/ (Nn[1] +2*Nn[2])
  Nn1plus <-  sum(tabulate(bigram$freq[grep(paste("^",query1,sep=""),bigram$word)]))
  Nn1plus2 <- sum(tabulate(bigram$freq[grep(paste(query4,"$",sep=""),bigram$word)]))
  Nn1plus3 <- sum(Nn)
  Pkn <-  max(A-D,0)/ B + (D/B) * N1plus *(max(Aa-Dd,0)/ Bb + (Dd/Bb * Nn1plus*Nn1plus2/Nn1plus3)) 
  trigram$Pkn[i] <- Pkn
}  