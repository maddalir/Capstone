library(quanteda)

cdirname <- "/Users/rajmaddali/GitHub/Capstone/final/en_US/sample2"
docs <- Corpus(DirSource(cdirname))

docs <- tm_map(docs, toEnglish)
docs <- tm_map(docs, toSpace, ",")
#docs <- docs2
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, '"')
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, tolower) 
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, stopwords("english"))   # *Removing "stopwords" 
docs <- tm_map(docs, PlainTextDocument)



x1 <- readLines("en_US.blogs.txt")
#x1_t <- tokenize(c1, removePunct = TRUE, ngrams = 1:3)
x1_t <- lapply(x1,function(x) tokenize(x))

setwd("/Users/rajmaddali/GitHub/Capstone/final/en_US/sample")
t1 <- textfile("en_US.blogs.txt")
t2 <- textfile("en_US.news.txt")
t3 <- textfile("en_US.twitter.txt")
t4 <-  textfile("./test.txt")
c1 <- corpus(t1) + corpus(t2) + corpus(t3)

c1_tokenized <- tokenize(c1, removePunct = TRUE, ngrams = 1:3)
c1_tokenized_rf <- sapply(c1_tokenized, function(x) removeFeatures(x,stopwords("english"), case_insensitive = TRUE))
sample2_1 <- dfm(unlist(c1_tokenized_rf), ngrams=3,removeNumbers = TRUE, removePunct = TRUE,concatenator=" ",verbose=FALSE)

n2 <- ngrams(list(unlist(c1_tokenized)),n=2)
#n2 <- ngrams(list(unlist(c1_tokenized_rf)),n=2)
n2_table = table(n2)
n2_df = data.frame(n2_table)

#n2_df[grepl("cell_",n2_df$n2),]

n4 <- ngrams(list(unlist(c1_tokenized_rf)),n=4)
n4_table = table(n4)
n4_df = data.frame(n4_table)

c1_tokenized <- tokenize(c1, removePunct = TRUE, ngrams = 1:3)
str(unlist(c1_tokenized))

n3 <- ngrams(c1_tokenized,n=3)

summary(c1)

dfm1 <- dfm(c1)

data.frame(table(unlist(l1)))

# Create a dataframe of ngrams

n2_df <- as.data.frame(table(n2))


n3 <- ngrams(c1,n=3)
n3_df <- as.data.frame(table(n3))
saveRDS(n3_df, file="n3.rds")

n4 <- ngrams(t1,n=4)
n5 <- ngrams(t1,n=5)
n6 <- ngrams(t1,n=6)
n7 <- ngrams(t1,n=7)
n8 <- ngrams(t1,n=8)
n9 <- ngrams(t1,n=9)



saveRDS(n1,"n1.rds")
saveRDS(n2,"n2.rds")
saveRDS(n3,"n3.rds")
saveRDS(n4,"n4.rds")
saveRDS(n5,"n5.rds")
saveRDS(n6,"n6.rds")
saveRDS(n7,"n7.rds")
saveRDS(n8,"n8.rds")
saveRDS(n9,"n9.rds")
