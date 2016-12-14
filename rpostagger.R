library(NLP) 
library(openNLP)

extractPOS <- function(x, thisPOSregex) {
  x <- as.String(x)
  wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
  POSwords <- subset(POSAnnotation, type == "word")
  tags <- sapply(POSwords$features, '[[', "POS")
  thisPOSindex <- grep(thisPOSregex, tags)
  tokenizedAndTagged <- sprintf("%s/%s", x[POSwords][thisPOSindex], tags[thisPOSindex])
  untokenizedAndTagged <- paste(tokenizedAndTagged, collapse = " ")
  untokenizedAndTagged
}

p <- function(...,sep) {
  if (sep == "")
    paste("^",...,"$",sep=sep)  
  else
    paste(...,sep=sep)  
  
}


#https://www.coursera.org/learn/data-science-project/module/VNKmf/discussions/HmPU3OvyEeWfwAohgaM63Q
#https://www.coursera.org/learn/data-science-project/module/VNKmf/discussions/V40gAPE3EeWFuw7QEATDpw

# ngrams
n[1:4]_sw_df_s - n4_sw
n[1:4]_df_s - n4

#wa
wa_sw3_df
wa_3_df

wa_sw2_df
wa_2_df

waSearch <- function(word1,word2="") {
  word1 <- cleanWord(word1)
  
  # Search for first word Count
  word1=paste("^",word1,"$",sep="") 
  print(word1)

  if (word2 != "")  {
    word2 <- cleanWord(word2)
    word2=paste("^",word2,"$",sep="") 
    print(word2)
    
  } else {
    word2 = "*"
    print(word2)
  }
  #print(head(wa_dfm[grepl(word1,wa_dfm$word1) & grepl(y,wa_dfm$word2),],5))
  #print(head(wa_dfm_sw[grepl(word1,wa_dfm_sw$word1) & grepl(y,wa_dfm_sw$word2),],5))
  wa_df <- as.data.frame(wa_dfm)
  wa_sw_df2 <- as.data.frame(wa_dfm_sw2)
  wa_dfm_2
  print(head(wa_df[grepl(word1,wa_df$word1) & grepl(word2,wa_df$word2),],20))
  print(head(wa_sw_df[grepl(word1,wa_sw_df$word1) & grepl(word2,wa_sw_df$word2),],20))
  print(head(wa_sw_df2[grepl(word1,wa_sw_df2$word1) & grepl(word2,wa_sw_df2$word2),],20))
  
}




