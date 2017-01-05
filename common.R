#library("tm")
library("utils")
library("R.utils")
library("hash")
library("tuple")
library("data.table")
library("sets")
library("wordcloud")

#load Packages
loadPackages <- function() {
  # List of packages for session
  .packages = c("tm", "utils", "R.utils","hash","tuple","quanteda","data.table","sets","stringr")
  
  # Install CRAN packages (if not already installed)
  .inst <- .packages %in% installed.packages()
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# searchString is space delimited
listOfNums <- function(searchString,hash) {
  string_list <- strsplit(trim(searchString),"\\W+")  
  #print(string_list)
  new_string_list <- sapply(string_list,cleanWord)
  #print(new_string_list)
  hash_list <- sapply(new_string_list,hashit,hash)
  #print(hash_list)
  unname(hash_list)
}

hashit <- function(charstring,hash) {
  
  #print(charstring)
  result <- NULL
  if (!has.key(charstring,hash)) {
    #9999999
    1
  }
  else {
    result <- hash[charstring]
    unname(values(result))
  }
}

unhashit <- function(hashint,rhash) {
  unname(values(rhash[as.character(hashint)]) ) 
}

loadDT <- function(source_dir) {
  #rm(list=ls())
  
  setwd(source_dir)
  g1 <- readRDS("g1.rds")
  g2 <- readRDS("g2.rds")
  g3 <- readRDS("g3.rds")
  g4 <- readRDS("g4.rds")
  
  g1$Row <- c(1:dim(g1)[1])
  g <- tuple(g1,g2,g3,g4)
  g
}  

cleanWord <- function(word) {
  # remove profanity
  # remove characters
  tolower(gsub("[?.;!¡¿·']", "", word))
}

loadDTC <- function(source_dir) {
  #rm(list=ls())
  
  setwd(source_dir)
  g1 <- readRDS("g1c.rds")
  g2 <- readRDS("g2c.rds")
  g3 <- readRDS("g3c.rds")
  g4 <- readRDS("g4c.rds")
  
  #g1 <- g1[1:10,]
  #g2 <- g2[1:1000,]
  #g3 <- g3[1:1000,]
  #g4 <- g4[1:1000,]
  
  library(sets)
  # SW has more phrases
  library(hash)
  g1$Row <- c(1:dim(g1)[1])
  g <- tuple(g1,g2,g3,g4)
  g
}  

loadDTCA <- function(source_dir) {

  #rm(list=ls())
  #print(source_dir)
  setwd(source_dir)
  g1 <- readRDS("g1ca.rds")
  g2 <- readRDS("g2ca.rds")
  g3 <- readRDS("g3ca.rds")
  g4 <- readRDS("g4ca.rds")
  
  library(sets)
  # SW has more phrases
  library(hash)
  g1$Row <- c(1:dim(g1)[1])
  g <- tuple(g1,g2,g3,g4)
  g
}  
