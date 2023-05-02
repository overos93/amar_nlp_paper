## ---------------------------
##
## Script name: Topic Analysis Articles
##
## Author: Henry Overos
##
## Date Created: 2022-05-09
##
## Email: 
##
## ---------------------------
##
## Notes: Every time you edit the code,
##        please leave a short, dated note on the changes or additions made
##   
##
## ---------------------------
rm(list = ls())
options(scipen = 6, digits = 4)
memory.limit(30000000)

## ---------------------------

## load up the packages we will need:  (uncomment as required)

if("pacman" %in% rownames(installed.packages()) == FALSE){
  install.packages("pacman")
}

pacman::p_load(here, rio, tidyverse, quanteda, quanteda.textmodels,newsmap, seededlda)

stopwords <- readRDS(here("data-raw", "extra_stopwords.rds")) # load stopword list

stopwords$word <- stopwords$word %>% tolower() # convert stopwords to lowercase

source(here("code", "functions.r"))

pacman::p_load(rio, tidyverse, here)

usProtest <- import(here("data-raw", "us_protest_train.rds"))

model <- import(here("data", "binaryModelSentences.rds"))

usProtest$body <- as.character(usProtest$body)

protestCorp <- corpus(usProtest, text_field = "body")

protestCorp <- corpus_reshape(protestCorp, to = "sentences")

protestToks <- clean_corpus(protestCorp)

protestDFM <- dfm(protestToks)

lda <- textmodel_lda(k = 10, protestDFM)

terms(lda)

protestDFM$topics <- topics(lda)

table(protestDFM$topics)

topics <- topics(lda)

export(lda, here("data", "lda_results.rds"))

#seeded lda

protestDict <- dictionary(file = here("dictionaries", "3_level_dictionary.YAML"), format = "YAML")

protestDict <- protestDict[["Protest"]]

seededldares <- textmodel_seededlda(protestDFM, dictionary = protestDict)

beepr::beep()

terms(seededldares)

table(topics(seededldares))

