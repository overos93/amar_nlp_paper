## ---------------------------
##
## Script name: Converting Articles to Sentence Level Data
##
## Author: Henry Overos
##
## Date Created: 1/19/2022
##
## Email: hoveros@terpmail.umd.edu
##
## ---------------------------
##
## Notes: Every time you edit the code, please leave a short, dated note on the changes or additions made
##  2022-01-19 - created file from template and started working on creating this sentence-level data
##               The goal of this document is to create a small set of sentence-level data for hand coding.
##
##  2022-02-04 - I changed the code and hand-selected Protest 3 because there's fake news stories that 
##                were being selected (Onion and Babylon Bee)
## ---------------------------
rm(list = ls())
## set working directory for Mac and PC
currentuser <- Sys.info()['effective_user']
## setwd("~") mac
# Henry laptop
if (currentuser == 'Henry') {
  setwd("C:/Users/Henry/Dropbox/10-19 Research Projects/11 AMAR Expansion/AMAR21/AMAR")
  print(getwd())
# Oja laptop
} else if (currentuser == 'oja') {
  setwd("/Users/oja/Dropbox/AMAR21/")
  print(getwd())
} else {
  print("USER NOT DETECTED. Ensure that you have downloaded the replication materials.")
}

## ---------------------------
set.seed(1236)
options(scipen = 6, digits = 4)
memory.limit(30000000)

## ---------------------------

## load up the packages we will need:  (uncomment as required)

if("pacman" %in% rownames(installed.packages()) == FALSE){
  install.packages("pacman")
}

pacman::p_load(caret, newsmap, purrr, quanteda, readxl, rvest, stringr, tidyverse, rio, xml2)

## load data

testUSProtest <- rio::import("data/north_america/united_states/test/uta_coded_test/_AHL _RH _United States _Protest.xlsx")

## selecting articles coded as generally about protest

protestArticles <- testUSProtest %>% filter(Protest > 0)
# First sample

p1Articles <- protestArticles %>% filter(Protest_Type == 1) %>% slice_sample(., n=7)
p2Articles <- protestArticles %>% filter(Protest_Type == 2) %>% slice_sample(.,n=7)
p3Articles <- protestArticles %>% filter(Protest_Type == 3)
p3Articles <- p3Articles[c(2,5,26,49,120,134,147), ]

protestSample <- rbind(p1Articles,p2Articles,p3Articles)

protestSample$Title


# March 18, making sample 2
protestSample2 <- testUSProtest %>% filter(Protest == 1) %>% slice_sample(n = 70)

protestSample2 <- protestSample2 %>% anti_join(.,protestSample, by = "Title")

protestSample2 <- protestSample2[1:40,]


#Scrape stories

parseText <- function(nod){
  
  body <- str_squish(nod %>% read_html() %>%
                       html_nodes('p') %>% #collects all text in article
                       html_text())
  one_body <- paste(body, collapse = " ") # puts all of the text together
  
  return(one_body)
}

parseText2 <- function(nod){
  
  body <- str_squish(nod %>% read_html() %>%
                       html_nodes('body') %>% #collects all text in article
                       html_text())
  one_body <- paste(body, collapse = " ") # puts all of the text together
  
  
  return(one_body)
}

# This function simply continues to parse text even if there is an error
safelyParseText <- safely(parseText)

for (i in 1:nrow(protest_articles_test)) {
  body <- safelyParseText(protest_articles_test$URL[i])
  protest_articles_test$body[i] <- body
  Sys.sleep(0.5)
}

for (i in 1:nrow(test)) {
  body <- safelyParseText(protestSample2$URL[i])
  protestSample2$body[i] <- body
  Sys.sleep(0.5)
}


#us_protest
for (i in 1:nrow(protestSample2)) {
  body <- safelyParseText(protestSample2$URL[i])
  protestSample2$body[i] <- body
  Sys.sleep(0.5)
}

protestSample2$body <- as.character(protestSample2$body)

protestSample2 <- select(protestSample2, ID, URL, Title, body, everything())

protestTexts2 <- protestSample2 %>% select(ID, URL, Title, body)

sentences2 <- tokenize_sentence(protestTexts2$body)

textSentences2 <- data.frame()

for (i in 1:length(sentences2)) {
  n <- length(sentences2[[i]])
  textDF <- data.frame(article_id = rep(i,n), sentence = sentences2[[i]])
  textSentences2 <- rbind(textSentences2, textDF)
}

protestTexts2 <- protestTexts2 %>% mutate(article_id = c(1:40))

textSentences2 <- left_join(textSentences2, protestTexts2, by = "article_id") %>% select(ID, URL, Title, sentence)

protestSample$body <- as.character(protestSample$body)

protestSample <- select(protestSample, ID, URL, Title, body, everything())

protestTexts <- protestSample %>% select(ID, URL, Title, body)

sentences <- tokenize_sentence(protestTexts$body)

textSentences <- data.frame()

for (i in 1:length(sentences)) {
  n <- length(sentences[[i]])
  textDF <- data.frame(article_id = rep(i,n), sentence = sentences[[i]])
  textSentences <- rbind(textSentences, textDF)
}

protestTexts <- protestTexts %>% mutate(article_id = c(1:21))

textSentences <- left_join(textSentences, protestTexts, by = "article_id") %>% select(ID, URL, Title, sentence)

setwd("data/north_america/united_states/test/uncoded_test")

write.csv(textSentences, file = "testUSProtest_withtext_sentencelevel.csv")

export(textSentences2, file = "testUSProtest_withtext_sentencelevel2.csv")

#testProtestSentences <- read_csv("testUSProtest_withtext_sentencelevel.csv", 
#                                 +     na = "NA")
      