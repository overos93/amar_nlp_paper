## ---------------------------
##
## Script name: Cleaning and Preparing Data for Analysis
##
## Author: Henry Overos
##
## Date Created: 2022-03-30
##
## Email: hoveros@umd.edu
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

if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
}

pacman::p_load(rio, tidyverse, here, quanteda, newsmap, caret)

# Load functions
source(here("code", "functions.R"))

# Load Data

more_stopwords <- data.frame(word = c("material", "llc", "network", "quote*", "headlines",
                                      "subscri*", "cookie*", "factset", "null"))

stopwords <- readRDS(here("data-raw", "extra_stopwords.rds")) # load stopword list

stopwords <- rbind(stopwords,more_stopwords)

export(stopwords, here("data-raw", "extra_stopwords.rds"))

stopwords$word <- stopwords$word %>% tolower() # convert stopwords to lowercase

codes <- import(file = here("data-raw", "umd_coded_resolved.xlsx"))
us_afam_train <- readRDS("C:/Users/Henry/Dropbox/10-19 Research Projects/11_amar_research_note/data-raw/us_afam_train.rds")
us_protest_train <- readRDS("C:/Users/Henry/Dropbox/10-19 Research Projects/11_amar_research_note/data-raw/us_protest_train.rds")
load("C:/Users/Henry/Dropbox/10-19 Research Projects/11_amar_research_note/data-raw/us_protest_withtext.RData")

train <- rbind(us_afam_train,us_protest_train)

t2 <-  import(here("data-raw", "test_usafam.csv"))
t2 <- t2 %>% mutate(ID = as.numeric(row_id))
articleAnnotation2 <-  import(here("data-raw", "_AHL _United States _AFAM.xlsx"))
articleAnnotation2 <- articleAnnotation2 %>% left_join(.,t2, by = "ID")
articleAnnotation2 <- articleAnnotation2 %>% select(ID,
                                                    Title,
                                                    body,
                                                    Protest = Protest.x,
                                                    Protest_Type)

t4 <-  import(here("data-raw", "test_usprotest.csv"))
t4 <- t4[-1,1:6]
t4$V1 <- as.numeric(t4$V1)
colnames(t4) <- c("ID","URL", "Title", "body", "discrimination", "protest")
articleAnnotation4 <-  import(here("data-raw", "_AHL _RH _United States _Protest.xlsx"))
articleAnnotation4 <- articleAnnotation4 %>% left_join(.,t4, by = "ID")
articleAnnotation4 <- articleAnnotation4 %>% select(ID,
                                                    Title = Title.x,
                                                    body,
                                                    Protest,
                                                    Protest_Type)

articleAnnotation2 <- articleAnnotation2 %>%
  mutate(Protest = as.factor(Protest)) %>% mutate(Protest_Type = as.factor(Protest_Type))

articleAnnotation4 <- articleAnnotation4 %>%
  mutate(Protest = as.factor(Protest)) %>% mutate(Protest_Type = as.factor(Protest_Type))

articleAnnotation <- rbind(articleAnnotation2,articleAnnotation4)

articleAnnotation[is.na(articleAnnotation$Protest_Type),]$Protest_Type <- "0"

# Cleaning Coding Sheets

sentenceAnnotation <- transform(codes,
                                resolved_protest_fixed = ifelse(!is.na(resolved_protest),
                                                                resolved_protest,
                                                                protest_oja))

sentenceAnnotation <- transform(sentenceAnnotation,
                                resolved_type_fixed = ifelse(!is.na(resolved_type),
                                                             resolved_type,
                                                             type_oja))

sentenceAnnotation <- sentenceAnnotation %>% select(ID,
                                                    Title,
                                                    sentence,
                                                    protest = resolved_protest_fixed,
                                                    type = resolved_type_fixed) %>% 
  mutate(protest = as.factor(protest)) %>%
  mutate(type = as.factor(type))

table(sentenceAnnotation$protest)

saveRDS(sentenceAnnotation, file = here("data", "sentenceAnnotation.rds"))


saveRDS(articleAnnotation, file = here("data", "articleAnnotation.rds"))

# Create Test and Train Sets
`%!in%` <- function(x,y){!(`%in%`(x,y))} 

## Train - Articles
trainArticles <- train %>% filter(train$title %!in% articleAnnotation$Title)

## Test - Articles
testArticles <- train %>% filter(train$title %in% articleAnnotation$Title)

## Train - Sentences

## Test - Sentences

# Fix text variable to be character instead of list (result of web scrape)

test$body <- as.character(test$body)

testSent$body <- as.character(testSent$body)

train$body <- as.character(train$body)

testSent$body[sapply(testSent$body, is.null)] <- NA

test$body[sapply(test$body, is.null)] <- NA

train$body[sapply(train$body, is.null)] <- NA

# Create and Clean Corpuses

corpusTrain <- corpus(train, text_field = "body")

corpusTestSent <- corpus(testSent, text_field = "body")

corpusTrainSent <- corpus_reshape(corpusTrain, to = "sentences")

corpusTestSent <- corpus_reshape(corpusTestSent, to = "sentences")

toksTrainSent <- clean_corpus(corpusTrainSent)

toksTestSent <- clean_corpus(corpusTestSent)

toksTrain <- clean_corpus(corpusTrain)

toksTest <- clean_corpus(corpusTest)

# Convert Corpuses to DFM

compounds <- c(
  "african american*", "black lives matter", "black american*", "black *man", "black *men",
  "verbal opposition", "opposition speech", "codemn speech*", "public letter*",
  "court action", "sign petition*", "destruct property", "property destruct*", "traffic block*",
  "civil disobedience", "martin luther king jr*"
) # creating set of multiword tokens

toksTest <- tokens_compound(toksTest, pattern = compounds)

toksTrain <- tokens_compound(toksTrain, pattern = compounds)

toksTestSent <- tokens_compound(toksTestSent, pattern = compounds)

toksTrainSent <- tokens_compound(toksTrainSent, pattern = compounds)

dfmTest <- dfm(toksTest)

dfmTrain <- dfm(toksTrain)

dfmTestSent <- dfm(toksTestSent)

dfmTestSent <- dfm_subset(dfmTestSent, ntoken(dfmTestSent) > 0)

dfmTrainSent <- dfm(toksTrainSent)

export(dfmTest, file = here("data", "dfmTest.rds"))

export(dfmTrain, file = here("data", "dfmTrain.rds"))

export(dfmTestSent, file = here("data", "dfmTestsent.rds"))

export(dfmTrainSent, file = here("data", "dfmTrainsent.rds"))
