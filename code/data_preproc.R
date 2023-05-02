## ---------------------------
##
## Script name: Cleaning and Preparing Data for Analysis
##
## Author: Henry Overos
##
## Date Created: 2022-03-09
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

stopwords <- readRDS(here("data-raw", "extra_stopwords.rds")) # load stopword list

stopwords$word <- stopwords$word %>% tolower() # convert stopwords to lowercase

codes <- import(file = here("data-raw", "umd_coded_resolved.xlsx"))

usProtest <- readRDS(here("data-raw", "us_protest_train.rds"))

# Cleaning Coding Sheet

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
  mutate(protest = as.factor(protest)) %>% mutate(type = as.factor(type))

saveRDS(sentenceAnnotation, file = here("data", "sentenceAnnotation.rds"))

articleAnnotation <- import(here("data", "articleAnnotation.rds"))
# articleAnnotation$Protest_Type[is.na(articleAnnotation$Protest_Type)] <- "0"
# articleAnnotation <- articleAnnotation %>%
#   mutate(type_combined = ifelse(Protest_Type == "1" | Protest_Type == "2",
#                                 "1",
#                                 ifelse(Protest_Type == "3",
#                                        2,
#                                        0
#                                 )
#   ))
#articleAnnotation <- articleAnnotation[unique(articleAnnotation$ID),]
# export(articleAnnotation, here("data", "articleAnnotation.rds"))

compounds <- c(
  "african american*", "black lives matter", "black american*", "black *man", "black *men",
  "verbal opposition", "opposition speech", "codemn speech*", "public letter*",
  "court action", "sign petition*", "destruct property", "property destruct*", "traffic block*"
) # creating set of multiword tokens

# Create Test and Train Sets
## Separate Test and Training Articles
testIdxSentences <- unique(codes$ID)

testSentences <- usProtest[testIdxSentences, ]

testidxArticles <- unique(articleAnnotation$ID)

testArticles <- usProtest[testidxArticles, ]

train <- usProtest %>% anti_join(., testArticles, by = c("title" = "title"))
# Fix text variable to be character instead of list (result of web scrape)

testArticles$body <- as.character(testArticles$body)

testSentences$body <- as.character(testSentences$body)

train$body <- as.character(train$body)

testArticles$body[sapply(testArticles$body, is.null)] <- NA

testSentences$body[sapply(testSentences$body, is.null)] <- NA

train$body[sapply(train$body, is.null)] <- NA

# Article-Level First
## Train Set
corpusTrain <- corpus(train, text_field = "body")

toksTrain <- clean_corpus(corpusTrain)

toksTrain <- tokens_compound(toksTrain, pattern = compounds)

dfmTrain <- dfm(toksTrain)

## Test Set
corpusTestArticles <- corpus(testArticles, text_field = "body")

toksTestArticles <- clean_corpus(corpusTestArticles)

toksTestArticles <- tokens_compound(toksTestArticles, pattern = compounds)

dfmTestArticles <- dfm(toksTestArticles)

# Sentence-Level Second
## Train

corpusTrainSentences <- corpus_reshape(corpusTrain, to = "sentences")

toksTrainSentences <- clean_corpus(corpusTrainSentences)

toksTrainSentences <- tokens_compound(toksTrainSentences, pattern = compounds)

dfmTrainSentences <- dfm(toksTrainSentences)

## Test

corpusTestSentences <- corpus(testSentences, text_field = "body")

corpusTestSentences <- corpus_reshape(corpusTestSentences, to = "sentences")

toksTestSentences <- clean_corpus(corpusTestSentences)

toksTestSentences <- tokens_compound(toksTestSentences, pattern = compounds)

dfmTestSentences <- dfm(toksTestSentences)

# Save final objects in data folder

rio::export(dfmTestArticles, file = here("data", "dfmTestArticles.rds"))

rio::export(dfmTrain, file = here("data", "dfmTrainArticles.rds"))

rio::export(dfmTestSentences, file = here("data", "dfmTestSentences.rds"))

rio::export(dfmTrainSentences, file = here("data", "dfmTrainSentences.rds"))


##### OLD CODE


# Looking at top words in Corpus Test
# corpusTest$type <- sentenceAnnotation$type
# 
# corpusTestP1 <- corpus_subset(corpusTest, type == 1)
# corpusTestP2 <- corpus_subset(corpusTest, type == 2)
# corpusTestP3 <- corpus_subset(corpusTest, type == 3)
# 
# tokst1 <- clean_corpus(corpusTestP1)
# dfmt1 <- dfm(tokst1)
# tokst2 <- clean_corpus(corpusTestP2)
# dfmt2 <- dfm(tokst2)
# tokst3 <- clean_corpus(corpusTestP3)
# dfmt3 <- dfm(tokst3)
# 
# wordFreq1 <- quanteda.textstats::textstat_frequency(dfmt1)
# wordFreq2 <- quanteda.textstats::textstat_frequency(dfmt2)
# wordFreq3 <- quanteda.textstats::textstat_frequency(dfmt3)
# 
# wordFreq1$type <- 1
# wordFreq2$type <- 2
# wordFreq3$type <- 3
# 
# wordfreqs <- rbind(wordFreq1, wordFreq2, wordFreq3)
# export(wordfreqs, file = here("data", "wordFrequencies.xlsx"))
# 
# uniques <- wordfreqs %>% group_by(type) %>% 
#   summarise(count = n_distinct(feature))







# Convert Corpuses to DFM






