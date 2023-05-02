
rm(list = ls())
options(scipen = 6, digits = 4)
memory.limit(30000000)

## ---------------------------

## load up the packages we will need:  (uncomment as required)

if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
}

pacman::p_load(rio, tidyverse, here, quanteda, newsmap, caret)

source(here("code", "functions.r"))
stopwords <- readRDS(here("data-raw", "extra_stopwords.rds")) # load stopword list

stopwords$word <- stopwords$word %>% tolower() # convert stopwords to lowercase

# Load Data
dict <- dictionary(file = here("dictionaries", "variables.YAML"), format = "YAML")

sentenceAnnotation <- import(here("data", "sentenceAnnotation.rds"))

articleAnnotation <- import(here("data", "articleAnnotation.rds"))

dfmTest <- import(here("data", "dfmTest.rds"))

dfmTrain <- import(here("data", "dfmTrain.rds"))

dfmTestSent <- import(here("data", "dfmTestSentences.rds"))

dfmTrainSent <- import(here("data", "dfmTrainSentences.rds"))

trainLookup <- dfm_lookup(dfmTrainSent, dictionary = dict)

model <- textmodel_newsmap(dfmTrainSent, trainLookup, smooth = 1)

modPredict <- predict(model, newdata = dfmTestSent, confidence.fit = TRUE)

modPredict <- as_tibble(modPredict)

modPredict <- transform(modPredict, class_fixed = ifelse(confidence.fit < 0, 0, class))

modPredict <- modPredict %>% mutate(class_fixed = as.factor(class_fixed))

table(modPredict$class)

