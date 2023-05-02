## ---------------------------
##
## Script name: Sentence Analysis, Levels 1 + 2 combined
##
## Author: Henry Overos
##
## Date Created: 2022-03-22
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
options(scipen = 10, digits = 2)
memory.limit(30000000)

## ---------------------------

## load up the packages we will need:  (uncomment as required)

if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
}

pacman::p_load(rio, tidyverse, here, quanteda, newsmap, caret)

source(here("code", "functions.r"))

# Load Data
sentenceAnnotation <- import(here("data", "sentenceAnnotation.rds"))

sentenceAnnotation <- sentenceAnnotation %>%
  mutate(type_combined = ifelse(type == 1 | type == 2,
    1,
    ifelse(type == 3,
      2,
      0
    )
  ))

dfmTestSent <- import(here("data", "dfmTestSentences.rds"))

dfmTrainSent <- import(here("data", "dfmTrainSentences.rds"))

dfmTestArticles <- import(here("data", "dfmTestSentences.rds"))

dfmTrainArticles <- import(here("data", "dfmTrain.rds"))


# Load Dictionary

firstprotestDict <- dictionary(file = here("dictionaries", "post_process_single_prot_dictionary.yaml"),format = "YAML")
multiprotestDict <- dictionary(file = here("dictionaries", "3_level_dictionary.YAML"), format = "YAML")

# Run Model

trainLookup <- dfm_lookup(dfmTrain, dictionary = firstprotestDict)

model <- textmodel_newsmap(dfmTrain, trainLookup, smooth = 3)

modelOutput <- model[["model"]]

export(model, here("data","multimodel.rds"))

modelOutput <- t(modelOutput)

modPredict <- predict(model, newdata = dfmTest, confidence.fit = TRUE)

modPredict <- as_tibble(modPredict)

modPredict <- transform(modPredict, class_fixed = ifelse(confidence.fit < 0, 0, class))

modPredict <- modPredict %>% mutate(class_fixed = as.factor(class_fixed))

sentenceAnnotation$type_combined <- as.factor(sentenceAnnotation$type_combined)

topics <- plyr::revalue(topics, c("general_news" = 0, "sports" = 0, "prot_1" = 1, "prot_2" = 2))
topics[is.na(topics)] <- 0
topics
type_human_annotated <- sentenceAnnotation$type_combined

results <- caret::confusionMatrix(topics, type_human_annotated)

classResults <- results[["byClass"]]
classResults

results

table(topics)
table(type_human_annotated)
