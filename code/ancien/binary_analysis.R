
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
sentenceAnnotation <- import(here("data", "sentenceAnnotation.rds"))

articleAnnotation <- import(here("data", "articleAnnotation.rds"))

dfmTestArticles <- import(here("data", "dfmTestArticles.rds"))

dfmTrainArticles <- import(here("data", "dfmTrainArticles.rds"))

dfmTestSent <- import(here("data", "dfmTestSentences.rds"))

dfmTrainSent <- import(here("data", "dfmTrainSentences.rds"))

# Final Model Sentences
postprocDict <- dictionary(file = here("dictionaries", "post_process_single_prot_dictionary.YAML"), format = "YAML")

trainLookup <- dfm_lookup(dfmTrainSent, dictionary = postprocDict)

model <- textmodel_newsmap(dfmTrainSent, trainLookup, smooth = 1)

modPredict <- predict(model, newdata = dfmTestSent, confidence.fit = TRUE)

modPredict <- as_tibble(modPredict)

modPredict <- transform(modPredict, class_fixed = ifelse(confidence.fit < 0, 0, class))

modPredict <- modPredict %>% mutate(class_fixed = as.factor(class_fixed))

results <- caret::confusionMatrix(modPredict$class_fixed, sentenceAnnotation$protest, positive = "1")

classResults <- results[["byClass"]]

results

classResults

# Final Model Articles
trainLookup <- dfm_lookup(dfmTrainArticles, dictionary = postprocDict)

modelA <- textmodel_newsmap(dfmTrainArticles, trainLookup, smooth = 1)

modPredict <- predict(modelA, newdata = dfmTestArticles, confidence.fit = TRUE)

modPredict <- as_tibble(modPredict)

modPredict <- transform(modPredict, class_fixed = ifelse(confidence.fit < 0, 0, class))

modPredict <- modPredict %>% mutate(class_fixed = as.factor(class_fixed))

idxna <- is.na(modPredict[,1])

prediction <- modPredict[!idxna,3]

reference <- articleAnnotation[!idxna,4]

results <- caret::confusionMatrix(prediction, reference)

length(prediction)

length(reference)

classResults <- results[["byClass"]]

results

classResults

export(model, here("data", "binaryModelSentences.rds"))



# Final Model Articles
trainLookup <- dfm_lookup(dfmTrainArticles, dictionary = postprocDict)

model <- textmodel_newsmap(dfmTrainArticles, trainLookup)

modPredict <- predict(model, newdata = dfmTestArticles, confidence.fit = TRUE)

modPredict <- as_tibble(modPredict)

modPredict <- transform(modPredict, class_fixed = ifelse(confidence.fit < 0, 0, class))

modPredict <- modPredict %>% mutate(class_fixed = as.factor(class_fixed))

articleAnnotation <- articleAnnotation[!is.na(articleAnnotation$body),]

results <- caret::confusionMatrix(modPredict$class_fixed, articleAnnotation$Protest)

classResults <- results[["byClass"]]

results

classResults

#
## Compare the base dictionary to the binary model

trainLookup <- dfm_lookup(dfmTestSent, dictionary = postprocDict)

raw_lookup <- convert(trainLookup, to = "data.frame")

raw_lookup <- raw_lookup %>% mutate(protest = if_else(Protest > 0, 1, 0))
  
raw_results <- caret::confusionMatrix(as.factor(raw_lookup$protest),
                                      as.factor(sentenceAnnotation$protest))
classResults <- results[["byClass"]]

raw_results
