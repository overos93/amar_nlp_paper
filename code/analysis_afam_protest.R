# Analysis of newsmap on afam + protest data (t1)

## Binary Model ####

postprocDict <- dictionary(file = here("dictionaries", "post_process_single_prot_dictionary.YAML"), format = "YAML")

trainLookup <- dfm_lookup(dfmTrainArticles1, dictionary = postprocDict)

modelAB1 <- textmodel_newsmap(dfmTrainArticles1, trainLookup)

modPredictAB1 <- predict(modelAB1, newdata = dfmTestArticles1, confidence.fit = TRUE)

modPredictAB1 <- as_tibble(modPredictAB1)

modPredictAB1 <- transform(modPredictAB1, class_fixed = ifelse(confidence.fit < 0, 0, class))

modPredictAB1 <- transform(modPredictAB1, class_fixed = ifelse(class_fixed == 2, 0, class_fixed))

modPredictAB1 <- modPredictAB1 %>% mutate(class_fixed = as.factor(class_fixed))

idxna <- is.na(modPredictAB1[, 1])

prediction <- modPredictAB1[!idxna, 3]

prediction <- as.factor(prediction)

reference <- articleAnnotation1[!idxna, 4]

resultsAB1 <- caret::confusionMatrix(prediction, reference, positive = "1")

classResultsAB1 <- resultsAB1[["byClass"]]

## Multiclass Models ####
multiprotestDict <- dictionary(file = here("dictionaries", "3_level_dictionary.YAML"), format = "YAML")

### Single-Dictionary Model ####

#### Article Level ####
trainLookup <- dfm_lookup(dfmTrainArticles1, dictionary = multiprotestDict)

modelAM1 <- textmodel_newsmap(dfmTrainArticles1, trainLookup)

modelOutputAM1 <- modelAM1[["model"]]

export(modelAM1, here("data", "modelArticlesMulticlass1.rds"))

modelOutputAM1 <- t(modelOutputAM1)

modPredictAM1 <- predict(modelAM1, newdata = dfmTestArticles1, confidence.fit = TRUE)

modPredictAM1 <- as_tibble(modPredictAM1)

modPredictAM1 <- transform(modPredictAM1, class_fixed = ifelse(confidence.fit < 0, 0, class))

modPredictAM1 <- transform(modPredictAM1, class_fixed = ifelse(class_fixed == 3, 0, class_fixed))

modPredictAM1 <- modPredictAM1 %>% mutate(class_fixed = as.factor(class_fixed))

idxna <- is.na(modPredictAM1[, 1])

prediction <- modPredictAM1[!idxna, 3]

prediction <- as.factor(prediction)

articleAnnotation1 <- articleAnnotation1 %>%
  mutate(type_combined = ifelse(Protest_Type == 3,
                                2,
                                ifelse(Protest_Type == 2 | Protest_Type == 1,
                                       1,
                                       0
                                )
  ))

reference <- articleAnnotation1[!idxna,]$type_combined

reference <- as.factor(reference)

resultsAM1 <- caret::confusionMatrix(prediction, reference)

classResultsAM1 <- resultsAM1[["byClass"]]
