dictionary_discrim <- dictionary(file = here("dictionaries", "post_process_single_discrim_dictionary.YAML"), format = "YAML")

lookup <- dfm_lookup(dfmTrainArticles1, dictionary_discrim)

modelABDiscrim <- newsmap::textmodel_newsmap(dfmTrainArticles1, lookup)

modPredictABDiscrim <- predict(modelABDiscrim, newdata = dfmTestArticles1, confidence = TRUE)

modPredictABDiscrim <- as_tibble(modPredictABDiscrim)

modPredictABDiscrim <- transform(modPredictABDiscrim, class_fixed = ifelse(confidence.fit < 0, 0, class))

modPredictABDiscrim <- modPredictABDiscrim %>% mutate(class_fixed = as.factor(class_fixed))

idxna <- is.na(modPredictABDiscrim[, 1])

prediction <- modPredictABDiscrim[!idxna, 3]

prediction <- as.factor(prediction)

reference <- articleAnnotation1[!idxna, 6]

resultsABDiscrim <- caret::confusionMatrix(prediction, reference, positive = "1")

classResultsABDiscrim <- resultsABDiscrim[["byClass"]]

classResultsABDiscrim

resultsABDiscrim

refpred <- data.frame(reference = reference, prediction = prediction)

table(refpred)
idx <- refpred[refpred$reference == 0 & refpred$prediction == 1, ]
idx <- as.numeric(as.character(rownames(idx)))

test <- articleAnnotation1[!idxna,]
test <- test[idx,]
