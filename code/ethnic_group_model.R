dictionary_ethnic <- dictionary(file = here("dictionaries", "ethnic_dictionary.YAML"), format = "YAML")

lookup <- dfm_lookup(dfmTrainArticles2, dictionary_ethnic)

t <- quanteda::convert(lookup, to = "matrix")
y <- if_else(t[,1] > 0,1,0)
sum(y)/length(y)

modelethnic <- newsmap::textmodel_newsmap(dfmTrainArticles2, lookup)

modPredictEthnic<- predict(modelethnic, newdata = dfmTestArticles2, confidence = TRUE)

modPredictEthnic <- as_tibble(modPredictEthnic)

modPredictEthnic <- transform(modPredictEthnic, class_fixed = ifelse(confidence.fit < 0, 0, class))

modPredictEthnic <- modPredictEthnic %>% mutate(class_fixed = as.factor(class_fixed))

idxna <- is.na(modPredictEthnic[, 1])

prediction <- modPredictEthnic[!idxna, 3]

prediction <- as.factor(prediction)

reference <- articleAnnotation2[!idxna, 4]

reference <- as.factor(reference)

resultsEthnic <- caret::confusionMatrix(prediction, reference)

classresultsEthnic <- resultsEthnic[["byClass"]]

classresultsEthnic

resultsEthnic

refpred <- data.frame(reference = reference, prediction = prediction)

table(refpred)
