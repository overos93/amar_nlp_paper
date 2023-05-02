# Analysis of newsmap on afam + protests + dalit data

## Binary Model ####

postprocDict <- dictionary(file = here("dictionaries", "post_process_single_prot_dictionary.YAML"), format = "YAML")

trainLookup <- dfm_lookup(dfmTrainArticles2, dictionary = postprocDict)

modelAB2 <- textmodel_newsmap(dfmTrainArticles2, trainLookup)

modPredictAB2 <- predict(modelAB2, newdata = dfmTestArticles2, confidence = TRUE)

modPredictAB2 <- as_tibble(modPredictAB2)

modPredictAB2 <- transform(modPredictAB2, class_fixed = ifelse(confidence.fit < 0, 0, class))

modPredictAB2 <- transform(modPredictAB2, class_fixed = ifelse(class_fixed == 2, 0, class_fixed))

modPredictAB2 <- modPredictAB2 %>% mutate(class_fixed = as.factor(class_fixed))

idxna <- is.na(modPredictAB2[, 1])

prediction <- modPredictAB2[!idxna, 3]

prediction <- as.factor(prediction)

reference <- articleAnnotation2[!idxna, 5]

resultsAB2 <- caret::confusionMatrix(prediction, reference, positive = "1")

classResultsAB2 <- resultsAB2[["byClass"]]
classResultsAB2

refpred <- data.frame(reference = reference, prediction = prediction)

idx <- refpred[refpred$reference == 0 & refpred$prediction == 1, ]
idx <- as.numeric(as.character(rownames(idx)))

test <- articleAnnotation2[!idxna,]
test <- test[idx,]

# ## Multiclass Models ####
# multiprotestDict <- dictionary(file = here("dictionaries", "3_level_dictionary.YAML"), format = "YAML")
# 
# ### Single-Dictionary Model ####
# 
# #### Article Level ####
# trainLookup <- dfm_lookup(dfmTrainArticles2, dictionary = multiprotestDict)
# 
# modelAM2 <- textmodel_newsmap(dfmTrainArticles2, trainLookup)
# 
# modelOutputAM2 <- modelAM1[["model"]]
# 
# export(modelAM2, here("data", "modelArticlesMulticlass1.rds"))
# 
# modelOutputAM2 <- t(modelOutputAM2)
# 
# modPredictAM2 <- predict(modelAM2, newdata = dfmTestArticles2, confidence = TRUE)
# 
# modPredictAM2 <- as_tibble(modPredictAM2)
# 
# modPredictAM2 <- transform(modPredictAM2, class_fixed = ifelse(confidence.fit < 0, 0, class))
# 
# modPredictAM2 <- transform(modPredictAM2, class_fixed = ifelse(class_fixed == 3, 0, class_fixed))
# 
# modPredictAM2 <- modPredictAM2 %>% mutate(class_fixed = as.factor(class_fixed))
# 
# idxna <- is.na(modPredictAM2[, 1])
# 
# prediction <- modPredictAM2[!idxna, 3]
# 
# prediction <- as.factor(prediction)
# 
# articleAnnotation2 <- articleAnnotation2 %>%
#   mutate(type_combined = ifelse(Protest_Type == 3,
#                                 2,
#                                 ifelse(Protest_Type == 2 | Protest_Type == 1,
#                                        1,
#                                        0
#                                 )
#   ))
# 
# reference <- articleAnnotation2[!idxna,]$type_combined
# 
# reference <- as.factor(reference)
# 
# resultsAM2 <- caret::confusionMatrix(prediction, reference)
# 
# classResultsAM2 <- resultsAM2[["byClass"]]
