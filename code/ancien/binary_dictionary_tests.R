## ---------------------------
##
## Script name: binary dictionary tests
##
## Author: 
##
## Date Created: 2022-03-23
##
## Email: 
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

if("pacman" %in% rownames(installed.packages()) == FALSE){
  install.packages("pacman")
}

pacman::p_load(rio, tidyverse, here, quanteda, newsmap, caret)


# Load Dictionary

protestDict <- dictionary(file = here("dictionaries", "single_prot_dict.yaml"), format = "YAML")

# Initial Model
trainLookup <- dfm_lookup(dfmTrainSent, dictionary = protestDict)

model <- textmodel_newsmap(dfmTrainSent, trainLookup)

modPredict <- predict(model, newdata = dfmTestSent, confidence.fit = TRUE)

modPredict <- as_tibble(modPredict)

modPredict <- transform(modPredict, class_fixed = ifelse(confidence.fit < 0, 0, class))

modPredict <- modPredict %>% mutate(class_fixed = as.factor(class_fixed))

results <- caret::confusionMatrix(modPredict$class_fixed, sentenceAnnotation$protest)

classResults <- results[["byClass"]]

results

classResults

# Change in model for each individual word added
protestDictInit <- get_initial(protestDict)

## Create a list to add more words
protestDictAdd <- flatten_dictionary(protestDict, levels = 1)

# Generate Results for each word
dat_final <- data.frame(stringsAsFactors = FALSE)

row <- 0
cov <- 0
resAfe <- 0

protestDictTemp <- protestDictInit

for (j in 1:length(protestDictAdd[[1]])) {
  
  row <- sum(row, 1)
  
  wordAdded = protestDictAdd[[1]][j]
  
  protestDictTemp <- append_list_binary(protestDictInit, protestDictAdd, j)
  
  labelDfm <- dfm_lookup(dfmTrainSent, dictionary(protestDictTemp), levels = 1)
  
  map <- textmodel_newsmap(dfmTrainSent, labelDfm, smooth = 3)
  
  modPredict <- predict(map, newdata = dfmTestSent, confidence.fit = TRUE)
  
  modPredict <- as_tibble(modPredict)
  
  modPredict <- transform(modPredict, class_fixed = ifelse(confidence.fit < 0, 0, class))
  
  modPredict <- modPredict %>% mutate(class_fixed = as.factor(class_fixed))
  
  results <- caret::confusionMatrix(modPredict$class_fixed,
                                    sentenceAnnotation$type,
                                    mode = "prec_recall")
  
  cov <- get_coverage(labelDfm)
  
  resAfe <- newsmap::afe(dfmTrainSent, labelDfm, smooth = 3)
  
  s <- as.data.frame(results$byClass)
  
  overall <- as.data.frame(results$overall)
  
  t_overall <- t(overall)
  
  t_overall <- as.data.frame(t_overall)
  
  colnames(t_overall) <- rownames(overall)
  
  rownames(t_overall) <- colnames(overall)
  
  datTemp <- data.frame(
    j = j,
    noise = FALSE,
    word_added = wordAdded,
    coverage = cov,
    afe = resAfe,
    p = s$Precision[1],
    r = s$Recall[1],
    f1 = s$F1[1],
    stringsAsFactors = FALSE
  )
  
  if(j==1){datFinal <- datTemp}
  
  datFinal <- rbind(datFinal, datTemp)
  
  cat(sprintf(
    "%d Word Added %s (Coverage %.3f, F1 Score %.3f)", j,
    datTemp$word_added, datTemp$coverage, datTemp$f1
  ), "\n")
}

coverageInit <- datFinal$coverage[1]
afeInit <- datFinal$afe[1]
f1Init <- datFinal$f1[1]

datFinal$entropy_introduced <- afeInit - datFinal$afe
datFinal$cov_diff <- datFinal$coverage - coverageInit
datFinal$f1_diff <- f1Init - datFinal$f1


ggplot(datFinal, aes(x = cov_diff, y = f1_diff, label = word_added)) +
  geom_abline(slope = 0, intercept = 0) +
  geom_text()

finalWords <- datFinal[datFinal$f1_diff > 0 & datFinal$cov_diff > 0,]
finalWords


protestDictTemp <- protestDictInit
row <- 0
cov <- 0
resAfe <- 0

for (j in 1:length(protestDictAdd[[1]])) {
    
    row <- sum(row, 1)
    
    wordAdded = protestDictAdd[[1]][[j+1]]
    
    protestDictTemp <- append_list(protestDictTemp, protestDictAdd,1, j+1)
    
    labelDfm <- dfm_lookup(dfmTrainSent, dictionary(protestDictTemp), levels = 1)
    
    map <- textmodel_newsmap(dfmTrainSent, labelDfm, smooth = 3)
    
    modPredict <- predict(map, newdata = dfmTestSent, confidence.fit = TRUE)
    
    modPredict <- as_tibble(modPredict)
    
    modPredict <- transform(modPredict, class_fixed = ifelse(confidence.fit < 0, 0, class))
    
    modPredict <- modPredict %>% mutate(class_fixed = as.factor(class_fixed))
    
    results <- caret::confusionMatrix(modPredict$class_fixed,
                                      sentenceAnnotation$protest,
                                      mode = "prec_recall")
    
    cov <- get_coverage(labelDfm)
    
    resAfe <- newsmap::afe(dfmTrainSent, labelDfm, smooth = 3)
    
    s <- as.data.frame(results$byClass)
    
    s <- t(s)
    
    s <- as.data.frame(s)
    
    overall <- as.data.frame(results$overall)
    
    t_overall <- t(overall)
    
    t_overall <- as.data.frame(t_overall)
    
    colnames(t_overall) <- rownames(overall)
    
    rownames(t_overall) <- colnames(overall)
    
    datTemp <- data.frame(
      j = j,
      noise = FALSE,
      word_added = wordAdded,
      coverage = cov,
      afe = resAfe,
      precision = s$Precision[1],
      recall = s$Recall[1],
      f1 = s$F1[1],
      accuracy = t_overall$Accuracy,
      stringsAsFactors = FALSE
    )
    
    if(j==1){datFinal <- datTemp}
    
    datFinal <- rbind(datFinal, datTemp)
    
    cat(sprintf(
      "%d Word Added %s (Coverage %.3f, Accuracy %.3f)", j,
      datTemp$word_added, datTemp$coverage, datTemp$accuracy
    ), "\n")
}

coverageInit <- datFinal$coverage[1]
afeInit <- datFinal$afe[1]
f1Init <- datFinal$f1[1]

datFinal$entropy_introduced <- afeInit - datFinal$afe
datFinal$cov_diff <- datFinal$coverage - coverageInit
datFinal$f1_diff <- datFinal$f1 - f1Init


ggplot(datFinal, aes(x = cov_diff, y = f1_diff, label = word_added)) +
  geom_abline(slope = 0, intercept = 0) +
  geom_text()
