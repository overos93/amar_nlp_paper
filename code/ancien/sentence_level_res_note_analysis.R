## ---------------------------
##
## Script name: research note analysis at sentence level
##
## Author: Henry Overos
##
## Date Created: 2022-03-07
##
## Email: hoveros@umd.edu
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

source(here("code", "functions.r"))

# Load Data
sentenceAnnotation <- import(here("data", "sentenceAnnotation.rds"))

dfmTest <- import(here("data", "dfmTest.rds"))

dfmTrain <- import(here("data", "dfmTrain.rds"))

# Load Dictionary

protestDict <- dictionary(file = here("dictionaries", "variables.yaml"), format = "YAML")

protestDict <- protestDict[["Protest"]]

# Test Dictionary Words

## Get first words from dictionary
protestDictInit <- get_initial(protestDict)

## Create a list to add more words
protestDictAdd <- flatten_dictionary(protestDict, levels = 1)

# Generate Results for each word
dat_final <- data.frame(stringsAsFactors = FALSE)

row <- 0
cov <- 0
afe <- 0

protestDictTemp <- protestDictInit

# Change in model for each word added to dictionary

for (i in 1:3) {
  for (j in 1:length(protestDictAdd[[i]])) {
    
    row <- sum(row, 1)
    
    wordAdded = protestDictAdd[[i]][j]

    protestDictTemp <- append_list(protestDictTemp, protestDictAdd, i, j)
    
    labelDfm <- dfm_lookup(dfmTrain, dictionary(protestDictTemp), levels = 1)
    
    map <- textmodel_newsmap(dfmTrain, labelDfm)
    
    modPredict <- predict(map, newdata = dfmTest, confidence.fit = TRUE)
    
    modPredict <- as_tibble(modPredict)
    
    modPredict <- transform(modPredict, class_fixed = ifelse(confidence.fit < 0, 0, class))
    
    modPredict <- modPredict %>% mutate(class_fixed = as.factor(class_fixed))
    
    results <- caret::confusionMatrix(modPredict$class_fixed,
                                      sentenceAnnotation$type,
                                      mode = "prec_recall")
    
    cov <- get_coverage(labelDfm)
    
    resAfe <- newsmap::afe(dfmTrain, labelDfm, smooth = 1)
    
    s <- as.data.frame(results$byClass)
    
    overall <- as.data.frame(results$overall)
    
    t_overall <- t(overall)
    
    t_overall <- as.data.frame(t_overall)
    
    colnames(t_overall) <- rownames(overall)
    
    rownames(t_overall) <- colnames(overall)
    
    datTemp <- data.frame(
      i = i,
      j = j,
      noise = FALSE,
      word_added = wordAdded,
      coverage = cov,
      afe = resAfe,
      p_protest_0 = s$Precision[1],
      r_protest_0 = s$Recall[1],
      f1_protest_0 = s$F1[1],
      p_protest_1 = s$Precision[2],
      r_protest_1 = s$Precision[2],
      f1_protest_1 = s$F1[2],
      p_protest_2 = s$Precision[3],
      r_protest_2 = s$Recall[3],
      f1_protest_2 = s$F1[3],
      p_protest_3 = s$Precision[4],
      r_protest_3 = s$Recall[4],
      f1_protest_3 = s$F1[4],
      accuracy = t_overall$Accuracy,
      stringsAsFactors = FALSE
    )
    
    if(i == 1 & j==1){datFinal <- datTemp}
    
    datFinal <- rbind(datFinal, datTemp)
    
    cat(sprintf(
      "%d %d Word Added %s (Coverage %.3f, Accuracy %.3f)", i, j,
      datTemp$word_added, datTemp$coverage, datTemp$accuracy
    ), "\n")
  }
}

# Change in model for each individual word added

for (i in 1:3) {
  for (j in 1:length(protestDictAdd[[i]])) {
    
    row <- sum(row, 1)
    
    wordAdded = protestDictAdd[[i]][j]
    
    protestDictTemp <- append_list(protestDictInit, protestDictAdd, i, j)
    
    labelDfm <- dfm_lookup(dfmTrain, dictionary(protestDictTemp), levels = 1)
    
    map <- textmodel_newsmap(dfmTrain, labelDfm, smooth = 3)
    
    modPredict <- predict(map, newdata = dfmTest, confidence.fit = TRUE)
    
    modPredict <- as_tibble(modPredict)
    
    modPredict <- transform(modPredict, class_fixed = ifelse(confidence.fit < 0, 0, class))
    
    modPredict <- modPredict %>% mutate(class_fixed = as.factor(class_fixed))
    
    results <- caret::confusionMatrix(modPredict$class_fixed,
                                      sentenceAnnotation$type,
                                      mode = "prec_recall")
    
    cov <- get_coverage(labelDfm)
    
    resAfe <- newsmap::afe(dfmTrain, labelDfm, smooth = 1)
    
    s <- as.data.frame(results$byClass)
    
    overall <- as.data.frame(results$overall)
    
    t_overall <- t(overall)
    
    t_overall <- as.data.frame(t_overall)
    
    colnames(t_overall) <- rownames(overall)
    
    rownames(t_overall) <- colnames(overall)
    
    datTemp <- data.frame(
      i = i,
      j = j,
      noise = FALSE,
      word_added = wordAdded,
      coverage = cov,
      afe = resAfe,
      p_protest_0 = s$Precision[1],
      r_protest_0 = s$Recall[1],
      f1_protest_0 = s$F1[1],
      p_protest_1 = s$Precision[2],
      r_protest_1 = s$Precision[2],
      f1_protest_1 = s$F1[2],
      p_protest_2 = s$Precision[3],
      r_protest_2 = s$Recall[3],
      f1_protest_2 = s$F1[3],
      p_protest_3 = s$Precision[4],
      r_protest_3 = s$Recall[4],
      f1_protest_3 = s$F1[4],
      accuracy = t_overall$Accuracy,
      stringsAsFactors = FALSE
    )
    
    if(i == 1 & j==1){datFinal <- datTemp}
    
    datFinal <- rbind(datFinal, datTemp)
    
    cat(sprintf(
      "%d %d Word Added %s (Coverage %.3f, Accuracy %.3f)", i, j,
      datTemp$word_added, datTemp$coverage, datTemp$accuracy
    ), "\n")
  }
}

coverageInit <- datFinal$coverage[1]
accInit <- datFinal$accuracy[1]
afeInit <- datFinal$afe[1]
f1_0Init <- datFinal$f1_protest_0[1]
f1_1Init <- datFinal$f1_protest_1[1]
f1_2Init <- datFinal$f1_protest_2[1]
f1_3Init <- datFinal$f1_protest_3[1]

datFinal$entropy_introduced <- afeInit - datFinal$afe
datFinal$cov_diff <- datFinal$coverage - coverageInit
datFinal$acc_diff <- datFinal$accuracy - accInit
datFinal$f1_protest_0_diff <- f1_0Init - datFinal$f1_protest_0
datFinal$f1_protest_1_diff <- f1_1Init - datFinal$f1_protest_1
datFinal$f1_protest_2_diff <- f1_2Init - datFinal$f1_protest_2
datFinal$f1_protest_3_diff <- f1_3Init - datFinal$f1_protest_3



ggplot(datFinal, aes(x = cov_diff, y = entropy_introduced, label = word_added)) +
  geom_abline(slope = 0, intercept = 0) +
  geom_text(aes(size = acc_diff))

ggplot(datFinal[datFinal$i == 1,], aes(x = f1_protest_0_diff, y = entropy_introduced, label = word_added)) +
  geom_text(aes(size = cov_diff))

ggplot(datFinal[datFinal$i == 1,], aes(x = f1_protest_1_diff, y = entropy_introduced, label = word_added)) +
  geom_text(aes(size = cov_diff))

ggplot(datFinal[datFinal$i == 2,], aes(x = f1_protest_2_diff, y = entropy_introduced, label = word_added)) +
  geom_text(aes(size = cov_diff))

ggplot(datFinal[datFinal$i == 3,], aes(x = f1_protest_3_diff, y = entropy_introduced, label = word_added)) +
  geom_text(aes(size = cov_diff))


newDict <- dictionary(file = here("dictionaries", "post_processes_prot_dictionary.yaml"), format = "YAML")

# Model
trainLookup <- dfm_lookup(dfmTrain, dictionary = newDict)

model <- textmodel_newsmap(dfmTrain, trainLookup, smooth = 3)

modPredict <- predict(model, newdata = dfmTest, confidence.fit = TRUE)
 
modPredict <- as_tibble(modPredict)
 
modPredict <- transform(modPredict, class_fixed = ifelse(confidence.fit < 0, 0, class))
 
modPredict <- modPredict %>% mutate(class_fixed = as.factor(class))
 
results <- caret::confusionMatrix(modPredict$class_fixed, sentenceAnnotation$type)

classResults <- results[["byClass"]]
