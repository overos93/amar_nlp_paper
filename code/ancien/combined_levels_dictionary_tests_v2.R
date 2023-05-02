## ---------------------------
##
## Script name: multilevel dictionary tests
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

options(scipen = 6, digits = 4)
memory.limit(30000000)

## ---------------------------

## load up the packages we will need:  (uncomment as required)

if("pacman" %in% rownames(installed.packages()) == FALSE){
  install.packages("pacman")
}
source(here("code", "functions.R"))

pacman::p_load(rio, tidyverse, here, quanteda, newsmap, caret)

discrimDict <- dictionary(file = here("dictionaries", "post_process_single_discrim_dictionary.YAML"), format = "YAML")
dfmTrain <- import(here("data", "dfmTrainArticles_t1.rds"))
dfmTest <- import(here("data", "dfmTestArticles_t1.rds"))
articleAnnotation2 <- import(here("data", "articleAnnotation1.rds"))

## Get first words from dictionary
discrimDictInit <- get_initial(discrimDict)

## Create a list to add more words
discrimDictAdd <- flatten_dictionary(discrimDict, levels = 1)

# Generate Results for each word
dat_final <- data.frame(stringsAsFactors = FALSE)

row <- 0
cov <- 0
afe <- 0

discrimDictTemp <- discrimDictInit

# Change in model for each individual word added

for (i in 1:2) {
  for (j in 1:length(discrimDictAdd[[i]])) {
    
    row <- sum(row, 1)
    
    wordAdded = discrimDictAdd[[i]][j]
    
    discrimDictTemp <- append_list(discrimDictInit, discrimDictAdd, i, j)
    
    labelDfm <- dfm_lookup(dfmTrain, dictionary(discrimDictTemp), levels = 1)
    
    map <- textmodel_newsmap(dfmTrain, labelDfm)
    
    modPredict <- predict(map, newdata = dfmTest, confidence = TRUE)
    
    modPredict <- as_tibble(modPredict)
    
    modPredict <- transform(modPredict, class_fixed = ifelse(confidence.fit < 0, 0, class))
    
    modPredict <- modPredict %>% mutate(class_fixed = as.factor(class_fixed))
    
    results <- caret::confusionMatrix(modPredict$class_fixed,
                                      articleAnnotation2$Discrimination,
                                      mode = "prec_recall")
    
    cov <- get_coverage(labelDfm)
    
    resAfe <- newsmap::afe(dfmTrain, labelDfm, smooth = 1)
    
    s <- as.data.frame(results$byClass)
    
    t_s <- t(s)
    t_s <- as.data.frame(t_s)
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
      p_discrim = t_s$Precision,
      r_discrim = t_s$Recall,
      f1_discrim = t_s$F1,
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
f1_0Init <- datFinal$f1_discrim[1]

datFinal$entropy_introduced <- afeInit - datFinal$afe
datFinal$cov_diff <- datFinal$coverage - coverageInit
datFinal$acc_diff <- datFinal$accuracy - accInit
datFinal$f1_discri<- f1_0Init - datFinal$f1_discrim

ggplot(datFinal, aes(x = cov_diff, y = entropy_introduced, label = word_added)) +
  geom_abline(slope = 0, intercept = 0) +
  geom_text(aes(size = acc_diff),position = "jitter")

ggplot(datFinal[datFinal$i == 1,],
       aes(x = f1_discri,
           y = entropy_introduced,
           label = word_added)
) +
  geom_text(aes(size = cov_diff))

ggplot(datFinal[datFinal$i == 2,], aes(x = f1_discrim_2_diff, y = entropy_introduced, label = word_added)) +
  geom_text(aes(size = cov_diff))

discrimDictTemp <- discrimDictInit
for (i in 1:2) {
  for (j in 1:length(discrimDictAdd[[i]])) {
    
    row <- sum(row, 1)
    
    wordAdded = discrimDictAdd[[i]][j]
    
    discrimDictTemp <- append_list(discrimDictTemp, discrimDictAdd, i, j)
    
    labelDfm <- dfm_lookup(dfmTrain, dictionary(discrimDictTemp), levels = 1)
    
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
      p_discrim_0 = s$Precision[1],
      r_discrim_0 = s$Recall[1],
      f1_discrim_0 = s$F1[1],
      p_discrim_1 = s$Precision[2],
      r_discrim_1 = s$Precision[2],
      f1_discrim_1 = s$F1[2],
      p_discrim_2 = s$Precision[3],
      r_discrim_2 = s$Recall[3],
      f1_discrim_2 = s$F1[3],
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


## Get first words from dictionary
discrimDictInit <- get_initial(discrimDict)

## Create a list to add more words
discrimDictAdd <- flatten_dictionary(discrimDict, levels = 1)

# Generate Results for each word
dat_final <- data.frame(stringsAsFactors = FALSE)

row <- 0
cov <- 0
afe <- 0

discrimDictTemp <- discrimDictInit

# Change in model for each individual word added

for (i in 1:2) {
  for (j in 1:length(discrimDictAdd[[i]])) {
    
    row <- sum(row, 1)
    
    wordAdded = discrimDictAdd[[i]][j]
    
    discrimDictTemp <- append_list(discrimDictInit, discrimDictAdd, i, j)
    
    labelDfm <- dfm_lookup(dfmTrain, dictionary(discrimDictTemp), levels = 1)
    
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
      p_discrim_0 = s$Precision[1],
      r_discrim_0 = s$Recall[1],
      f1_discrim_0 = s$F1[1],
      p_discrim_1 = s$Precision[2],
      r_discrim_1 = s$Precision[2],
      f1_discrim_1 = s$F1[2],
      p_discrim_2 = s$Precision[3],
      r_discrim_2 = s$Recall[3],
      f1_discrim_2 = s$F1[3],
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
f1_0Init <- datFinal$f1_discrim_0[1]
f1_1Init <- datFinal$f1_discrim_1[1]
f1_2Init <- datFinal$f1_discrim_2[1]

datFinal$entropy_introduced <- afeInit - datFinal$afe
datFinal$cov_diff <- datFinal$coverage - coverageInit
datFinal$acc_diff <- datFinal$accuracy - accInit
datFinal$f1_discrim_0_diff <- f1_0Init - datFinal$f1_discrim_0
datFinal$f1_discrim_1_diff <- f1_1Init - datFinal$f1_discrim_1
datFinal$f1_discrim_2_diff <- f1_2Init - datFinal$f1_discrim_2

ggplot(datFinal, aes(x = cov_diff, y = entropy_introduced, label = word_added)) +
  geom_abline(slope = 0, intercept = 0) +
  geom_text(aes(size = acc_diff))

ggplot(datFinal[datFinal$i == 1,],
       aes(x = f1_discrim_1_diff,
           y = entropy_introduced,
           label = word_added)
) +
  geom_text(aes(size = cov_diff))

ggplot(datFinal[datFinal$i == 2,], aes(x = f1_discrim_2_diff, y = entropy_introduced, label = word_added)) +
  geom_text(aes(size = cov_diff))

discrimDictTemp <- discrimDictInit
for (i in 1:2) {
  for (j in 1:length(discrimDictAdd[[i]])) {
    
    row <- sum(row, 1)
    
    wordAdded = discrimDictAdd[[i]][j]
    
    discrimDictTemp <- append_list(discrimDictTemp, discrimDictAdd, i, j)
    
    labelDfm <- dfm_lookup(dfmTrain, dictionary(discrimDictTemp), levels = 1)
    
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
      p_discrim_0 = s$Precision[1],
      r_discrim_0 = s$Recall[1],
      f1_discrim_0 = s$F1[1],
      p_discrim_1 = s$Precision[2],
      r_discrim_1 = s$Precision[2],
      f1_discrim_1 = s$F1[2],
      p_discrim_2 = s$Precision[3],
      r_discrim_2 = s$Recall[3],
      f1_discrim_2 = s$F1[3],
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
