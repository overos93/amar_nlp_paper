# Master Script for AMAR Research Note
# Henry Overos
# May 26, 2022
# This is a final script that incorporates the main data processing, analysis,
# and graph generation for the 2022 Working Paper draft.

# Setting Up Working Environment ####

rm(list = ls())

options(scipen = 6, digits = 4)

## Load Packages ####

if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
}

pacman::p_load(rio, tidyverse, here, quanteda, newsmap, caret, forecast, xts)

## Load Functions ####

source(here("code", "functions.R"))

## Load Raw Data ####

# stopwords
stopwords <- import(here("data-raw", "stopwords.rds"))

# training sets
#the following sets are loaded: African Americans in US, Protest in the US, Dalits/Indian Protest

trainUsAfam <- import(here("data-raw","us_afam_train.rds"))

trainUsProtest <- import(here("data-raw","us_protest_train.rds"))

trainIndDalit <- import(here("data-raw","dalits_india_train.rds"))

# These are the csvs sent to the coding team
testUsAfam <- import(here("data-raw", "test_usafam.csv"))

testUsProtest <- import(here("data-raw", "test_usprotest.csv"))

testIndDalit <- import(here("data-raw", "test_dalits_india.csv"))

# These are the csvs sent back with what the annotators did
articleAnnotationAfam <- import(here("data-raw", "_AHL _United States _AFAM.xlsx"))

articleAnnotationProtest <- import(here("data-raw", "_AHL _RH _United States _Protest.xlsx"))

articleAnnotationDalit <- import(here("data-raw", "dalits_test_complete.rds"))

# Data Pre-Processing ####

## Cleaning Raw Data ####

### Cleaning Raw Article Annotation Data ####

#### Cleaning US Afam Annotation Data ####
# The text for each article was not sent back by the annotators. So the body of
# each article needs to be merged with their annotations for the analysis.

testUsAfam <- testUsAfam %>%
  mutate(ID = as.numeric(row_id))

articleAnnotationAfam <- articleAnnotationAfam %>%
  left_join(., testUsAfam, by = "ID")

articleAnnotationAfam <- articleAnnotationAfam %>%
  select(ID, Title, body, Protest = Protest.x, Protest_Type, Discrimination = Discrimination.x,
         Discrim_Type)

articleAnnotationAfam <- articleAnnotationAfam %>%
  mutate(Protest = as.factor(Protest)) %>%
  mutate(Protest_Type = as.factor(Protest_Type)) %>% 
  mutate(Discrimination = as.factor(Discrimination)) %>% 
  mutate(Discrim_Type = as.factor(Discrim_Type))

#### Cleaning US Protest Annotation Data ####
testUsProtest <- testUsProtest[-1, 1:8]

testUsProtest$V1 <- as.numeric(testUsProtest$V1)

colnames(testUsProtest) <- c("ID", "URL", "Title", "body", "Protest", "Protest_Type", "Discrimination", "Discrim_Type")

articleAnnotationProtest <- articleAnnotationProtest %>% left_join(., testUsProtest, by = "ID")

articleAnnotationProtest <- articleAnnotationProtest %>%
  select(ID, Title = Title.x, body, Protest = Protest.x, Protest_Type = Protest_Type.x,
         Discrimination = Discrimination.x,
         Discrim_Type = Discrim_Type.x)

articleAnnotationProtest <- articleAnnotationProtest %>%
  mutate(Protest = as.factor(Protest)) %>%
  mutate(Protest_Type = as.factor(Protest_Type)) %>% 
  mutate(Discrimination = as.factor(Discrimination)) %>% 
  mutate(Discrim_Type = as.factor(Discrim_Type))

#### Cleaning India Dalit Annotation Data ####
testIndDalit <- testIndDalit[-1, 1:4]

testIndDalit$V1 <- as.numeric(testIndDalit$V1)

colnames(testIndDalit) <- c("ID", "URL", "Title", "body")

articleAnnotationDalit <- articleAnnotationDalit %>% left_join(., testIndDalit, by = "ID")

articleAnnotationDalit <- articleAnnotationDalit %>%
  select(ID, Title = Title.x, body, Protest, Protest_Type,
         Discrimination = Discrim,
         Discrim_Type)

articleAnnotationDalit <- articleAnnotationDalit %>%
  mutate(Protest = as.factor(Protest)) %>%
  mutate(Protest_Type = as.factor(Protest_Type)) %>% 
  mutate(Discrimination = as.factor(Discrimination)) %>% 
  mutate(Discrim_Type = as.factor(Discrim_Type))

### Merging Raw Article Annotation Data ####

articleAnnotation <- rbind(articleAnnotationAfam, articleAnnotationProtest, articleAnnotationDalit)

articleAnnotation[is.na(articleAnnotation$Protest_Type), ]$Protest_Type <- "0"
articleAnnotation[is.na(articleAnnotation$Discrim_Type), ]$Discrim_Type <- "0"

table(articleAnnotation$Protest)
table(articleAnnotation$Discrimination)

### Merging Training Data ####

train <- rbind(trainUsAfam, trainUsProtest, trainIndDalit)

## Separating Data into Training and Testing Data ####

compounds <- c(
  "african american*", "black lives matter", "black american*", "black *man", "black *men",
  "verbal opposition", "opposition speech", "codemn speech*", "public letter*",
  "court action", "sign petition*", "destruct property", "property destruct*", "traffic block*"
) # creating set of multiword tokens

### Article-Level Data Separation ####

# Body of text was scraped which R stores as a list object instead of a character
# leading to errors unless it is converted. The following code converts the
# body of each article into a character format first. Then each corpus is converted
# into a document feature matrix which will be used for analysis.

#### Article Training Set ####

##### Cleaning
trainArticles <- train %>% dplyr::filter(train$title %nin% articleAnnotation$Title)

trainArticles$body <- as.character(trainArticles$body)

trainArticles$body[sapply(trainArticles$body, is.null)] <- NA

##### Converting into Corpus

corpusTrainArticles <- corpus(trainArticles, text_field = "body")

toksTrainArticles <- clean_corpus(corpusTrainArticles, stopwords)

toksTrainArticles <- tokens_compound(toksTrainArticles, pattern = compounds)

dfmTrainArticles <- dfm(toksTrainArticles)

#### Article Testing Set ####
# articleAnnotation is the testing set

articleAnnotation$body <- as.character(articleAnnotation$body)

articleAnnotation$body[sapply(articleAnnotation$body, is.null)] <- NA

corpusTestArticles <- corpus(articleAnnotation, text_field = "body")

toksTestArticles <- clean_corpus(corpusTestArticles, stopwords)

toksTestArticles <- tokens_compound(toksTestArticles, pattern = compounds)

dfmTestArticles <- dfm(toksTestArticles)

# Save all Data Sets

export(dfmTestArticles, here("data", "dfmTestArticles_t2.rds"))
export(dfmTrainArticles, here("data", "dfmTrainArticles_t2.rds"))

# Analysis ####

## Binary Model ####
postprocDict <- dictionary(file = here("dictionaries", "single_prot_dict.yaml"), format = "YAML")

trainLookup <- dfm_lookup(dfmTrainArticles, dictionary = postprocDict)

modelAB <- textmodel_newsmap(dfmTrainArticles, trainLookup)

modPredict2 <- predict(modelAB, newdata = dfmTestArticles, confidence = TRUE)

modPredict2 <- as_tibble(modPredict2)

modPredict2 <- transform(modPredict2, class_fixed = ifelse(confidence.fit < 0, 0, class))

modPredict2 <- transform(modPredict2, class_fixed = ifelse(class_fixed == 2, 0, class_fixed))

modPredict2 <- modPredict2 %>% mutate(class_fixed = as.factor(class_fixed))

idxna <- is.na(modPredict2[, 1])

prediction <- modPredict2[!idxna, 3]

prediction <- as.factor(prediction)

reference <- articleAnnotation[!idxna, 4]

results2 <- caret::confusionMatrix(prediction, reference, positive = "1")

classResults2 <- results2[["byClass"]]

results2

classResults2

## Multiclass Models ####
multiprotestDict <- dictionary(file = here("dictionaries", "3_level_dictionary.YAML"), format = "YAML")

### Single-Dictionary Model ####

#### Article Level ####
trainLookup <- dfm_lookup(dfmTrainArticles, dictionary = multiprotestDict)

modelAM1 <- textmodel_newsmap(dfmTrainArticles, trainLookup)

modelOutputAM1 <- modelAM1[["model"]]

export(modelAM1, here("data", "modelArticlesMulticlass1.rds"))

modelOutputAM1 <- t(modelOutputAM1)

modPredict4 <- predict(modelAM1, newdata = dfmTestArticles, confidence = TRUE)

modPredict4 <- as_tibble(modPredict4)

modPredict4 <- transform(modPredict4, class_fixed = ifelse(confidence.fit < 0, 0, class))

modPredict4 <- transform(modPredict4, class_fixed = ifelse(class_fixed == 3, 0, class_fixed))

modPredict4 <- modPredict4 %>% mutate(class_fixed = as.factor(class_fixed))

idxna <- is.na(modPredict4[, 1])

prediction <- modPredict4[!idxna, 3]

prediction <- as.factor(prediction)

articleAnnotation <- articleAnnotation %>%
  mutate(type_combined = ifelse(Protest_Type == 3,
                                2,
                                ifelse(Protest_Type == 2 | Protest_Type == 1,
                                       1,
                                       0
                                )
  ))

reference <- articleAnnotation[!idxna, 6]

reference <- as.factor(reference)

results4 <- caret::confusionMatrix(prediction, reference)

classResults4 <- results4[["byClass"]]
classResults4

results4

# Just dictionaries
multiprotestDict[[1]][1:2]
prtdict <- dictionary(multiprotestDict[[1]][1:2])
dictionarylookup <- dfm_lookup(dfmTrainArticles, dictionary = prtdict)
t <- convert(dictionarylookup, to = "matrix")
y <- if_else(t[,1] > 0 | t[,2] > 0, 1, 0)

sum(y)/length(y)

prtdict2 <- dictionary(postprocDict[1])
dictionarylookup2 <- dfm_lookup(dfmTrainArticles, dictionary = prtdict2)
t <- convert(dictionarylookup2, to = "matrix")
y <- if_else(t[,1] > 0, 1, 0)

sum(y)/length(y)


# Prediction ####

multiClassRes <- rbind(classResults2, classResults4)

multiClassRes
library(pROC)

proc <- roc(as.ordered(reference), as.ordered(prediction))
ggroc(proc)

# Graphical Representations ####

## Binary Graph

usProtest <- import(here("data-raw", "us_protest_train.rds"))

usProtest$body <- as.character(usProtest$body)

protestCorp <- corpus(usProtest, text_field = "body")

protestToks <- clean_corpus(protestCorp, stopwords)

protestDFM <- dfm(protestToks)

pred <- predict(modelAB, protestDFM, confidence.fit = TRUE)

pred <- transform(pred, class_fixed = ifelse(confidence.fit < 0, 0, class))

pred <- transform(pred, class_fixed = ifelse(class_fixed == 2, 0, class_fixed))

pred$rowname <- rownames(pred)

pred$text <- gsub("\\..*", "", pred$rowname)

pred$sentence <- as.numeric(gsub(".*\\.", "", pred$rowname))

pred <- pred %>% dplyr::select(text, class_fixed, everything())

grp_preds <- pred %>% group_by(text) %>%  tally(class_fixed)

usProtest$protest_counts <- grp_preds$n

usProtest$datefixed <- lubridate::as_datetime(usProtest$published_date)

usProtest$datefixed <- as.Date(usProtest$datefixed)

plotData <- usProtest %>% dplyr::select(datefixed, protest_counts)

plotData <- arrange(plotData, datefixed)

plotData <- plotData %>% group_by(datefixed) %>% tally(protest_counts)

plotxts <- xts(plotData$n, plotData$datefixed)

george_floyd <- lubridate::ymd("2020-05-25")

covid_extension <- lubridate::ymd("2020-04-15")

cleanedTS <- as.data.frame(tsclean(as.ts(plotxts)))

cleanedTS$datefixed <- plotData$datefixed

p1 <- ggplot(plotData, aes(x = datefixed, y = n, color = "observed")) +
  geom_line() +
  geom_line(data = cleanedTS, aes(x = datefixed, y = x, color = "estimated")) +
  labs(x = "Day", y = "Count of Texts Identified as Protest") +
  scale_color_manual(name = "",
                     breaks = c("observed", "estimated"),
                     values = c("observed" = "#66c2a5", "estimated" = "#fc8d62")) +
  theme(legend.position = c(.8,.5)) +
  annotate(geom = "vline",
           x = c(george_floyd),
           xintercept = c(george_floyd),
           linetype = c("dotted"),
           color = "#8da0cb") +
  annotate(geom = "text",
           label = "Murder of George Floyd",
           x = george_floyd,
           y = 12,
           angle = 90,
           vjust = -1,
           hjust = -0.1,
           color = "#8da0cb") +
  annotate(geom = "vline",
           x = c(covid_extension),
           xintercept = c(covid_extension),
           linetype = c("dotted"),
           color = "#8da0cb") +
  annotate(geom = "text",
           label = "Covid Lockdowns Extended",
           x = covid_extension,
           y = 12,
           angle = 90,
           vjust = -1,
           hjust = -0.1,
           color = "#8da0cb") +
  ggtitle("Counts of Texts Identified as Protest in US, 2020", subtitle = "Observed daily values and estimated smoothed values")

p1

## Multi-class prediction

pred2 <- predict(modelAM1, protestDFM, confidence = TRUE)


pred2 <- transform(pred2, class_fixed = ifelse(confidence.fit < 0, 0, class))

pred2 <- transform(pred2, class_fixed = ifelse(class_fixed == 3, 0, class_fixed))

pred2$rowname <- rownames(pred2)

pred2$text <- gsub("\\..*", "", pred2$rowname)

pred2 <- pred2 %>% dplyr::select(text, class_fixed, everything())

pred2 <- pred2 %>% mutate(vo_dummy = if_else(pred2$class_fixed == 1, 1, 0), md_dummy = if_else(pred2$class_fixed == 2, 1, 0))

grp_predsvo <- pred2 %>% group_by(text) %>%  tally(vo_dummy)

usProtest$protest_counts_vo <- grp_predsvo$n

plotData2 <- usProtest %>% dplyr::select(datefixed, protest_counts_vo)

plotData2 <- arrange(plotData2, datefixed)

plotData2 <- plotData2 %>% group_by(datefixed) %>% tally(protest_counts_vo)

plotxts2 <- xts(plotData2$n, plotData2$datefixed)

cleanedTS2 <- as.data.frame(tsclean(as.ts(plotxts2)))

cleanedTS2$datefixed <- plotData2$datefixed

p2 <- ggplot(plotData2, aes(x = datefixed, y = n, color = "observed")) +
  geom_line() +
  geom_line(data = cleanedTS2, aes(x = datefixed, y = x, color = "estimated")) +
  labs(x = "", y = "", title = "Verbal Opposition") +
  scale_color_manual(name = "",
                     breaks = c("observed", "estimated"),
                     values = c("observed" = "#66c2a5", "estimated" = "#fc8d62")) +
  theme(legend.position = c(.9,.8), text = element_text(size = 9))

p2

grp_predsmd <- pred2 %>% group_by(text) %>% tally(md_dummy)

usProtest$protest_counts_md <- grp_predsmd$n

plotData3 <- usProtest %>% dplyr::select(datefixed, protest_counts_md)

plotData3 <- arrange(plotData3, datefixed)

plotData3 <- plotData3 %>% group_by(datefixed) %>% tally(protest_counts_md)

plotxts3 <- xts(plotData3$n, plotData3$datefixed)

cleanedTS3 <- as.data.frame(tsclean(as.ts(plotxts3)))

cleanedTS3$datefixed <- plotData3$datefixed

p3 <- ggplot(plotData3, aes(x = datefixed, y = n, color = "observed")) +
  geom_line() +
  geom_line(data = cleanedTS3, aes(x = datefixed, y = x, color = "estimated")) +
  labs(x = "", y = "", title = "Mass Demonstrations") +
  scale_color_manual(name = "",
                     breaks = c("observed", "estimated"),
                     values = c("observed" = "#66c2a5", "estimated" = "#fc8d62")) +
  theme(legend.position = "none", text = element_text(size = 9))

p3

gridExtra::grid.arrange(p2, p3, top = "Counts of Texts Identified as Protest by Type in US, 2020\n Results from Multi-Class Model", left = "Count of Texts", bottom = "Day")

## Results Graphs

mcResults <- data.frame(
  stringsAsFactors = TRUE,
  class = c("Not Protest", "Verbal Opposition","Mass Protest"),
  Precision = c(0.66, 0.04, 0.86),
  Recall = c(0.67, 0.13, 0.63),
  F1 = c(0.67, 0.06, 0.73)
)

mcResults_edit <- reshape2::melt(mcResults) %>% rename("Metric" = "variable")

mcResults_edit$class <- factor(mcResults_edit$class, levels = c("Not Protest", "Verbal Opposition", "Mass Protest"))

color_grid <- c("Recall" = "#8da0cb", "Precision" = "#66c2a5", "F1" = "#fc8d62")

shape_grid <- c("Recall" = 21, "Precision" = 22, "F1" = 23)

ggplot(data = mcResults_edit, aes(y = Metric)) +
  geom_point(aes(x = value, color = Metric, fill = Metric, shape = Metric),
             size = 3) +
  facet_wrap(~class) +
  scale_color_manual(values = color_grid) +
  scale_fill_manual(values = color_grid) +
  scale_shape_manual(values = shape_grid) +
  xlab("Value") +
  ylab("Class") +
  xlim(0,1) +
  ggtitle("Accuracy Metric Results for Multi-Class Models",
          subtitle = "Faceted by model class")

bnResults <- data.frame(
  stringsAsFactors = TRUE,
  Metric = c("Precision", "Recall", "F1"),
  Value = c(0.93, 0.89, 0.91)
)

ggplot(data = bnResults, aes(y = Metric)) +
  geom_point(aes(x = Value, color = Metric, fill = Metric, shape = Metric), size = 5) +
  scale_color_manual(values = color_grid) +
  scale_fill_manual(values = color_grid) +
  scale_shape_manual(values = shape_grid) +
  xlim(0,1) +
  xlab("Value") +
  ylab("Accuracy Metrics") +
  ggtitle("Accuracy Metric Results for Binary Classifier")


