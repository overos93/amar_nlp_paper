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

stopwords <- import(here("data-raw", "stopwords.rds"))

## Create and Clean Data ####

# source(here("code", "data_clean_afam_protest.R"))
# 
# source(here("code", "data_clean_afam_protest_dalit.R"))

## Load Cleaned Data
## dfmTrainArticles 1 is only US
dfmTestArticles1 <- import(here("data", "dfmTestArticles_t1.rds"))
## dfmTrainArticles 2 is US and India
dfmTrainArticles1 <- import(here("data", "dfmTrainArticles_t1.rds"))

dfmTestArticles2 <- import(here("data", "dfmTestArticles_t2.rds"))

dfmTrainArticles2 <- import(here("data", "dfmTrainArticles_t2.rds"))

articleAnnotation1 <- import(here("data", "articleAnnotation1.rds"))

articleAnnotation2 <- import(here("data", "articleAnnotation2.rds"))

source(here("code", "analysis_afam_protest.R"))

source(here("code", "analysis_afam_protest_discrim.R"))

source(here("code", "analysis_afam_protest_dalit.R"))

source(here("code", "analysis_afam_discrim_dalit.R"))

source(here("code", "ethnic_group_model.R"))

# Just dictionaries
dictionary_discrim <- dictionary(file = here("dictionaries", "post_process_single_discrim_dictionary.YAML"), format = "YAML")
dict_lookup <- dfm_lookup(dfmTrainArticles2, dictionary = dictionary_discrim)
t <- quanteda::convert(dict_lookup, to = "matrix")
y <- if_else(t[,1] > 0,1,0)
sum(y)/length(y)

prtdict2 <- dictionary(postprocDict[1])
dictionarylookup2 <- dfm_lookup(dfmTrainArticles1, dictionary = prtdict2)
t <- convert(dictionarylookup2, to = "matrix")
y <- if_else(t[,1] > 0, 1, 0)
sum(y)/length(y)

# Graphical Representations ####

## Binary Graph

# load the raw protest article data
usProtest <- import(here("data-raw", "us_protest_train.rds"))
indDalit <- import(here("data-raw", "dalits_india_withtext.Rdata"))
fullData <- rbind(usProtest, indDalit)

# Cleans the protest data up
fullData$body <- as.character(fullData$body)

protestCorp <- corpus(fullData, text_field = "body")

protestToks <- clean_corpus(protestCorp, stopwords)

protestDFM <- dfm(protestToks)

# Takes the protest data and predicts protest using the model from earlier
# model Article-level Binary First run
###### YOU need to run all of this on a different data set
###### dfmArticleTrain2 is the data that contains both India & US 
###### articleAnnotation2 is the test data - not sure if you need to know that but just in case
# replace modelAB1 with modelAB2 - was trained on both US and India data

# predict protest outcomes
pred <- predict(modelAB2, protestDFM, confidence.fit = TRUE)

# predict ethnic outcomes
pred_2 <- predict(modelethnic, protestDFM, confidence.fit = TRUE)

pred <- transform(pred, class_fixed = ifelse(confidence.fit < 0, 0, class))

pred_2 <- transform(pred_2, class_fixed = ifelse(confidence.fit < 0, 0, class))

pred <- transform(pred, class_fixed = ifelse(class_fixed == 2, 0, class_fixed))

pred_2 <- transform(pred_2, class_fixed = ifelse(class_fixed == 2, 0, class_fixed))

pred$rowname <- rownames(pred)

pred_2$rowname <- rownames(pred_2)

pred$text <- gsub("\\..*", "", pred$rowname)

pred_2$text <- gsub("\\..*", "", pred_2$rowname)

pred$sentence <- as.numeric(gsub(".*\\.", "", pred$rowname))

pred_2$sentence <- as.numeric(gsub(".*\\.", "", pred_2$rowname))

pred <- pred %>% dplyr::select(text, class_fixed, everything())

pred_2 <- pred_2 %>% dplyr::select(text, class, everything())

grp_preds <- pred %>% group_by(text) %>%  tally(class_fixed)

fullData$ethnic_group<- pred_2$class

fullData$protest_counts <- grp_preds$n

fullData$datefixed <- lubridate::as_datetime(fullData$published_date)

fullData$datefixed <- as.Date(fullData$datefixed)

plotData <- fullData %>% dplyr::select(datefixed, protest_counts, ethnic_group)

plotData <- arrange(plotData, datefixed)

plotData$ethnic_protest <- ifelse(plotData$protest_counts > 0 & plotData$ethnic_group == "african_american", 1,
                                  ifelse(plotData$protest_counts>0 & plotData$ethnic_group == "dalit", 2,
                                         0))


plotData <- plotData %>% group_by(datefixed, ethnic_group) %>% tally(protest_counts)

plotxts <- xts(plotData$n, plotData$datefixed)

george_floyd <- lubridate::ymd("2020-05-25")

teen_death <- lubridate::ymd("2020-09-27")

covid_extension <- lubridate::ymd("2020-04-15")

couple_suicide <- lubridate::ymd("2020-07-14")

pts <- as.data.frame(tsclean(plotxts))

cleanedTS <- as.data.frame(tsclean(ts(plotxts)))

cleanedTS$datefixed <- plotData$datefixed

pts$datefixed <- plotData$datefixed

plotData$ethnic_group <- factor(plotData$ethnic_group, levels = c("african_american",
                                                                  "dalit", "NA"),
                                labels = c("African American", "Dalit", "No Ethnic Group"))

# tikz(file = "ethnic_and_protest_results_plot.tex", width = 12, height = 14)

p2 <- ggplot(na.omit(plotData), aes(x = datefixed, y = n, color = "observed")) +
  geom_line() +
  geom_line(data = cleanedTS, aes(x = datefixed, y = `Series 1`, color = "estimated")) +
  labs(x = "Day", y = "Count of Texts Identified as Protest") +
  scale_color_manual(name = "",
                     breaks = c("observed", "estimated"),
                     values = c("observed" = "#66c2a5", "estimated" = "#fc8d62")) +
  theme(legend.position = "top") +
  annotate(geom = "vline",
           x = c(george_floyd),
           xintercept = c(george_floyd),
           linetype = c("dotted"),
           color = "#8da0cb") +
  annotate(geom = "text",
           label = 'scriptstyle("Murder of George Floyd")',
           x = george_floyd,
           y = 12,
           angle = 90,
           vjust = -1,
           hjust = -0.1,
           color = "black",
           parse = TRUE) +
  annotate(geom = "vline",
           x = c(covid_extension),
           xintercept = c(covid_extension),
           linetype = c("dotted"),
           color = "black") +
  annotate(geom = "text",
           label = 'scriptstyle("Covid Lockdowns Extended")',
           x = covid_extension,
           y = 12,
           angle = 90,
           vjust = -1,
           hjust = -0.1,
           color = "black",
           parse = TRUE) +
  annotate(geom="vline",
           x = c(teen_death),
           xintercept = c(teen_death),
           linetype = c("dotted"),
           color = "forestgreen") +
  annotate(geom = "text",
           label = 'scriptstyle("Murder and Rape of Dalit teen")',
           x=teen_death,
           y = 12,
           angle = 90,
           vjust = -1,
           hjust = -0.1,
           color = "forestgreen",
           parse = TRUE) +
  annotate(geom="vline",
           x = c(couple_suicide),
           xintercept = c(couple_suicide),
           linetype = c("dotted"),
           color = "forestgreen") +
  annotate(geom = "text",
           label = 'scriptstyle("Dalit Couple Suicide")',
           x=couple_suicide,
           y = 12,
           angle = 90,
           vjust = -1,
           hjust = -0.1,
           color = "forestgreen",
           parse = TRUE) +
  theme(text = element_text(size = 15)) +
  facet_wrap(~ ethnic_group, ncol = 1)

p2

print(p2)
dev.off()
p2
# Code that identifies how many articles mention George Floyd 
# t <- usProtest[usProtest$datefixed == "2020-05-31",]
# t <- corpus(t, text_field = "display_title")
# t <- tokens(t)
# t <- dfm(t)
# floyd <- dictionary(list(floyd = c("George", "Floyd")))
# tt <- dfm_lookup(t,floyd)
# tt <- as.data.frame(tt)
# tt <- sum(tt[tt$floyd > 1,])

## results graph v2 
## Results Graphs

mcResults <- data.frame(
  stringsAsFactors = TRUE,
  class = c("Protest", "Discrimination"),
  Precision = c(0.93, 0.35),
  Recall = c(0.89, 0.83),
  F1 = c(0.91, 0.50)
)

mcResults_edit <- reshape2::melt(mcResults) %>% rename("Metric" = "variable")

color_grid <- c("Recall" = "#8da0cb", "Precision" = "#66c2a5", "F1" = "#fc8d62")

shape_grid <- c("Recall" = 21, "Precision" = 22, "F1" = 23)
library(tikzDevice)

tikz(file = "binary_results_plot.tex", width = 5, height = 5)

plot <- ggplot(data = mcResults_edit, aes(y = Metric)) +
  geom_point(aes(x = value),
             size = 3) +
  geom_label(aes(x = value, label = value)) +
  facet_wrap(~class) +
  scale_color_manual(values = color_grid) +
  scale_fill_manual(values = color_grid) +
  scale_shape_manual(values = shape_grid) +
  xlab("Value") +
  ylab("Metric") +
  xlim(0,1)

print(plot)

dev.off()

# bnResults <- data.frame(
#   stringsAsFactors = TRUE,
#   Metric = c("Precision", "Recall", "F1"),
#   Value = c(0.93, 0.89, 0.91)
# )
# 
# ggplot(data = bnResults, aes(y = Metric)) +
#   geom_point(aes(x = Value, color = Metric, fill = Metric, shape = Metric), size = 5) +
#   scale_color_manual(values = color_grid) +
#   scale_fill_manual(values = color_grid) +
#   scale_shape_manual(values = shape_grid) +
#   xlim(0,1) +
#   xlab("Value") +
#   ylab("Accuracy Metrics") +
#   ggtitle("Accuracy Metric Results for Binary Classifier")
# 

mcResults <- data.frame(
  stringsAsFactors = TRUE,
  class = c("Protest", "Discrimination"),
  Precision = c(0.65, 0.63),
  Recall = c(0.85, 0.91),
  F1 = c(0.73, 0.75)
)

mcResults_edit <- reshape2::melt(mcResults) %>% rename("Metric" = "variable")

color_grid <- c("Recall" = "#8da0cb", "Precision" = "#66c2a5", "F1" = "#fc8d62")

shape_grid <- c("Recall" = 21, "Precision" = 22, "F1" = 23)

# tikz(file = "binary_results_plot.tex", width = 5, height = 5)

plot2 <- ggplot(data = mcResults_edit, aes(y = Metric)) +
  geom_point(aes(x = value),
             size = 3) +
  geom_label(aes(x = value, label = value)) +
  facet_wrap(~class) +
  scale_color_manual(values = color_grid) +
  scale_fill_manual(values = color_grid) +
  scale_shape_manual(values = shape_grid) +
  xlab("Value") +
  ylab("Metric") +
  xlim(0,1) +
  theme(text = element_text(size = 15))

print(plot2)

dev.off()

plot2
