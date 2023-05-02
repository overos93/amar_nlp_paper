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
pred <- predict(modelAB2, protestDFM, confidence.fit = TRUE)

pred <- transform(pred, class_fixed = ifelse(confidence.fit < 0, 0, class))

pred <- transform(pred, class_fixed = ifelse(class_fixed == 2, 0, class_fixed))

pred$rowname <- rownames(pred)

pred$text <- gsub("\\..*", "", pred$rowname)

pred$sentence <- as.numeric(gsub(".*\\.", "", pred$rowname))

pred <- pred %>% dplyr::select(text, class_fixed, everything())

grp_preds <- pred %>% group_by(text) %>%  tally(class_fixed)

fullData$protest_counts <- grp_preds$n

fullData$datefixed <- lubridate::as_datetime(fullData$published_date)

fullData$datefixed <- as.Date(fullData$datefixed)

plotData <- fullData %>% dplyr::select(datefixed, protest_counts)

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
  theme(text = element_text(size = 15))

p1
fullData$ethnic_group

# Code that identifies how many articles mention George Floyd 
t <- fullData[fullData$datefixed == "2020-05-31" & fullData$ethnic_group == "african_american",]
t <- corpus(t, text_field = "display_title")
t <- tokens(t)
t <- dfm(t)
# 
tt <- dfm_lookup(t, dictionary = dictionary(list(floyd = c("floyd", "george"))))
# 
tt <- ifelse(tt[,1] > 0, 1, 0)

sum(tt)
tt

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
  class     = c("Protest", "Discrimination"),
  Precision = c(0.65, 0.64),
  Recall    = c(0.85, 0.91),
  F1        = c(0.73, 0.75)
)

mcResults_edit <- reshape2::melt(mcResults) %>% rename("Metric" = "variable")

color_grid <- c("Recall" = "#8da0cb", "Precision" = "#66c2a5", "F1" = "#fc8d62")

shape_grid <- c("Recall" = 21, "Precision" = 22, "F1" = 23)

tikz(file = "binary_results_plot.tex", width = 5, height = 5)

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

ethnicModelResults <- data.frame(
  stringsAsFactors = TRUE,
  class = c("African American", "Dalit"),
  Precision = c(0.815, 0.916),
  Recall = c(0.809, 0.970),
  F1 = c(0.812, 0.942)
)

mcResults_edit <- reshape2::melt(mcResults) %>% rename("Metric" = "variable")

color_grid <- c("Recall" = "#8da0cb", "Precision" = "#66c2a5", "F1" = "#fc8d62")

shape_grid <- c("Recall" = 21, "Precision" = 22, "F1" = 23)

tikz(file = "binary_results_plot.tex", width = 5, height = 5)

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
