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

stopwords <- readRDS(here("data-raw", "extra_stopwords.rds")) # load stopword list

stopwords$word <- stopwords$word %>% tolower() # convert stopwords to lowercase

# Load Data

sentenceAnnotation <- import(here("data", "sentenceAnnotation.rds"))

articleAnnotation <- import(here("data", "articleAnnotation.rds"))

dfmTestArticles <- import(here("data", "dfmTestArticles.rds"))

dfmTrainArticles <- import(here("data", "dfmTrain.rds"))

dfmTestSent <- import(here("data", "dfmTestSentences.rds"))

dfmTrainSent <- import(here("data", "dfmTrainSentences.rds"))

usProtest <- import(here("data-raw", "us_protest_train.rds"))

# Final Model Sentences
## Model 1 is Binary Protest/Not Protest

postprocDict <- dictionary(file = here("dictionaries", "post_process_single_prot_dictionary.YAML"), format = "YAML")

multiprotestDict <- dictionary(file = here("dictionaries", "3_level_dictionary.YAML"), format = "YAML")

trainLookup <- dfm_lookup(dfmTrainSent, dictionary = postprocDict)

model <- textmodel_newsmap(dfmTrainSent, trainLookup, smooth = 1)

usProtest$body <- as.character(usProtest$body)

protestCorp <- corpus(usProtest, text_field = "body")

protestCorp <- corpus_reshape(protestCorp, to = "sentences")

protestToks <- clean_corpus(protestCorp)

protestDFM <- dfm(protestToks)

pred <- predict(model, protestDFM, confidence.fit = TRUE)

pred <- transform(pred, class_fixed = ifelse(confidence.fit < 0, 0, class))

pred$rowname <- rownames(pred)

docvars(protestDFM, "class_fixed") <- pred$class_fixed

## For model 2, keep only those identified in round 1 as protest, then run model on subset
keep <- dfm_subset(protestDFM, class_fixed == 1)

train_lookup_2 <- dfm_lookup(keep, dictionary = multiprotestDict)

model2 <- textmodel_newsmap(keep, train_lookup_2, smooth = 0)

pred2 <- predict(model2, keep)

pred2 <- as.data.frame(pred2)

pred2$rowname <- rownames(pred2)

#merge model 1 and model 2 results
all_preds <- left_join(pred,pred2, by = "rowname")

all_preds <- all_preds %>% mutate(prot_1 = if_else(is.na(pred2), 0,
                                                       if_else(pred2 == "Protest.prot_1", 1, 0)),
                                  prot_2 = if_else(is.na(pred2), 0,
                                                   if_else(pred2 == "Protest.prot_2", 1, 0)))

# Vizualise results
all_preds$text <- gsub("\\..*", "", pred$rowname)

all_preds$sentence <- as.numeric(gsub(".*\\.", "", pred$rowname))

all_preds <- all_preds %>% dplyr::select(text, sentence, prot_1, prot_2, everything())

grp_preds <- all_preds %>% group_by(text) %>%  summarise(n_prot_1 = sum(prot_1),
                                                         n_prot_2 = sum(prot_2),
                                                         n_prot = sum(class_fixed, na.rm = TRUE))
usProtest$prot_1 <- grp_preds$n_prot_1

usProtest$prot_2 <- grp_preds$n_prot_2

usProtest$prot <- grp_preds$n_prot

usProtest$datefixed <- lubridate::as_datetime(usProtest$published_date)

usProtest$datefixed <- as.Date(usProtest$datefixed)

plotData <- usProtest %>% dplyr::select(datefixed, prot, prot_1, prot_2)

plotData <- arrange(plotData, datefixed)

plotData <- plotData %>% group_by(datefixed) %>% summarise(n_prot_1 = sum(prot_1),
                                                           n_prot_2 = sum(prot_2),
                                                           n_prot = sum(prot)) %>% 
  mutate(relfreqp1 = (n_prot_1/sum(n_prot)),
         relfreqp2 = (n_prot_2/sum(n_prot)),
         relfreqp = (n_prot/sum(n_prot)))

p <- ggplot(plotData, aes(x = datefixed)) + ylim(c(0,3000))

p1 <- p + geom_line(aes(y = n_prot_1)) +
  xlab("") +
  ylab("Count") +
  ggtitle("Verbal Opposition") +
  theme_bw()
  
p2 <- p +geom_line(aes(y = n_prot_2)) +
  xlab("Day") +
  ylab("Count") +
  ggtitle("Mass Protest") +
  theme_bw()

p3 <- p + geom_line(aes(y = n_prot)) +
  xlab("") +
  ylab("Count") +
  ggtitle("All Protest") +
  theme_bw()

gridExtra::grid.arrange(p3,p1,p2, top = "Hierarchical Newsmap Results for Protest in US News, 2020")
george_floyd <- lubridate::ymd("2020-05-25")
covid_extension <- lubridate::ymd("2020-04-15")

# p1_full <- p1 +
#   geom_line(aes(y = MA7,color = "7 Day Rolling AVG"), size = 1) +
#   annotate(geom = "vline",
#            x = c(george_floyd),
#            xintercept = c(george_floyd),
#            linetype = c("dashed"),
#            color = "blue") +
#   annotate(geom = "text",
#            label = "Murder of George Floyd",
#            x = george_floyd,
#            y = 1000,
#            angle = 90,
#            vjust = -1,
#            hjust = -0.1,
#            color = "blue") +
#   annotate(geom = "vline",
#            x = c(covid_extension),
#            xintercept = c(covid_extension),
#            linetype = c("dashed"),
#            color = "red") +
#   annotate(geom = "text",
#            label = "Covid Lockdowns Extended",
#            x = covid_extension,
#            y = 1000,
#            angle = 90,
#            vjust = -1,
#            hjust = -0.1,
#            size = 3,
#            color = "red") +
#   ylab("Count of Protest Texts per Day") +
#   scale_colour_manual("", 
#                       breaks = c("7 Day Rolling AVG", "Count per Day"),
#                       values = c("gray", "black")) +
#   xlab("Day") +
#   theme_bw()
# 
# 
# 
# library(lubridate)

plot <- ggplot(plotData, aes(x = datefixed, y = relfreqp))+
  geom_line(color = "red") +
  ylab("Frequency") +
  xlab("") +
  ggtitle("Frequency of Texts Identified as Protest") +
  theme_bw()

plot2 <- ggplot(plotData, aes(x = datefixed, y = relfreqp1))+
  geom_line() +
  ylab("Frequency") +
  xlab("") +
  ggtitle("Frequency of Texts Identified as Verbal Opposition") +
  theme_bw()

plot3 <- ggplot(plotData, aes(x = datefixed, y = relfreqp2))+
  geom_line() +
  xlab("Date") +
  ggtitle("Frequency of Texts Identified as Mass Protest") +
  theme_bw()

gridExtra::grid.arrange(plot,plot2,plot3)

plota <- ggplot(plotData, aes(x = datefixed, y = n_prot))+
  geom_line(color = "red") +
  ylab("Count") +
  xlab("") +
  ggtitle("Sentence Count Identified as Protest") +
  theme_bw()

plot2a <- ggplot(plotData, aes(x = datefixed, y = n_prot_1))+
  geom_line() +
  ylab("Count") +
  xlab("") +
  ggtitle("Sentence Count Identified as Verbal Opposition") +
  theme_bw()

plot3a <- ggplot(plotData, aes(x = datefixed, y = n_prot_2))+
  geom_line() +
  xlab("Date") +
  ggtitle("Sentence Count Identified as Mass Protest") +
  theme_bw()

gridExtra::grid.arrange(plota,plot2a,plot3a)

# Running on test set

modPredict1 <- predict(model, newdata = dfmTestSent, confidence.fit = TRUE)

modPredict1 <- as_tibble(modPredict1)

modPredict1 <- transform(modPredict1, class_fixed = ifelse(confidence.fit < 0, 0, class))

modPredict1 <- modPredict1 %>% mutate(class_fixed = as.factor(class_fixed),
                                      rowname = docnames(dfmTestSent))

docvars(dfmTestSent, field = "Protest") <- modPredict1$class_fixed

keepDocs <- dfm_subset(dfmTestSent, Protest == 1)

modPredict2 <- predict(model2, keepDocs)

modPredict2 <- as.data.frame(modPredict2)

modPredict2$rowname <- rownames(modPredict2)

all_preds <- left_join(modPredict1,modPredict2, by = "rowname")

all_preds <- all_preds %>% mutate(prot_1 = if_else(is.na(modPredict2), 0,
                                                   if_else(modPredict2 == "Protest.prot_1", 1, 0)),
                                  prot_2 = if_else(is.na(modPredict2), 0,
                                                   if_else(modPredict2 == "Protest.prot_2", 1, 0))) %>% 
  mutate(type_combined = if_else(prot_1 == 1, 1,
                                 if_else(prot_2 == 1, 2,
                                         0)))


sentenceAnnotation$type_combined <- as.factor(sentenceAnnotation$type)

sentenceAnnotation <- sentenceAnnotation %>%
  mutate(type_combined = ifelse(type == 1 | type == 2,
                                1,
                                ifelse(type == 3,
                                       2,
                                       0
                                )
  ))


tt <- caret::confusionMatrix(as.factor(all_preds$type_combined),
                                 as.factor(sentenceAnnotation$type_combined))

tt$byClass

# Run simple multi-class model 

trainLookup3 <- dfm_lookup(dfmTrain, dictionary = multiprotestDict)

model <- textmodel_newsmap(dfmTrain, trainLookup3, smooth = 3)

modelOutput <- model[["model"]]

modelOutput <- t(modelOutput)

modPredict <- predict(model, newdata = dfmTest, confidence.fit = TRUE)

modPredict <- as_tibble(modPredict)

modPredict <- transform(modPredict, class_fixed = ifelse(confidence.fit < 0, 0, class))

modPredict <- modPredict %>% mutate(class_fixed = as.factor(class_fixed))

results <- caret::confusionMatrix(modPredict$class_fixed, as.factor(sentenceAnnotation$type_combined))

classResults <- results[["byClass"]]
classResults

results

# Articles Level

