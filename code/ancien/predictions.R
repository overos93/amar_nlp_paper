## ---------------------------
##
## Script name: Prediction Graphs
##
## Author: Henry Overos 
##
## Date Created: 2022-04-12
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

pacman::p_load(here,rio,tidyverse,TTR, quanteda, newsmap, forecast)

stopwords <- readRDS(here("data-raw", "extra_stopwords.rds")) # load stopword list

stopwords$word <- stopwords$word %>% tolower() # convert stopwords to lowercase

source(here("code", "functions.r"))

pacman::p_load(rio, tidyverse, here)

usProtest <- import(here("data-raw", "us_protest_train.rds"))

model <- import(here("data", "binaryModelSentences.rds"))

usProtest$body <- as.character(usProtest$body)

protestCorp <- corpus(usProtest, text_field = "body")

protestCorp <- corpus_reshape(protestCorp, to = "sentences")

protestToks <- clean_corpus(protestCorp)

protestDFM <- dfm(protestToks)

pred <- predict(model, protestDFM, confidence.fit = TRUE)

pred <- transform(pred, class_fixed = ifelse(confidence.fit < 0, 0, class))

pred$rowname <- rownames(pred)

pred$text <- gsub("\\..*", "", pred$rowname)

pred$sentence <- as.numeric(gsub(".*\\.", "", pred$rowname))

pred <- pred %>% dplyr::select(text, sentence, class_fixed, everything())

grp_preds <- pred %>% group_by(text) %>%  tally(class_fixed)

usProtest$protest_counts <- grp_preds$n

usProtest$datefixed <- lubridate::as_datetime(usProtest$published_date)

usProtest$datefixed <- as.Date(usProtest$datefixed)

plotData <- usProtest %>% dplyr::select(datefixed, protest_counts)

plotData <- arrange(plotData, datefixed)

plotData <- plotData %>% group_by(datefixed) %>% tally(protest_counts)

plotData$MA7 <- SMA(plotData$n, n = 7)

plotData$MA30 <- SMA(plotData$n, n = 30)

p1 <- ggplot(plotData, aes(x = datefixed)) + theme_bw()
p1

p1 <- p1 + geom_line(aes(y = n), color = "#0027b4")

george_floyd <- lubridate::ymd("2020-05-25")
covid_extension <- lubridate::ymd("2020-04-15")

p1_full <- p1 +
  annotate(geom = "vline",
           x = c(george_floyd),
           xintercept = c(george_floyd),
           linetype = c("dotted"),
           color = "#0077ec") +
  annotate(geom = "text",
           label = "Murder of George Floyd",
           x = george_floyd,
           y = 1000,
           angle = 90,
           vjust = -1,
           hjust = -0.1,
           color = "#0077ec") +
  annotate(geom = "vline",
           x = c(covid_extension),
           xintercept = c(covid_extension),
           linetype = c("dotted"),
           color = "#0077ec") +
  annotate(geom = "text",
           label = "Covid Lockdowns Extended",
           x = covid_extension,
           y = 1000,
           angle = 90,
           vjust = -1,
           hjust = -0.1,
           color = "#0077ec") +
    ylab("Count of Protest Texts per Day") +
  xlab("Day") +
  theme(legend.position = "bottom")

p1_full

plot <- ggplot(plotData, aes(x = datefixed, y = n))+
  geom_line() +
  xlab("Date") +
  ylab("Count of Texts Identified as Protest") +
  theme_bw()

aggData <- plotData %>% mutate(month = month(datefixed)) %>%
  mutate(month2 = as.Date(paste0("2020-",month,"-01"), "%Y-%m-%d")) %>% 
  group_by(month2) %>% summarise(mean_n = mean(n))

fig_test <- ggplot(aggData, aes(x = month2, y = mean_n)) +
  geom_point() +
  geom_line() +
  ylim(0,2*mean(aggData$mean_n))

mean(aggData$mean_n)
sd(aggData$mean_n)

fig_test +
  geom_hline(yintercept = c(225.4,225.4+90,225.4-90))


#### COMBO MODEL

model <- import(here("data", "multimodel.rds"))

pred <- predict(model, protestDFM, confidence.fit = TRUE)

pred <- transform(pred, class_fixed = ifelse(confidence.fit < 0, 0, class))

pred$rowname <- rownames(pred)

pred$text <- gsub("\\..*", "", pred$rowname)

pred$sentence <- as.numeric(gsub(".*\\.", "", pred$rowname))

pred <- pred %>% dplyr::select(text, sentence, class_fixed, everything())

usProtest$datefixed <- lubridate::as_datetime(usProtest$published_date)

usProtest$datefixed <- as.Date(usProtest$datefixed)

usProtest$text <- unique(pred$text)

dates <- usProtest %>% dplyr::select(datefixed,text)

pred <- na.omit(pred)

grps <- pred %>% group_by(text) %>% count(class)

#pred.dummies <- as.data.frame(model.matrix( ~ class - 1, data = pred))

#pred <- cbind(pred, pred.dummies)


grps <- grps %>% left_join(.,dates, by = "text")

plotData <- arrange(grps, datefixed)

plotData <- na.omit(plotData)

p1 <- ggplot(plotData %>% filter(class == "general_news"), aes(x = datefixed, y = n))+
  geom_line() +
  xlab("") +
  ylab("Counts") +
  ggtitle("General News") +
  scale_y_continuous(limits = c(0,3000)) +
  theme_bw()

p2 <- ggplot(plotData %>% filter(class == "sports"), aes(x = datefixed, y = n))+
  geom_line() +
  xlab("") +
  ylab("") +
  ggtitle("Sports") +
  scale_y_continuous(limits = c(0,3000)) +
  theme_bw()

p3 <- ggplot(plotData %>% filter(class == "prot_1"), aes(x = datefixed, y = n))+
  geom_line() +
  xlab("Date") +
  ylab("Counts") +
  ggtitle("Verbal Opposition") +
  scale_y_continuous(limits = c(0,3000)) +
  theme_bw()

p4 <- ggplot(plotData %>% filter(class == "prot_2"), aes(x = datefixed, y = n))+
  geom_line() +
  xlab("Date") +
  ylab("") +
  ggtitle("Mass Protest") +
  scale_y_continuous(limits = c(0,3000)) +
  theme_bw()

gridExtra::grid.arrange(p1,p2,p3,p4, top = "Counts of Sentences Classified by Newsmap, 2020")

verboppdate <- plotData[plotData$class == "prot_1" & plotData$n == max(plotData[plotData$class == "prot_1",]$n), ]$datefixed

verbopp <- usProtest[usProtest$datefixed == verboppdate,]