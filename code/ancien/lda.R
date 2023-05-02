library(cluster)
library(quanteda)
library(topicmodels)
library(tidyverse)
library(rio)
library(tidytext)
library(here)

dfmTestSent <- import(here("data", "dfmTestsent.rds"))

dfmTrainSent <- import(here("data", "dfmTrainsent.rds"))

dfmTestSentTrim <- dfm_trim(dfmTestSent,
                            min_docfreq = 0.01,
                            max_docfreq = 0.9,
                            docfreq_type = "prop")

dfmTrainSentTrim <-  dfm_trim(dfmTrainSent,
                              min_docfreq = 0.01,
                              max_docfreq = 0.9,
                              docfreq_type = "prop")

rm(dfmTrainSent,dfmTestSent)

topicCount <- 15

dfm2TM <- quanteda::convert(dfmTrainSentTrim, to = "topicmodels")

ldaModel <- LDA(dfm2TM, topicCount)

topics <- tidy(ldaModel, matrix = "beta")

top_terms <- topics %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

ldaSimilarity <- as.data.frame(ldaModel@beta) %>% 
  scale() %>% 
  dist(method = "euclidian") %>% 
  hclust(method = "ward.D2")

par(mar = c(5,4,2))

plot(ldaSimilarity,
    main = "LDA Topic Similarity by Features", 
    xlab = "",
    sub = "")

silhouette(ldaSimilarity)


