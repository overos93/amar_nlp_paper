# Cleaning Data for just AFAM + Protest Data Sets

## Load Raw Data ####

stopwords <- import(here("data-raw", "stopwords.rds"))

sentenceAnnotation <- import(here("data-raw", "umd_coded_resolved.xlsx"))

usAfamTrain <- readRDS(here("data-raw/us_afam_train.rds"))

usProtestTrain <- readRDS(here("data-raw/us_protest_train.rds"))

# These are the csvs sent to the coding team
usAfamTest <- import(here("data-raw", "test_usafam.csv"))

usProtestTest <- import(here("data-raw", "test_usprotest.csv"))

# These are the csvs sent back with what the annotators did
articleAnnotationAfam <- import(here("data-raw", "_AHL _United States _AFAM.xlsx"))

articleAnnotationProtest <- import(here("data-raw", "_AHL _RH _United States _Protest.xlsx"))

# Data Pre-Processing ####

## Cleaning Raw Data ####

### Cleaning Raw Article Annotation Data ####

#### Cleaning US Afam Annotation Data ####
# The text for each article was not sent back by the annotators. So the body of
# each article needs to be merged with their annotations for the analysis.

usAfamTest <- usAfamTest %>%
  mutate(ID = as.numeric(row_id))

articleAnnotationAfam <- articleAnnotationAfam %>%
  left_join(., usAfamTest, by = "ID")

articleAnnotationAfam <- articleAnnotationAfam %>%
  select(ID, Title, body, Protest = Protest.x, Protest_Type, Discrimination = Discrimination.x)

articleAnnotationAfam <- articleAnnotationAfam %>%
  mutate(Protest = as.factor(Protest)) %>%
  mutate(Protest_Type = as.factor(Protest_Type)) %>% 
  mutate(Discrimination = as.factor(Discrimination))

#### Cleaning US Protest Annotation Data ####
usProtestTest <- usProtestTest[-1, 1:6]

usProtestTest$V1 <- as.numeric(usProtestTest$V1)

colnames(usProtestTest) <- c("ID", "URL", "Title", "body", "discrimination", "protest")

articleAnnotationProtest <- articleAnnotationProtest %>% left_join(., usProtestTest, by = "ID")

articleAnnotationProtest <- articleAnnotationProtest %>%
  select(ID, Title = Title.x, body, Protest, Protest_Type, Discrimination)

articleAnnotationProtest <- articleAnnotationProtest %>%
  mutate(Protest = as.factor(Protest)) %>%
  mutate(Protest_Type = as.factor(Protest_Type)) %>% 
  mutate(Discrimination = as.factor(Discrimination))

### Merging Raw Article Annotation Data ####

articleAnnotation <- rbind(articleAnnotationAfam, articleAnnotationProtest)

articleAnnotation[is.na(articleAnnotation$Protest_Type), ]$Protest_Type <- "0"

### Cleaning Raw Sentence Annotation Data ####
sentenceAnnotation <- transform(sentenceAnnotation,
                                resolved_protest_fixed = ifelse(!is.na(resolved_protest),
                                                                resolved_protest,
                                                                protest_oja
                                )
)

sentenceAnnotation <- transform(sentenceAnnotation,
                                resolved_type_fixed = ifelse(!is.na(resolved_type),
                                                             resolved_type,
                                                             type_oja
                                )
)

sentenceAnnotation <- sentenceAnnotation %>%
  select(ID, Title, sentence, protest = resolved_protest_fixed, type = resolved_type_fixed) %>%
  mutate(protest = as.factor(protest)) %>%
  mutate(type = as.factor(type))

### Merging Training Data ####

train <- rbind(usProtestTrain, usAfamTrain)

## Separating Data into Training and Testing Data ####
`%!in%` <- function(x, y) {
  !(`%in%`(x, y))
}

compounds <- c(
  "african american*", "black lives matter", "black american*", "black *man", "black *men",
  "verbal opposition", "opposition speech", "codemn speech*", "public letter*",
  "court action", "sign petition*", "destruct property", "property destruct*", "traffic block*",
  "under represent*"
) # creating set of multiword tokens

### Article-Level Data Separation ####

# Body of text was scraped which R stores as a list object instead of a character
# leading to errors unless it is converted. The following code converts the
# body of each article into a character format first. Then each corpus is converted
# into a document feature matrix which will be used for analysis.

#### Article Training Set ####

##### Cleaning
trainArticles <- train %>% dplyr::filter(train$title %!in% articleAnnotation$Title)

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

### Sentence-Level Data Separation ####

#### Sentence Training Set ####
trainSentences <- train %>% filter(train$title %!in% unique(sentenceAnnotation$Title))

trainSentences$body <- as.character(trainSentences$body)

trainSentences$body[sapply(trainSentences$body, is.null)] <- NA

corpusTrainSentences <- corpus(trainSentences, text_field = "body")

corpusTrainSentences <- corpus_reshape(corpusTrainSentences, to = "sentences")

toksTrainSentences <- clean_corpus(corpusTrainSentences, stopwords)

toksTrainSentences <- tokens_compound(toksTrainSentences, pattern = compounds)

dfmTrainSentences <- dfm(toksTrainSentences)

#### Sentence Testing Set ####
testIdxSentences <- unique(sentenceAnnotation$ID)

testSentences <- usProtestTrain[testIdxSentences, ]

testSentences$body <- as.character(testSentences$body)

testSentences$body[sapply(testSentences$body, is.null)] <- NA

corpusTestSentences <- corpus(testSentences, text_field = "body")

corpusTestSentences <- corpus_reshape(corpusTestSentences, to = "sentences")

toksTestSentences <- clean_corpus(corpusTestSentences, stopwords)

toksTestSentences <- tokens_compound(toksTestSentences, pattern = compounds)

dfmTestSentences <- dfm(toksTestSentences)

# Save all Data Sets

export(dfmTestArticles, here("data", "dfmTestArticles_t1.rds"))
export(dfmTestSentences, here("data", "dfmTestSentences_t1.rds"))
export(dfmTrainArticles, here("data", "dfmTrainArticles_t1.rds"))
export(dfmTrainSentences, here("data", "dfmTrainSentences_t1.rds"))
export(articleAnnotation, here("data", "articleAnnotation1.rds"))
