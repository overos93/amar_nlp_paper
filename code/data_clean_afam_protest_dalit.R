# Cleaning Data for AFAM + Protest + Dalit Data Sets

# stopwords
stopwords <- import(here("data-raw", "stopwords.rds"))

# training sets
#the following sets are loaded: African Americans in US, Protest in the US, Dalits/Indian Protest, White Protest

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
  select(ID, Title, body, Group = Relevant, Protest = Protest.x, Protest_Type, Discrimination = Discrimination.x,
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
  select(ID, Title = Title.x, body, Group, Protest = Protest.x, Protest_Type = Protest_Type.x,
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
  select(ID, Title = Title.x, body, Group, Protest, Protest_Type,
         Discrimination = Discrim,
         Discrim_Type)

articleAnnotationDalit <- articleAnnotationDalit %>%
  mutate(Protest = as.factor(Protest)) %>%
  mutate(Protest_Type = as.factor(Protest_Type)) %>% 
  mutate(Discrimination = as.factor(Discrimination)) %>% 
  mutate(Discrim_Type = as.factor(Discrim_Type)) %>% 
  mutate(Group = if_else(Group == 1, 2, 0))

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
export(articleAnnotation, here("data", "articleAnnotation2.rds"))