options(stringsAsFactors = FALSE)
library(dplyr)
library(textcat)
library(stringr)
library(quanteda)

#read in CSV file with tagged corpus
data <- read.csv("tagged_corpus_preprocessed.csv", header = TRUE, sep = ";", encoding = "UTF-8")

data <- data %>% filter(!grepl('artist', artist))
data$X <- NULL
data$Unnamed..0 <- NULL

#deleting entries with "noTag" tag
data_without_noTag <- data %>% filter(!grepl('noTag', tag))
#deleting entries with "topTagsError" tag
data_without_noTagsError <- data_without_noTag %>% filter(!grepl('topTagsError', tag))
#deleting entries without lyrics 
data_only_lyrics <- data_without_noTagsError %>% filter(grepl(' ', lyrics))

#setting textcat profile to English to search for only English lyrics in corpus.
myProfiles <- TC_byte_profiles[names(TC_byte_profiles) %in% c("english", "spanish", "french", 'german', 'chinese')]

#append language column to data_only_lyrics data frame
data_only_lyrics$language <- textcat(data_only_lyrics[,5], p=myProfiles)

#create new data frame containing only entries with English lyrics
data_english <- data_only_lyrics %>% filter(grepl('english', language))

#create lyrics corpus
lyrics_corpus <- corpus(data_english$lyrics, docnames = data_english$song_id)

# Build a dictionary of lemmas
lemma_data <- read.csv("baseform_en.tsv", encoding = "UTF-8")

# stopword list
stopwords_extended <- readLines("stopwords_en.txt",
                                encoding = "UTF-8")

# Create a DTM 
corpus_tokens <- lyrics_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma,
                 valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = T)

lyrics_collocations <- quanteda.textstats::textstat_collocations(corpus_tokens,
                                                                 min_count = 25)
lyrics_collocations <- lyrics_collocations[1:100, ]

corpus_tokens <- tokens_compound(corpus_tokens, lyrics_collocations)

#create corpus/dtm with 100 songs of each genre
pop_idx <- sample(which(data_english$tag == 'pop'), 100, replace=FALSE)
rap_idx <- sample(which(data_english$tag == 'rap'), 100, replace=FALSE)
reggae_idx <- sample(which(data_english$tag == 'reggae'), 100, replace=FALSE)
rnb_idx <- sample(which(data_english$tag == 'rnb'), 100, replace=FALSE)
blues_idx <- sample(which(data_english$tag == 'blues'), 100, replace=FALSE)
country_idx <- sample(which(data_english$tag == 'country'), 100, replace=FALSE)
folk_idx <- sample(which(data_english$tag == 'folk'), 100, replace=FALSE)
gospel_spiritual_idx <- sample(which((data_english$tag == 'gospel') | (data_english$tag == 'spiritual')), 100, replace=FALSE)
rock_idx <- sample(which(data_english$tag == 'rock'), 100, replace=FALSE)
heavyMetal_idx <- sample(which(data_english$tag == 'heavy metal'), 100, replace=FALSE)
hipHop_idx <- sample(which(data_english$tag == 'hip hop'), 100, replace=FALSE)
soul_idx <- sample(which(data_english$tag == 'soul'), 100, replace=FALSE)
jazz_idx <- sample(which(data_english$tag == 'jazz'), 100, replace=FALSE)
kids_idx <- sample(which(data_english$tag == 'kids'), 100, replace=FALSE)

sample_idx <- c(pop_idx, rap_idx, reggae_idx, rnb_idx, blues_idx, country_idx, folk_idx, 
                gospel_spiritual_idx, rock_idx, heavyMetal_idx, hipHop_idx, soul_idx, jazz_idx, 
                kids_idx)

sample_corpus_tokens <- corpus_tokens[sample_idx] %>% tokens_remove("") 
sample_data <- data_english[sample_idx,]

DTM <- sample_corpus_tokens %>%
  dfm() %>%
  dfm_trim(min_docfreq = 0.01, max_docfreq = 0.99, docfreq_type = "prop")

sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
sample_data <- sample_data[sel_idx, ]

###################topic modeling#################################
require(topicmodels)

#best model so far

K <- 12

topicModel <- LDA(DTM, K, method = "Gibbs", control = list(iter = 500, alpha = 0.2,
                                                           seed = 1, verbose = 25))

tmResult <- posterior(topicModel)

beta <- tmResult$terms

theta <- tmResult$topics

rowSums(theta)[1:12]

terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse = " ")


####################visualization wordclouds#########################################
require(wordcloud2)

#interesting topics to visualize as wordclouds, can be more or different ones

topicToViz <- 3

topicToViz <- grep("shit", topicNames)[1]

top40terms <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:40]
words <- names(top40terms)

probabilities <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:40]

wordcloud2(data.frame(words, probabilities), shuffle = FALSE)

topicToViz2 <- 12

topicToViz2 <- grep("love", topicNames2)[1]

top40terms2 <- sort(tmResult$terms[topicToViz2, ], decreasing = TRUE)[1:40]
words2 <- names(top40terms2)

probabilities2 <- sort(tmResult$terms[topicToViz2, ], decreasing = TRUE)[1:40]

wordcloud2(data.frame(words2, probabilities2), shuffle = FALSE)

topicToViz3 <- 1

topicToViz3 <- grep("night", topicNames)[1]

top40terms3 <- sort(tmResult$terms[topicToViz3, ], decreasing = TRUE)[1:40]
words3 <- names(top40terms3)

probabilities3 <- sort(tmResult$terms[topicToViz3, ], decreasing = TRUE)[1:40]

wordcloud2(data.frame(words3, probabilities3), shuffle = FALSE)

########################### Topic proportions  ###################################################

#This part inspects the proportions of all topics in the sample corpus.
#First option gives the proportions of the topic in the sample corpus, second options calculates the number one topic in all documents,
# listing all topics and how many documents (songs), have that topic as the most likely topic



topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T),
                    2, paste, collapse = " ")

topicProportions <- colSums(theta)/nrow(DTM)
names(topicProportions) <- topicNames
sort(topicProportions, decreasing = TRUE)

countsOfPrimaryTopics <- rep(0, K)
names(countsOfPrimaryTopics) <- topicNames
for (i in 1:nrow(DTM)) {
  topicsPerDoc <- theta[i, ]
  primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1]
  countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] +
    1
}
sort(countsOfPrimaryTopics, decreasing = TRUE)

################################ visualization #############################################

#Here the proportions of the topics are visualized per genre, only pop genre so far

#creating empty vector for pop song ids
exampleIds <- c()

#getting the ids from pop_idx and concatenate them with 'text' in front of ids, step needed to get the right theta values from pop songs
for(i in pop_idx){
  str <- paste('text', as.character(i), sep= '')
  exampleIds <- c(exampleIds, str)
}
  
#creating new data frame with only pop song proportions of topics
topicProportionExamples <- theta[exampleIds,]
topicProportionExamples<- as.data.frame(topicProportionExamples)

#name vector to rename columns
columns <- c('Topic 1', 'Topic 2', 'Topic 3', 'Topic 4', 'Topic 5', 'Topic 6', 'Topic 7', 'Topic 8', 
             'Topic 9', 'Topic 10', 'Topic 11', 'Topic 12')

#initializing new data frame for calculated means of theta values for all topics over the whole pop song sample
topicProportionsPopGenre = data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(topicProportionsPopGenre) = columns

#renaming columns, due to type conflicts
names(topicProportionExamples)[names(topicProportionExamples)=='1'] <- 'Topic 1'
names(topicProportionExamples)[names(topicProportionExamples)=='2'] <- 'Topic 2'
names(topicProportionExamples)[names(topicProportionExamples)=='3'] <- 'Topic 3'
names(topicProportionExamples)[names(topicProportionExamples)=='4'] <- 'Topic 4'
names(topicProportionExamples)[names(topicProportionExamples)=='5'] <- 'Topic 5'
names(topicProportionExamples)[names(topicProportionExamples)=='6'] <- 'Topic 6'
names(topicProportionExamples)[names(topicProportionExamples)=='7'] <- 'Topic 7'
names(topicProportionExamples)[names(topicProportionExamples)=='8'] <- 'Topic 8'
names(topicProportionExamples)[names(topicProportionExamples)=='9'] <- 'Topic 9'
names(topicProportionExamples)[names(topicProportionExamples)=='10'] <- 'Topic 10'
names(topicProportionExamples)[names(topicProportionExamples)=='11'] <- 'Topic 11'
names(topicProportionExamples)[names(topicProportionExamples)=='12'] <- 'Topic 12'

#calculating means of theta values of each topic and store them in the correlating column
topicProportionsPopGenre$`Topic 1` <- mean(topicProportionExamples$`Topic 1`)
topicProportionsPopGenre$`Topic 2` <- mean(topicProportionExamples$`Topic 2`)
topicProportionsPopGenre$`Topic 3` <- mean(topicProportionExamples$`Topic 3`)
topicProportionsPopGenre$`Topic 4` <- mean(topicProportionExamples$`Topic 4`)
topicProportionsPopGenre$`Topic 5` <- mean(topicProportionExamples$`Topic 5`)
topicProportionsPopGenre$`Topic 6` <- mean(topicProportionExamples$`Topic 6`)
topicProportionsPopGenre$`Topic 7` <- mean(topicProportionExamples$`Topic 7`)
topicProportionsPopGenre$`Topic 8` <- mean(topicProportionExamples$`Topic 8`)
topicProportionsPopGenre$`Topic 9` <- mean(topicProportionExamples$`Topic 9`)
topicProportionsPopGenre$`Topic 10` <- mean(topicProportionExamples$`Topic 10`)
topicProportionsPopGenre$`Topic 11` <- mean(topicProportionExamples$`Topic 11`)
topicProportionsPopGenre$`Topic 12` <- mean(topicProportionExamples$`Topic 12`)

library("reshape2")
library("ggplot2")

#renaming the columns to the top5 words of the topic
colnames(topicProportionsPopGenre) <- topicNames  #-> können wir noch anders machen

#visualize data as horizontal bar plots
vizDataFrame <- melt(cbind(
  data.frame(topicProportionsPopGenre),
  pop = factor(1:1)),
  variable.name = "topic", id.vars = "pop")

ggplot(data = vizDataFrame,
       aes(topic, value, fill = pop), ylab = "proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ pop, ncol = 1)

