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

#add song ids
data_english$song_id <- 1:nrow(data_english)

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
  dfm_trim(min_docfreq = 0.01, max_docfreq = 0.90, docfreq_type = "prop")

sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
sample_data <- sample_data[sel_idx, ]

#update genre indicies in order to avoid problems at visualization
pop_idx <- intersect(pop_idx, sample_data$song_id)
rap_idx <- intersect(rap_idx, sample_data$song_id)
reggae_idx <- intersect(reggae_idx, sample_data$song_id)
rnb_idx <- intersect(rnb_idx, sample_data$song_id)
blues_idx <- intersect(blues_idx, sample_data$song_id)
country_idx <- intersect(country_idx, sample_data$song_id)
folk_idx <- intersect(folk_idx, sample_data$song_id)
gospel_spiritual_idx <- intersect(gospel_spiritual_idx, sample_data$song_id)
rock_idx <- intersect(rock_idx, sample_data$song_id)
heavyMetal_idx <- intersect(heavyMetal_idx, sample_data$song_id)
hipHop_idx <- intersect(hipHop_idx, sample_data$song_id)
soul_idx <- intersect(soul_idx, sample_data$song_id)
jazz_idx <- intersect(jazz_idx, sample_data$song_id)
kids_idx <- intersect(kids_idx, sample_data$song_id)

