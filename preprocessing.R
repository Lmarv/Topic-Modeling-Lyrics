options(stringsAsFactors = FALSE)
library(dplyr)
library(textcat)
library(stringr)

#read in CSV file with tagged corpus
data <- read.csv("tagged_corpus_preprocessed.csv", header = TRUE, sep = ";", encoding = "UTF-8")

data <- data %>% filter(!grepl('artist', artist))
data$X <- NULL
data$Unnamed..0 <- NULL

#deleting entries with "noTag" tag
data1 <- data %>% filter(!grepl('noTag', tag))
#deleting entries with "topTagsError" tag
data2 <- data1 %>% filter(!grepl('topTagsError', tag))
#deleting entries without lyrics 
data_only_lyrics <- data2 %>% filter(grepl(' ', lyrics))

#setting textcat profile to English to search for only English lyrics in corpus.
myProfiles <- TC_byte_profiles[names(TC_byte_profiles) %in% c("english", "spanish", "french", 'german', 'chinese')]

#create lyrics vector to loop through faster
lyrics <- data_only_lyrics[,5]

#create new vector to store lyric language
lyricsLang <- c()

#loop through lyrics vector and lyricsLang vector to save language information
for(i in lyrics){
  lyricsLang <- c(lyricsLang, textcat(i, p=myProfiles))
}

#append lyricsLang vector as language column to data_only_lyrics data frame
data_only_lyrics$language <- lyricsLang

#create new data frame containing only entries with English lyrics
data_english <- data_only_lyrics %>% filter(grepl('english', language))

#creating data frames for every genre
data_pop <- data_english %>% filter(grepl('pop', tag))

data_rap <- data_english %>% filter(grepl('rap', tag))

data_reggae <- data_english %>% filter(grepl('reggae', tag))

data_rnb <- data_english %>% filter(grepl('rnb', tag))

data_blues <- data_english %>% filter(grepl('blues', tag))

data_country <- data_english %>% filter(grepl('country', tag))

data_folk <- data_english %>% filter(grepl('folk', tag))

data_gospel <- data_english %>% filter(grepl('gospel', tag))

data_spiritual <- data_english %>% filter(grepl('spiritual', tag))

data_rock <- data_english %>% filter(grepl('rock', tag))

data_heavymetal <- data_english %>% filter(grepl('heavy metal', tag))

data_hiphop <- data_english %>% filter(grepl('hip hop', tag))

data_soul <- data_english %>% filter(grepl('soul', tag))

data_jazz <- data_english %>% filter(grepl('jazz', tag))

data_kids <- data_english %>% filter(grepl('kids', tag))


####################################################
library(tokenizers)
library(textstem)

# stopword list
stopwords_extended <- readLines("stopwords_en.txt",
                                encoding = "UTF-8")
# copy lyrics of data frame pop
lyrics_pop <- data_pop[,5]

tokens_pop <- c()
tokens <- c()

for(i in lyrics_pop){
  temp<- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  temp <- sapply(temp, unique)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in temp){
    tokens <- c(tokens, j)
  }
  tokens <- unlist(tokens)
  tokens_pop <- c(tokens_pop, tokens)
  tokens <- c()
}
  
#data_pop$tokens <- tokens_pop


