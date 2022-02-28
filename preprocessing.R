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

'In the following code, the lyrics column of every genre data frame will 
be copied into lists. Then there are tokens lists of every genre, which will store
the tokens to each song of the genre. The tokens will be lemmatized. When the 
for loop iterated over each song and stored the tokens, the token lists will be copied into 
another set of token lists for each genre. These token list will be lemmatized and
be added to the corresponding data frame of the genre'

# copy lyrics of data frame pop
lyrics_pop <- data_pop[,5]

#list of tokens for every pop song
tokens_pop <- c()

for(i in lyrics_pop){
  temp<- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  #temp <- sapply(temp, unique)
  tokens_temp <- temp
  tokens_pop <- c(tokens_pop, tokens_temp)
}
tokens_pop2 <- tokens_pop
n <-1
for(i in tokens_pop){
  temp<- unlist(i)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in 1:length(tokens_pop2[[n]])){
    tokens_pop2[[n]][[j]] = temp[j]
  }
  n <- n+1
}
  
data_pop$tokens <- tokens_pop2

lyrics_blues <- data_blues[,5]

#list of tokens for every blues song
tokens_blues <- c()

for(i in lyrics_blues){
  temp<- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  #temp <- sapply(temp, unique)
  tokens_temp <- temp
  tokens_blues <- c(tokens_blues, tokens_temp)
}
tokens_blues2 <- tokens_blues
n <-1
for(i in tokens_spirit){
  temp<- unlist(i)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in 1:length(tokens_blues2[[n]])){
    tokens_blues2[[n]][[j]] = temp[j]
  }
  n <- n+1
}

data_blues$tokens <- tokens_blues2

lyrics_country <- data_country[,5]

#list of tokens for every country song
tokens_country <- c()

for(i in lyrics_country){
  temp<- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  #temp <- sapply(temp, unique)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  tokens_temp <- temp
  tokens_country <- c(tokens_country, tokens_temp)
}
tokens_country2 <- tokens_country
n <-1
for(i in tokens_spirit){
  temp<- unlist(i)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in 1:length(tokens_country2[[n]])){
    tokens_country2[[n]][[j]] = temp[j]
  }
  n <- n+1
}

data_country$tokens <- tokens_country2

lyrics_folk <- data_folk[,5]

#list of tokens for every folk song
tokens_folk <- c()

for(i in lyrics_folk){
  temp<- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  #temp <- sapply(temp, unique)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  tokens_temp <- temp
  tokens_folk <- c(tokens_folk, tokens_temp)
}

tokens_folk2 <- tokens_folk
n <-1
for(i in tokens_folk){
  temp<- unlist(i)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in 1:length(tokens_folk2[[n]])){
    tokens_folk2[[n]][[j]] = temp[j]
  }
  n <- n+1
}

data_folk$tokens <- tokens_folk2

lyrics_gospel <- data_gospel[,5]

#list of tokens for every gospel song
tokens_gospel <- c()

for(i in lyrics_gospel){
  temp<- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  #temp <- sapply(temp, unique)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  tokens_temp <- temp
  tokens_gospel <- c(tokens_gospel, tokens_temp)
}
tokens_gospel2 <- tokens_gospel
n <-1
for(i in tokens_gospel){
  temp<- unlist(i)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in 1:length(tokens_gospel2[[n]])){
    tokens_gospel2[[n]][[j]] = temp[j]
  }
  n <- n+1
}

data_gospel$tokens <- tokens_gospel2

lyrics_hm <- data_heavymetal[,5]

#list of tokens for every heavy metal song
tokens_hm <- c()

for(i in lyrics_hm){
  temp<- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  #temp <- sapply(temp, unique)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  tokens_temp <- temp
  tokens_hm <- c(tokens_hm, tokens_temp)
}
tokens_hm2 <- tokens_hm
n <-1
for(i in tokens_spirit){
  temp<- unlist(i)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in 1:length(tokens_hm2[[n]])){
    tokens_hm2[[n]][[j]] = temp[j]
  }
  n <- n+1
}
data_heavymetal$tokens <- tokens_hm2

lyrics_hh <- data_hiphop[,5]

#list of tokens for every hip-hop song
tokens_hh <- c()

for(i in lyrics_hh){
  temp<- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  #temp <- sapply(temp, unique)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  tokens_temp <- temp
  tokens_hh <- c(tokens_hh, tokens_temp)
}
tokens_hh2 <- tokens_hh
n <-1
for(i in tokens_spirit){
  temp<- unlist(i)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in 1:length(tokens_hh2[[n]])){
    tokens_hh2[[n]][[j]] = temp[j]
  }
  n <- n+1
}

data_hiphop$tokens <- tokens_hh2

lyrics_jazz <- data_jazz[,5]

#list of tokens for every jazz song
tokens_jazz <- c()

for(i in lyrics_jazz){
  temp<- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  #temp <- sapply(temp, unique)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  tokens_temp <- temp
  tokens_jazz <- c(tokens_jazz, tokens_temp)
}
tokens_jazz2 <- tokens_jazz
n <-1
for(i in tokens_jazz){
  temp<- unlist(i)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in 1:length(tokens_jazz2[[n]])){
    tokens_jazz2[[n]][[j]] = temp[j]
  }
  n <- n+1
}

data_jazz$tokens <- tokens_jazz2

lyrics_rap <- data_rap[,5]

#list of tokens for every rap song
tokens_rap <- c()

for(i in lyrics_rap){
  temp<- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  #temp <- sapply(temp, unique)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  tokens_temp <- temp
  tokens_rap <- c(tokens_rap, tokens_temp)
}
tokens_rap2 <- tokens_rap
n <-1
for(i in tokens_rap){
  temp<- unlist(i)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in 1:length(tokens_rap2[[n]])){
    tokens_rap2[[n]][[j]] = temp[j]
  }
  n <- n+1
}

data_rap$tokens <- tokens_rap2

lyrics_reggae <- data_reggae[,5]

#list of tokens for every reaggae song
tokens_reggae <- c()

for(i in lyrics_reggae){
  temp<- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  #temp <- sapply(temp, unique)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  tokens_temp <- temp
  tokens_reggae <- c(tokens_reggae, tokens_temp)
}

tokens_reggae2 <- tokens_reggae
n <-1
for(i in tokens_reggae){
  temp<- unlist(i)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in 1:length(tokens_reggae2[[n]])){
    tokens_reggae2[[n]][[j]] = temp[j]
  }
  n <- n+1
}

data_reggae$tokens <- tokens_reggae2

lyrics_rnb <- data_rnb[,5]

#list of tokens for every rnb song
tokens_rnb <- c()

for(i in lyrics_rnb){
  temp<- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  #temp <- sapply(temp, unique)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  tokens_temp <- temp
  tokens_rnb <- c(tokens_rnb, tokens_temp)
}
tokens_rnb2 <- tokens_rnb
n <-1
for(i in tokens_rnb){
  temp<- unlist(i)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in 1:length(tokens_rnb2[[n]])){
    tokens_rnb2[[n]][[j]] = temp[j]
  }
  n <- n+1
}

data_rnb$tokens <- tokens_rnb2

lyrics_rock <- data_rock[,5]

#list of tokens for every rock song
tokens_rock <- c()

for(i in lyrics_rock){
  temp<- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  #temp <- sapply(temp, unique)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  tokens_temp <- temp
  tokens_rock <- c(tokens_rock, tokens_temp)
}
tokens_rock2 <- tokens_rock
n <-1
for(i in tokens_rock){
  temp<- unlist(i)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in 1:length(tokens_rock2[[n]])){
    tokens_rock2[[n]][[j]] = temp[j]
  }
  n <- n+1
}

data_rock$tokens <- tokens_rock2

lyrics_soul <- data_soul[,5]

#list of tokens for every soul song
tokens_soul <- c()

for(i in lyrics_soul){
  temp<- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  #temp <- sapply(temp, unique)
  tokens_temp <- temp
  tokens_soul <- c(tokens_soul, tokens_temp)
}

tokens_soul2 <- tokens_soul
n <-1
for(i in tokens_soul){
  temp<- unlist(i)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in 1:length(tokens_soul2[[n]])){
    tokens_soul2[[n]][[j]] = temp[j]
  }
  n <- n+1
}

data_soul$tokens <- tokens_soul2

lyrics_spirit <- data_spiritual[,5]

#list of tokens for every spiritual song
tokens_spirit <- c()

for(i in lyrics_spirit){
  temp<- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  #temp <- sapply(temp, unique)
  tokens_temp <- temp
  tokens_spirit <- c(tokens_spirit, tokens_temp)
}

tokens_spirit2 <- tokens_spirit
n <-1
for(i in tokens_spirit){
  temp<- unlist(i)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in 1:length(tokens_spirit2[[n]])){
    tokens_spirit2[[n]][[j]] = temp[j]
  }
  n <- n+1
}

data_spiritual$tokens <- tokens_spirit2

lyrics_kids <- data_kids[,5]

#list of tokens for every kids song
tokens_kids <- c()

for(i in lyrics_kids){
  tokens_temp <- tokenize_words(i, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords_extended)
  tokens_kids <- c(tokens_kids, tokens_temp)
}

tokens_kids2 <- tokens_kids
n <-1
for(i in tokens_kids){
  temp<- unlist(i)
  temp <- lemmatize_words(temp, dictionary = lexicon::hash_lemmas)
  for(j in 1:length(tokens_kids2[[n]])){
    tokens_kids2[[n]][[j]] = temp[j]
  }
  n <- n+1
}


data_kids$tokens <- tokens_kids2