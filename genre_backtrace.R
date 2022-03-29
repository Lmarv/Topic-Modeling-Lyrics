library(dplyr)
library(lsa)

#function to calculate cosine similarity between a single song and the means of genres to find 
#the most similar genres
cos_sim <- function(song_id) {
  song <- theta[as.character(song_id),]
  blues_sim <- cosine(song, as.vector(t(topicProportionsBluesGenre)))
  country_sim <- cosine(song, as.vector(t(topicProportionsCountryGenre)))
  folk_sim <- cosine(song, as.vector(t(topicProportionsFolkGenre)))
  gospel_sim <- cosine(song, as.vector(t(topicProportionsGospelGenre)))
  heavymetal_sim <- cosine(song, as.vector(t(topicProportionsHeavyMetalGenre)))
  hiphop_sim <- cosine(song, as.vector(t(topicProportionsHiphopGenre)))
  jazz_sim <- cosine(song, as.vector(t(topicProportionsJazzGenre)))
  kids_sim <- cosine(song, as.vector(t(topicProportionsKidsGenre)))
  pop_sim <- cosine(song, as.vector(t(topicProportionsPopGenre)))
  rap_sim <- cosine(song, as.vector(t(topicProportionsRapGenre)))
  rnb_sim <- cosine(song, as.vector(t(topicProportionsRnbGenre)))
  reggae_sim <- cosine(song, as.vector(t(topicProportionsReggaeGenre)))
  rock_sim <- cosine(song, as.vector(t(topicProportionsRockGenre)))
  soul_sim <- cosine(song, as.vector(t(topicProportionsSoulGenre)))
  
  genre_sims <- data.frame(genre = c('blues', 'country', 'folk', 'gospel', 'heavy metal', 'hip hop', 'jazz', 
                                     'kids', 'pop', 'rap', 'rnb', 'reggae', 'rock', 'soul'),
                           similarity = c(blues_sim, country_sim, folk_sim, gospel_sim, heavymetal_sim, hiphop_sim, 
                           jazz_sim, kids_sim, pop_sim, rap_sim, rnb_sim, reggae_sim, rock_sim, soul_sim))
  
  top_3_genres <- genre_sims %>% arrange(-similarity) %>% head(3)
  return(top_3_genres$genre)
}

#create data frame including the correct genre and the top 3 most similar genres
sim_df <- data.frame()

for (s in sample_data$song_id) {
  top_3 <- cos_sim(s)
  genre <- as.character(sample_data[which(sample_data$song_id == s), 6])
  sim_df <- rbind(sim_df, c(s, genre, top_3))
}

colnames(sim_df) <- c('song_id','genre','top1','top2','top3')

sim_df$identified <- ifelse(sim_df$genre == sim_df$top1 | sim_df$genre == sim_df$top2 | 
                              sim_df$genre == sim_df$top3, TRUE, FALSE)

#calculate the ratio of correctly identified genres
correctly_identified <- length(which(sim_df$identified)) / nrow(sample_data)

print(paste(paste("In", format(round(correctly_identified*100, 2), nsmall = 2)), "% of the songs the correct genres occur in the top 3 most similar genres regarding to the calculated theta values."))
