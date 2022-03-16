####################visualization wordclouds#########################################
require(wordcloud2)

#Topic to visualize as wordcloud. Set different Topic number and topic word for other topic to visualize.

topic <- 9

word <- 'baby'

topicToViz <- topic

topicToViz <- grep(word, topicNames)[1]

top40terms <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:40]
words <- names(top40terms)

probabilities <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:40]

wordcloud2(data.frame(words, probabilities), shuffle = FALSE)


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

#Here the proportions of the topics are visualized per genre

### POP ###
#creating empty vector for pop song ids
exampleIds <- c()

#getting the ids from pop_idx, step needed to get the right theta values from pop songs
exampleIds <- as.character(pop_idx)

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
colnames(topicProportionsPopGenre) <- topicNames

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


### RAP ###
#creating empty vector for rap song ids
exampleIds <- c()

#getting the ids from rap_idx, step needed to get the right theta values from rap songs
exampleIds <- as.character(rap_idx)

#creating new data frame with only rap song proportions of topics
topicProportionExamples <- theta[exampleIds,]
topicProportionExamples<- as.data.frame(topicProportionExamples)

#initializing new data frame for calculated means of theta values for all topics over the whole rap song sample
topicProportionsRapGenre = data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(topicProportionsRapGenre) = columns

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
topicProportionsRapGenre$`Topic 1` <- mean(topicProportionExamples$`Topic 1`)
topicProportionsRapGenre$`Topic 2` <- mean(topicProportionExamples$`Topic 2`)
topicProportionsRapGenre$`Topic 3` <- mean(topicProportionExamples$`Topic 3`)
topicProportionsRapGenre$`Topic 4` <- mean(topicProportionExamples$`Topic 4`)
topicProportionsRapGenre$`Topic 5` <- mean(topicProportionExamples$`Topic 5`)
topicProportionsRapGenre$`Topic 6` <- mean(topicProportionExamples$`Topic 6`)
topicProportionsRapGenre$`Topic 7` <- mean(topicProportionExamples$`Topic 7`)
topicProportionsRapGenre$`Topic 8` <- mean(topicProportionExamples$`Topic 8`)
topicProportionsRapGenre$`Topic 9` <- mean(topicProportionExamples$`Topic 9`)
topicProportionsRapGenre$`Topic 10` <- mean(topicProportionExamples$`Topic 10`)
topicProportionsRapGenre$`Topic 11` <- mean(topicProportionExamples$`Topic 11`)
topicProportionsRapGenre$`Topic 12` <- mean(topicProportionExamples$`Topic 12`)

#renaming the columns to the top5 words of the topic
colnames(topicProportionsRapGenre) <- topicNames

#visualize data as horizontal bar plots
vizDataFrame <- melt(cbind(
  data.frame(topicProportionsRapGenre),
  rap = factor(1:1)),
  variable.name = "topic", id.vars = "rap")

ggplot(data = vizDataFrame,
       aes(topic, value, fill = rap), ylab = "proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ rap, ncol = 1)


### REGGAE ###
#creating empty vector for rap song ids
exampleIds <- c()

#getting the ids from reggae_idx, step needed to get the right theta values from reggae songs
exampleIds <- as.character(reggae_idx)

#creating new data frame with only reggae song proportions of topics
topicProportionExamples <- theta[exampleIds,]
topicProportionExamples<- as.data.frame(topicProportionExamples)

#initializing new data frame for calculated means of theta values for all topics over the whole reggae song sample
topicProportionsReggaeGenre = data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(topicProportionsReggaeGenre) = columns

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
topicProportionsReggaeGenre$`Topic 1` <- mean(topicProportionExamples$`Topic 1`)
topicProportionsReggaeGenre$`Topic 2` <- mean(topicProportionExamples$`Topic 2`)
topicProportionsReggaeGenre$`Topic 3` <- mean(topicProportionExamples$`Topic 3`)
topicProportionsReggaeGenre$`Topic 4` <- mean(topicProportionExamples$`Topic 4`)
topicProportionsReggaeGenre$`Topic 5` <- mean(topicProportionExamples$`Topic 5`)
topicProportionsReggaeGenre$`Topic 6` <- mean(topicProportionExamples$`Topic 6`)
topicProportionsReggaeGenre$`Topic 7` <- mean(topicProportionExamples$`Topic 7`)
topicProportionsReggaeGenre$`Topic 8` <- mean(topicProportionExamples$`Topic 8`)
topicProportionsReggaeGenre$`Topic 9` <- mean(topicProportionExamples$`Topic 9`)
topicProportionsReggaeGenre$`Topic 10` <- mean(topicProportionExamples$`Topic 10`)
topicProportionsReggaeGenre$`Topic 11` <- mean(topicProportionExamples$`Topic 11`)
topicProportionsReggaeGenre$`Topic 12` <- mean(topicProportionExamples$`Topic 12`)

#renaming the columns to the top5 words of the topic
colnames(topicProportionsReggaeGenre) <- topicNames

#visualize data as horizontal bar plots
vizDataFrame <- melt(cbind(
  data.frame(topicProportionsReggaeGenre),
  reggae = factor(1:1)),
  variable.name = "topic", id.vars = "reggae")

ggplot(data = vizDataFrame,
       aes(topic, value, fill = reggae), ylab = "proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ reggae, ncol = 1)


### RNB ###
#creating empty vector for rnb song ids
exampleIds <- c()

#getting the ids from rnb_idx, step needed to get the right theta values from rnb songs
exampleIds <- as.character(rnb_idx)

#creating new data frame with only rnb song proportions of topics
topicProportionExamples <- theta[exampleIds,]
topicProportionExamples<- as.data.frame(topicProportionExamples)

#initializing new data frame for calculated means of theta values for all topics over the whole rnb song sample
topicProportionsRnbGenre = data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(topicProportionsRnbGenre) = columns

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
topicProportionsRnbGenre$`Topic 1` <- mean(topicProportionExamples$`Topic 1`)
topicProportionsRnbGenre$`Topic 2` <- mean(topicProportionExamples$`Topic 2`)
topicProportionsRnbGenre$`Topic 3` <- mean(topicProportionExamples$`Topic 3`)
topicProportionsRnbGenre$`Topic 4` <- mean(topicProportionExamples$`Topic 4`)
topicProportionsRnbGenre$`Topic 5` <- mean(topicProportionExamples$`Topic 5`)
topicProportionsRnbGenre$`Topic 6` <- mean(topicProportionExamples$`Topic 6`)
topicProportionsRnbGenre$`Topic 7` <- mean(topicProportionExamples$`Topic 7`)
topicProportionsRnbGenre$`Topic 8` <- mean(topicProportionExamples$`Topic 8`)
topicProportionsRnbGenre$`Topic 9` <- mean(topicProportionExamples$`Topic 9`)
topicProportionsRnbGenre$`Topic 10` <- mean(topicProportionExamples$`Topic 10`)
topicProportionsRnbGenre$`Topic 11` <- mean(topicProportionExamples$`Topic 11`)
topicProportionsRnbGenre$`Topic 12` <- mean(topicProportionExamples$`Topic 12`)

#renaming the columns to the top5 words of the topic
colnames(topicProportionsRnbGenre) <- topicNames

#visualize data as horizontal bar plots
vizDataFrame <- melt(cbind(
  data.frame(topicProportionsRnbGenre),
  rnb = factor(1:1)),
  variable.name = "topic", id.vars = "rnb")

ggplot(data = vizDataFrame,
       aes(topic, value, fill = rnb), ylab = "proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ rnb, ncol = 1)


### BLUES ###
#creating empty vector for blues song ids
exampleIds <- c()

#getting the ids from blues_idx, step needed to get the right theta values from blues songs
exampleIds <- as.character(blues_idx)

#creating new data frame with only rap song proportions of topics
topicProportionExamples <- theta[exampleIds,]
topicProportionExamples<- as.data.frame(topicProportionExamples)

#initializing new data frame for calculated means of theta values for all topics over the whole blues song sample
topicProportionsBluesGenre = data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(topicProportionsBluesGenre) = columns

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
topicProportionsBluesGenre$`Topic 1` <- mean(topicProportionExamples$`Topic 1`)
topicProportionsBluesGenre$`Topic 2` <- mean(topicProportionExamples$`Topic 2`)
topicProportionsBluesGenre$`Topic 3` <- mean(topicProportionExamples$`Topic 3`)
topicProportionsBluesGenre$`Topic 4` <- mean(topicProportionExamples$`Topic 4`)
topicProportionsBluesGenre$`Topic 5` <- mean(topicProportionExamples$`Topic 5`)
topicProportionsBluesGenre$`Topic 6` <- mean(topicProportionExamples$`Topic 6`)
topicProportionsBluesGenre$`Topic 7` <- mean(topicProportionExamples$`Topic 7`)
topicProportionsBluesGenre$`Topic 8` <- mean(topicProportionExamples$`Topic 8`)
topicProportionsBluesGenre$`Topic 9` <- mean(topicProportionExamples$`Topic 9`)
topicProportionsBluesGenre$`Topic 10` <- mean(topicProportionExamples$`Topic 10`)
topicProportionsBluesGenre$`Topic 11` <- mean(topicProportionExamples$`Topic 11`)
topicProportionsBluesGenre$`Topic 12` <- mean(topicProportionExamples$`Topic 12`)

#renaming the columns to the top5 words of the topic
colnames(topicProportionsBluesGenre) <- topicNames

#visualize data as horizontal bar plots
vizDataFrame <- melt(cbind(
  data.frame(topicProportionsBluesGenre),
  blues = factor(1:1)),
  variable.name = "topic", id.vars = "blues")

ggplot(data = vizDataFrame,
       aes(topic, value, fill = blues), ylab = "proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ blues, ncol = 1)


### COUNTRY ###
#creating empty vector for country song ids
exampleIds <- c()

#getting the ids from country_idx, step needed to get the right theta values from country songs
exampleIds <- as.character(country_idx)

#creating new data frame with only rap song proportions of topics
topicProportionExamples <- theta[exampleIds,]
topicProportionExamples<- as.data.frame(topicProportionExamples)

#initializing new data frame for calculated means of theta values for all topics over the whole country song sample
topicProportionsCountryGenre = data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(topicProportionsCountryGenre) = columns

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
topicProportionsCountryGenre$`Topic 1` <- mean(topicProportionExamples$`Topic 1`)
topicProportionsCountryGenre$`Topic 2` <- mean(topicProportionExamples$`Topic 2`)
topicProportionsCountryGenre$`Topic 3` <- mean(topicProportionExamples$`Topic 3`)
topicProportionsCountryGenre$`Topic 4` <- mean(topicProportionExamples$`Topic 4`)
topicProportionsCountryGenre$`Topic 5` <- mean(topicProportionExamples$`Topic 5`)
topicProportionsCountryGenre$`Topic 6` <- mean(topicProportionExamples$`Topic 6`)
topicProportionsCountryGenre$`Topic 7` <- mean(topicProportionExamples$`Topic 7`)
topicProportionsCountryGenre$`Topic 8` <- mean(topicProportionExamples$`Topic 8`)
topicProportionsCountryGenre$`Topic 9` <- mean(topicProportionExamples$`Topic 9`)
topicProportionsCountryGenre$`Topic 10` <- mean(topicProportionExamples$`Topic 10`)
topicProportionsCountryGenre$`Topic 11` <- mean(topicProportionExamples$`Topic 11`)
topicProportionsCountryGenre$`Topic 12` <- mean(topicProportionExamples$`Topic 12`)

#renaming the columns to the top5 words of the topic
colnames(topicProportionsCountryGenre) <- topicNames

#visualize data as horizontal bar plots
vizDataFrame <- melt(cbind(
  data.frame(topicProportionsCountryGenre),
  country = factor(1:1)),
  variable.name = "topic", id.vars = "country")

ggplot(data = vizDataFrame,
       aes(topic, value, fill = country), ylab = "proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ country, ncol = 1)


### FOLK ###
#creating empty vector for folk song ids
exampleIds <- c()

#getting the ids from folk_idx, step needed to get the right theta values from folk songs
exampleIds <- as.character(folk_idx)

#creating new data frame with only folk song proportions of topics
topicProportionExamples <- theta[exampleIds,]
topicProportionExamples<- as.data.frame(topicProportionExamples)

#initializing new data frame for calculated means of theta values for all topics over the whole folk song sample
topicProportionsFolkGenre = data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(topicProportionsFolkGenre) = columns

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
topicProportionsFolkGenre$`Topic 1` <- mean(topicProportionExamples$`Topic 1`)
topicProportionsFolkGenre$`Topic 2` <- mean(topicProportionExamples$`Topic 2`)
topicProportionsFolkGenre$`Topic 3` <- mean(topicProportionExamples$`Topic 3`)
topicProportionsFolkGenre$`Topic 4` <- mean(topicProportionExamples$`Topic 4`)
topicProportionsFolkGenre$`Topic 5` <- mean(topicProportionExamples$`Topic 5`)
topicProportionsFolkGenre$`Topic 6` <- mean(topicProportionExamples$`Topic 6`)
topicProportionsFolkGenre$`Topic 7` <- mean(topicProportionExamples$`Topic 7`)
topicProportionsFolkGenre$`Topic 8` <- mean(topicProportionExamples$`Topic 8`)
topicProportionsFolkGenre$`Topic 9` <- mean(topicProportionExamples$`Topic 9`)
topicProportionsFolkGenre$`Topic 10` <- mean(topicProportionExamples$`Topic 10`)
topicProportionsFolkGenre$`Topic 11` <- mean(topicProportionExamples$`Topic 11`)
topicProportionsFolkGenre$`Topic 12` <- mean(topicProportionExamples$`Topic 12`)

#renaming the columns to the top5 words of the topic
colnames(topicProportionsFolkGenre) <- topicNames 

#visualize data as horizontal bar plots
vizDataFrame <- melt(cbind(
  data.frame(topicProportionsFolkGenre),
  folk = factor(1:1)),
  variable.name = "topic", id.vars = "folk")

ggplot(data = vizDataFrame,
       aes(topic, value, fill = folk), ylab = "proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ folk, ncol = 1)


### GOSPEL/SPIRITUAL ###
#creating empty vector for rap song ids
exampleIds <- c()

#getting the ids from gospel_spiritual_idx, step needed to get the right theta values from gospel songs
exampleIds <- as.character(gospel_spiritual_idx)

#creating new data frame with only gospel song proportions of topics
topicProportionExamples <- theta[exampleIds,]
topicProportionExamples<- as.data.frame(topicProportionExamples)

#initializing new data frame for calculated means of theta values for all topics over the whole gospel song sample
topicProportionsGospelGenre = data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(topicProportionsGospelGenre) = columns

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
topicProportionsGospelGenre$`Topic 1` <- mean(topicProportionExamples$`Topic 1`)
topicProportionsGospelGenre$`Topic 2` <- mean(topicProportionExamples$`Topic 2`)
topicProportionsGospelGenre$`Topic 3` <- mean(topicProportionExamples$`Topic 3`)
topicProportionsGospelGenre$`Topic 4` <- mean(topicProportionExamples$`Topic 4`)
topicProportionsGospelGenre$`Topic 5` <- mean(topicProportionExamples$`Topic 5`)
topicProportionsGospelGenre$`Topic 6` <- mean(topicProportionExamples$`Topic 6`)
topicProportionsGospelGenre$`Topic 7` <- mean(topicProportionExamples$`Topic 7`)
topicProportionsGospelGenre$`Topic 8` <- mean(topicProportionExamples$`Topic 8`)
topicProportionsGospelGenre$`Topic 9` <- mean(topicProportionExamples$`Topic 9`)
topicProportionsGospelGenre$`Topic 10` <- mean(topicProportionExamples$`Topic 10`)
topicProportionsGospelGenre$`Topic 11` <- mean(topicProportionExamples$`Topic 11`)
topicProportionsGospelGenre$`Topic 12` <- mean(topicProportionExamples$`Topic 12`)

#renaming the columns to the top5 words of the topic
colnames(topicProportionsGospelGenre) <- topicNames 

#visualize data as horizontal bar plots
vizDataFrame <- melt(cbind(
  data.frame(topicProportionsGospelGenre),
  gospel = factor(1:1)),
  variable.name = "topic", id.vars = "gospel")

ggplot(data = vizDataFrame,
       aes(topic, value, fill = gospel), ylab = "proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ gospel, ncol = 1)


### ROCK ###
#creating empty vector for rock song ids
exampleIds <- c()

#getting the ids from rock_idx and concatenate them with 'text' in front of ids, step needed to get the right theta values from rock songs
exampleIds <- as.character(rock_idx)

#creating new data frame with only rap song proportions of topics
topicProportionExamples <- theta[exampleIds,]
topicProportionExamples<- as.data.frame(topicProportionExamples)

#initializing new data frame for calculated means of theta values for all topics over the whole rock song sample
topicProportionsRockGenre = data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(topicProportionsRockGenre) = columns

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
topicProportionsRockGenre$`Topic 1` <- mean(topicProportionExamples$`Topic 1`)
topicProportionsRockGenre$`Topic 2` <- mean(topicProportionExamples$`Topic 2`)
topicProportionsRockGenre$`Topic 3` <- mean(topicProportionExamples$`Topic 3`)
topicProportionsRockGenre$`Topic 4` <- mean(topicProportionExamples$`Topic 4`)
topicProportionsRockGenre$`Topic 5` <- mean(topicProportionExamples$`Topic 5`)
topicProportionsRockGenre$`Topic 6` <- mean(topicProportionExamples$`Topic 6`)
topicProportionsRockGenre$`Topic 7` <- mean(topicProportionExamples$`Topic 7`)
topicProportionsRockGenre$`Topic 8` <- mean(topicProportionExamples$`Topic 8`)
topicProportionsRockGenre$`Topic 9` <- mean(topicProportionExamples$`Topic 9`)
topicProportionsRockGenre$`Topic 10` <- mean(topicProportionExamples$`Topic 10`)
topicProportionsRockGenre$`Topic 11` <- mean(topicProportionExamples$`Topic 11`)
topicProportionsRockGenre$`Topic 12` <- mean(topicProportionExamples$`Topic 12`)

#renaming the columns to the top5 words of the topic
colnames(topicProportionsRockGenre) <- topicNames 

#visualize data as horizontal bar plots
vizDataFrame <- melt(cbind(
  data.frame(topicProportionsRockGenre),
  rock = factor(1:1)),
  variable.name = "topic", id.vars = "rock")

ggplot(data = vizDataFrame,
       aes(topic, value, fill = rock), ylab = "proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ rock, ncol = 1)


### HEAVY METAL ###
#creating empty vector for heavy metal song ids
exampleIds <- c()

#getting the ids from heavyMetal_idx, step needed to get the right theta values from heavy metal songs
exampleIds <- as.character(heavyMetal_idx)

#creating new data frame with only heavy metal song proportions of topics
topicProportionExamples <- theta[exampleIds,]
topicProportionExamples<- as.data.frame(topicProportionExamples)

#initializing new data frame for calculated means of theta values for all topics over the whole heavy metal song sample
topicProportionsHeavyMetalGenre = data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(topicProportionsHeavyMetalGenre) = columns

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
topicProportionsHeavyMetalGenre$`Topic 1` <- mean(topicProportionExamples$`Topic 1`)
topicProportionsHeavyMetalGenre$`Topic 2` <- mean(topicProportionExamples$`Topic 2`)
topicProportionsHeavyMetalGenre$`Topic 3` <- mean(topicProportionExamples$`Topic 3`)
topicProportionsHeavyMetalGenre$`Topic 4` <- mean(topicProportionExamples$`Topic 4`)
topicProportionsHeavyMetalGenre$`Topic 5` <- mean(topicProportionExamples$`Topic 5`)
topicProportionsHeavyMetalGenre$`Topic 6` <- mean(topicProportionExamples$`Topic 6`)
topicProportionsHeavyMetalGenre$`Topic 7` <- mean(topicProportionExamples$`Topic 7`)
topicProportionsHeavyMetalGenre$`Topic 8` <- mean(topicProportionExamples$`Topic 8`)
topicProportionsHeavyMetalGenre$`Topic 9` <- mean(topicProportionExamples$`Topic 9`)
topicProportionsHeavyMetalGenre$`Topic 10` <- mean(topicProportionExamples$`Topic 10`)
topicProportionsHeavyMetalGenre$`Topic 11` <- mean(topicProportionExamples$`Topic 11`)
topicProportionsHeavyMetalGenre$`Topic 12` <- mean(topicProportionExamples$`Topic 12`)

#renaming the columns to the top5 words of the topic
colnames(topicProportionsHeavyMetalGenre) <- topicNames 

#visualize data as horizontal bar plots
vizDataFrame <- melt(cbind(
  data.frame(topicProportionsHeavyMetalGenre),
  heavyMetal = factor(1:1)),
  variable.name = "topic", id.vars = "heavyMetal")

ggplot(data = vizDataFrame,
       aes(topic, value, fill = heavyMetal), ylab = "proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ heavyMetal, ncol = 1)


### HIP HOP ###
#creating empty vector for hiphop song ids
exampleIds <- c()

#getting the ids from hipHop_idx, step needed to get the right theta values from hiphop songs
exampleIds <- as.character(hipHop_idx)

#creating new data frame with only rap song proportions of topics
topicProportionExamples <- theta[exampleIds,]
topicProportionExamples<- as.data.frame(topicProportionExamples)

#initializing new data frame for calculated means of theta values for all topics over the whole hiphop song sample
topicProportionsHiphopGenre = data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(topicProportionsHiphopGenre) = columns

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
topicProportionsHiphopGenre$`Topic 1` <- mean(topicProportionExamples$`Topic 1`)
topicProportionsHiphopGenre$`Topic 2` <- mean(topicProportionExamples$`Topic 2`)
topicProportionsHiphopGenre$`Topic 3` <- mean(topicProportionExamples$`Topic 3`)
topicProportionsHiphopGenre$`Topic 4` <- mean(topicProportionExamples$`Topic 4`)
topicProportionsHiphopGenre$`Topic 5` <- mean(topicProportionExamples$`Topic 5`)
topicProportionsHiphopGenre$`Topic 6` <- mean(topicProportionExamples$`Topic 6`)
topicProportionsHiphopGenre$`Topic 7` <- mean(topicProportionExamples$`Topic 7`)
topicProportionsHiphopGenre$`Topic 8` <- mean(topicProportionExamples$`Topic 8`)
topicProportionsHiphopGenre$`Topic 9` <- mean(topicProportionExamples$`Topic 9`)
topicProportionsHiphopGenre$`Topic 10` <- mean(topicProportionExamples$`Topic 10`)
topicProportionsHiphopGenre$`Topic 11` <- mean(topicProportionExamples$`Topic 11`)
topicProportionsHiphopGenre$`Topic 12` <- mean(topicProportionExamples$`Topic 12`)

#renaming the columns to the top5 words of the topic
colnames(topicProportionsHiphopGenre) <- topicNames 

#visualize data as horizontal bar plots
vizDataFrame <- melt(cbind(
  data.frame(topicProportionsHiphopGenre),
  hiphop = factor(1:1)),
  variable.name = "topic", id.vars = "hiphop")

ggplot(data = vizDataFrame,
       aes(topic, value, fill = hiphop), ylab = "proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ hiphop, ncol = 1)



### SOUL ###
#creating empty vector for soul song ids
exampleIds <- c()

#getting the ids from soul_idx, step needed to get the right theta values from soul songs
exampleIds <- as.character(soul_idx)

#creating new data frame with only soul song proportions of topics
topicProportionExamples <- theta[exampleIds,]
topicProportionExamples<- as.data.frame(topicProportionExamples)

#initializing new data frame for calculated means of theta values for all topics over the whole soul song sample
topicProportionsSoulGenre = data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(topicProportionsSoulGenre) = columns

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
topicProportionsSoulGenre$`Topic 1` <- mean(topicProportionExamples$`Topic 1`)
topicProportionsSoulGenre$`Topic 2` <- mean(topicProportionExamples$`Topic 2`)
topicProportionsSoulGenre$`Topic 3` <- mean(topicProportionExamples$`Topic 3`)
topicProportionsSoulGenre$`Topic 4` <- mean(topicProportionExamples$`Topic 4`)
topicProportionsSoulGenre$`Topic 5` <- mean(topicProportionExamples$`Topic 5`)
topicProportionsSoulGenre$`Topic 6` <- mean(topicProportionExamples$`Topic 6`)
topicProportionsSoulGenre$`Topic 7` <- mean(topicProportionExamples$`Topic 7`)
topicProportionsSoulGenre$`Topic 8` <- mean(topicProportionExamples$`Topic 8`)
topicProportionsSoulGenre$`Topic 9` <- mean(topicProportionExamples$`Topic 9`)
topicProportionsSoulGenre$`Topic 10` <- mean(topicProportionExamples$`Topic 10`)
topicProportionsSoulGenre$`Topic 11` <- mean(topicProportionExamples$`Topic 11`)
topicProportionsSoulGenre$`Topic 12` <- mean(topicProportionExamples$`Topic 12`)

#renaming the columns to the top5 words of the topic
colnames(topicProportionsSoulGenre) <- topicNames 

#visualize data as horizontal bar plots
vizDataFrame <- melt(cbind(
  data.frame(topicProportionsSoulGenre),
  soul = factor(1:1)),
  variable.name = "topic", id.vars = "soul")

ggplot(data = vizDataFrame,
       aes(topic, value, fill = soul), ylab = "proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ soul, ncol = 1)


### JAZZ ###
#creating empty vector for jazz song ids
exampleIds <- c()

#getting the ids from jazz_idx, step needed to get the right theta values from jazz songs
exampleIds <- as.character(jazz_idx)

#creating new data frame with only jazz song proportions of topics
topicProportionExamples <- theta[exampleIds,]
topicProportionExamples<- as.data.frame(topicProportionExamples)

#initializing new data frame for calculated means of theta values for all topics over the whole jazz song sample
topicProportionsJazzGenre = data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(topicProportionsJazzGenre) = columns

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
topicProportionsJazzGenre$`Topic 1` <- mean(topicProportionExamples$`Topic 1`)
topicProportionsJazzGenre$`Topic 2` <- mean(topicProportionExamples$`Topic 2`)
topicProportionsJazzGenre$`Topic 3` <- mean(topicProportionExamples$`Topic 3`)
topicProportionsJazzGenre$`Topic 4` <- mean(topicProportionExamples$`Topic 4`)
topicProportionsJazzGenre$`Topic 5` <- mean(topicProportionExamples$`Topic 5`)
topicProportionsJazzGenre$`Topic 6` <- mean(topicProportionExamples$`Topic 6`)
topicProportionsJazzGenre$`Topic 7` <- mean(topicProportionExamples$`Topic 7`)
topicProportionsJazzGenre$`Topic 8` <- mean(topicProportionExamples$`Topic 8`)
topicProportionsJazzGenre$`Topic 9` <- mean(topicProportionExamples$`Topic 9`)
topicProportionsJazzGenre$`Topic 10` <- mean(topicProportionExamples$`Topic 10`)
topicProportionsJazzGenre$`Topic 11` <- mean(topicProportionExamples$`Topic 11`)
topicProportionsJazzGenre$`Topic 12` <- mean(topicProportionExamples$`Topic 12`)

#renaming the columns to the top5 words of the topic
colnames(topicProportionsJazzGenre) <- topicNames 

#visualize data as horizontal bar plots
vizDataFrame <- melt(cbind(
  data.frame(topicProportionsJazzGenre),
  jazz = factor(1:1)),
  variable.name = "topic", id.vars = "jazz")

ggplot(data = vizDataFrame,
       aes(topic, value, fill = jazz), ylab = "proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ jazz, ncol = 1)


### KIDS ###
#creating empty vector for kids song ids
exampleIds <- c()

#getting the ids from kids_idx, step needed to get the right theta values from kids songs
exampleIds <- as.character(kids_idx)

#creating new data frame with only kids song proportions of topics
topicProportionExamples <- theta[exampleIds,]
topicProportionExamples<- as.data.frame(topicProportionExamples)

#initializing new data frame for calculated means of theta values for all topics over the whole kids song sample
topicProportionsKidsGenre = data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(topicProportionsKidsGenre) = columns

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
topicProportionsKidsGenre$`Topic 1` <- mean(topicProportionExamples$`Topic 1`)
topicProportionsKidsGenre$`Topic 2` <- mean(topicProportionExamples$`Topic 2`)
topicProportionsKidsGenre$`Topic 3` <- mean(topicProportionExamples$`Topic 3`)
topicProportionsKidsGenre$`Topic 4` <- mean(topicProportionExamples$`Topic 4`)
topicProportionsKidsGenre$`Topic 5` <- mean(topicProportionExamples$`Topic 5`)
topicProportionsKidsGenre$`Topic 6` <- mean(topicProportionExamples$`Topic 6`)
topicProportionsKidsGenre$`Topic 7` <- mean(topicProportionExamples$`Topic 7`)
topicProportionsKidsGenre$`Topic 8` <- mean(topicProportionExamples$`Topic 8`)
topicProportionsKidsGenre$`Topic 9` <- mean(topicProportionExamples$`Topic 9`)
topicProportionsKidsGenre$`Topic 10` <- mean(topicProportionExamples$`Topic 10`)
topicProportionsKidsGenre$`Topic 11` <- mean(topicProportionExamples$`Topic 11`)
topicProportionsKidsGenre$`Topic 12` <- mean(topicProportionExamples$`Topic 12`)

#renaming the columns to the top5 words of the topic
colnames(topicProportionsKidsGenre) <- topicNames

#visualize data as horizontal bar plots
vizDataFrame <- melt(cbind(
  data.frame(topicProportionsKidsGenre),
  kids = factor(1:1)),
  variable.name = "topic", id.vars = "kids")

ggplot(data = vizDataFrame,
       aes(topic, value, fill = kids), ylab = "proportion") +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ kids, ncol = 1)



######### topic proportions per genre ###################
require(reshape2)
# get mean topic proportions per genre 

# step necessary to put gospel and spiritual data in one column for visualization
sample_data[sample_data == 'gospel' | sample_data == 'spiritual'] <- 'gospel + spiritual'

topic_proportion_per_genre <- aggregate(theta, by = list(genre = sample_data$tag), mean)

# set topic names to aggregated columns
colnames(topic_proportion_per_genre)[2:(K+1)] <- topicNames

# reshape data frame
vizDataFrame <- melt(topic_proportion_per_genre, id.vars = "genre") 

# plot topic proportions per genre as bar plot
require(pals)
ggplot(vizDataFrame,
       aes(x=genre, y=value, fill=variable)) +
  geom_bar(stat = "identity") + ylab("proportion") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "genre") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


