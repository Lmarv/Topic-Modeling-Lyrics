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


###### LDAvis ############

library(LDAvis)
library("tsne")
svd_tsne <- function(x) tsne(svd(x)$u)
json <- createJSON(phi = beta, theta = theta,
                   doc.length = rowSums(DTM), vocab = colnames(DTM), term.frequency = colSums(DTM),
                   mds.method = svd_tsne, plot.opts = list(xlab = "", ylab = ""))
serVis(json)