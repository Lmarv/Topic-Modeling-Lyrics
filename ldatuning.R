library("ldatuning")
result <- FindTopicsNumber(
  DTM,
  topics = seq(from = 2, to = 100, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)
