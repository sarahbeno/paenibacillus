# Read in HMM data; perform PCA, and add the components abck to hmm_data
# note that component 1 accounts for 79.5% of the variability in d14/d21 counts
# and will be used as the response in the linear models
hmm_data <- read.csv("hmm_data.csv")
growth_pca <- princomp(hmm_data[,c("Log_14","Log_21")])
hmm_data <- cbind(hmm_data,growth_pca$scores)

# For each query, we fit a model with the first principal component as response
# and the number of query matches per genome as dependent variable; each
# observation in a model is one isolate.
model_maker <- function(Query) {
  return(list(as.character(Query),
              lm(Comp.1 ~ CountMatchPerGenome,
                 data=hmm_data[hmm_data$Query==Query,])))
}
models <- lapply(unique(hmm_data$Query),FUN=model_maker)

# Print out the query protein followed by a summary of the corresponding
# linear model
for( m in models ) {
  print(m[[1]])
  print(summary(m[[2]]))
}