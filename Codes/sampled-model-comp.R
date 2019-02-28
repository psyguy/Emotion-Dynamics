source("./Codes/My Functions/my.library.loader.R")

sampled.path <- "../Processed data/sampled mirt models"
sampled.names <- list.files(path = sampled.path, pattern = "*.RData")


list.of.names <- list('CLINICAL ESM','Cogito','ELISE ESM14', 'JULIAN EGON',
                      'KATHLEEN STRESSKLINIEK', 'Laura ESM 2014','Laura ESM 2016',
                      'LONGITUDINAL W1', 'LONGITUDINAL W2', 'LONGITUDINAL W3','MARLIES BPD',
                      'MDD BPD TRULL', 'MDD GOTLIB', 'MTURK DAILY DIARY', 
                      'PETEMADDY')


outcomes <- data.frame(matrix(ncol = 15, nrow = 1))
colnames(outcomes) <- list.of.names

criterion <- "BIC"
for(sampled in sampled.names){
  load(paste0("./Processed data/sampled mirt models/", sampled, sep = ""))
  name <- sampled.mirt$name
  round <- sampled.mirt$round
  outcomes[round, name] <- sampled.mirt %>% my.mirt.comparison(criterion = criterion)
  sampled.mirt$model %>% l_ply(summary)
}