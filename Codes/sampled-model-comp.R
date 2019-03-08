source("./Codes/My-Functions/my.library.loader.R")

sampled.path <- "../Emotion Dynamics 1/ED 1 - Codes/3-mirt-Model-Comp/Processed-457/1persons.0days.0beeps/"
sampled.path <- "../Emotion Dynamics 1/ED 1 - Codes/3-mirt-Model-Comp/Processed-457/0persons.0days.1beeps/"

sample.name <- "20persons.0days.0beeps"
sampled.path <- paste("../Emotion Dynamics 1/ED 1 - Codes/3-mirt-Model-Comp/Processed-457/",
                       sample.name, "/", sep = "")

sampled.names <- list.files(path = sampled.path, pattern = "*.RData")


list.of.names <- list('CLINICAL ESM','Cogito','ELISE ESM14', 'JULIAN EGON',
                      'KATHLEEN STRESSKLINIEK', 'Laura ESM 2014','Laura ESM 2016',
                      'LONGITUDINAL W1', 'LONGITUDINAL W2', 'LONGITUDINAL W3','MARLIES BPD',
                      'MDD BPD TRULL', 'MDD GOTLIB', 'MTURK DAILY DIARY', 
                      'PETEMADDY')


outcomes <- data.frame(matrix(nrow = 0, ncol = 8))
# colnames(outcomes) <- list.of.names

t <- Sys.time()
for(sampled in sampled.names){
  load(paste(sampled.path, sampled, sep = ""))
  outcome.single <- to.be.saved %>% my.mirt.comparison
  outcomes <- outcome.single %>% rbind(outcomes)
}
Sys.time() - t

criterion.list <- c("AIC", "AICc", "SABIC", "HQ", "BIC", "logLik")

for(c in criterion.list) my.vote.plotter(outcomes, criterion = c, sample.name = "0persons.0days.1beeps")

# criterion.list %>% l_ply(my.vote.plotter, outcomes = outcomes, sample.name = "0persons.0days.1beeps")


getwd()
