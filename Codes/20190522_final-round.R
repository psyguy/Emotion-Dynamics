# reading the functions ---------------------------------------------------
source("./Codes/My-Functions/my.library.loader.R")
# rm(list=ls())


# reading appropriate samples ---------------------------------------------

technique <- "r.sameocc"

if(technique=="p.all") sampled.path <- "./22-May/p_all/"
if(technique=="r.randocc") sampled.path <- "./22-May/r_randocc/"
if(technique=="r.sameocc") sampled.path <- "./22-May/r_sameocc/"
sampled.names <- list.files(path = sampled.path, pattern = "*.RData")



## full list
# list.of.names <- list('CLINICAL ESM','Cogito','ELISE ESM14', 'JULIAN EGON',
#                       'KATHLEEN STRESSKLINIEK', 'Laura ESM 2014','Laura ESM 2016',
#                       'LONGITUDINAL W1', 'LONGITUDINAL W2', 'LONGITUDINAL W3','MARLIES BPD',
#                       'MDD BPD TRULL', 'MDD GOTLIB', 'MTURK DAILY DIARY', 
#                       'PETEMADDY')

## list of 12 datasets
list.of.names <- list('CLINICAL ESM','ELISE ESM14', 'JULIAN EGON',
                      'KATHLEEN STRESSKLINIEK', 'Laura ESM 2014','Laura ESM 2016',
                      'LONGITUDINAL W1', 'LONGITUDINAL W2', 'LONGITUDINAL W3','MARLIES BPD',
                      'MTURK DAILY DIARY', 
                      'PETEMADDY')


outcomes <- data.frame(matrix(nrow = 0, ncol = 8))
failed.samples <- c()
# colnames(outcomes) <- list.of.names

t <- Sys.time()
for(sampled in sampled.names){
  load(paste(sampled.path, sampled, sep = ""))
  if(!isS4(to.be.saved$model$def.A)){
    failed.samples <- c(failed.samples, to.be.saved$name)
    next()
    }
  outcome.single <- to.be.saved %>% my.mirt.comparison()
  outcomes <- outcome.single %>% rbind(outcomes)
}
Sys.time() - t

# outcomes.p.all <- outcomes
# outcomes.r.randocc <- outcomes
# outcomes.r.sameocc <- outcomes
