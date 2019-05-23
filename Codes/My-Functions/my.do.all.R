my.do.all <- function(technique){
  
# reading appropriate samples ---------------------------------------------
  
  technique <- "r.sameocc"
  
  path.to.processed <- "correct_processed-457_May22/"
  
  if(technique=="p.all") sampled.path <- "p_all/"
  if(technique=="r.randocc") sampled.path <- "r_randocc/"
  if(technique=="r.sameocc") sampled.path <- "r_sameocc/"
  
  sampled.path <- paste0(path.to.processed, sampled.path)
  # sampled.names <- list.files(path = sampled.path, pattern = "*.RData")
  
  list.of.names <- list('CLINICAL ESM','ELISE ESM14', 'JULIAN EGON',
                        'KATHLEEN STRESSKLINIEK', 'Laura ESM 2014','Laura ESM 2016',
                        'LONGITUDINAL W1', 'LONGITUDINAL W2', 'LONGITUDINAL W3','MARLIES BPD',
                        'MTURK DAILY DIARY', 
                        'PETEMADDY')
  
  f <- sampled.path %>% my.fit.extractor() %>%
                        quick.correct(is.loading = FALSE)
  ll <- sampled.path %>% my.loading.extractor()# %>%
                        llply(quick.correct)

  
  
  
# plotting density heatmaps for all datastes -----------------------------------------------
  
  t <- Sys.time()
  for(i in 1:9){
    for(cluster.what in c("items", "datasets")){
      paste("Doing it for", names(l.w[i]), "for", cluster.what, "within") %>% print()
      l.w[i] %>% my.together.plotter(cluster.what, "within")
      paste("Doing it for", names(l.w[i]), "for", cluster.what, "between") %>% print()
      l.b[i] %>% my.together.plotter(cluster.what, "between")
    }
  }
  Sys.time() - t
  
  
  
}


# correcting column names of outcomes matrix ------------------------------
# 
# l.w <- l.w %>% llply(function(x){
#   colnames(x)[1:2] <- c("dataset name", "model definition")
#   return(x)})