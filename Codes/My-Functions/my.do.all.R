my.do.all <- function(technique){
  
# reading appropriate samples ---------------------------------------------
  
  # technique <- "r.sameocc"
  
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
  
  ## remove comments, it works
  f <- sampled.path %>% my.fit.extractor() %>%
                          quick.correct(is.loading = FALSE)
  l <- sampled.path %>% my.loading.extractor() %>%
                          llply(quick.correct, is.loading = TRUE) %>%
                          llply(my.loading.aligner)
  save(list = c("f","l"), file = paste0(technique,"_loadings.and.factors.RData"))

  
  
# plotting density heatmaps for all datastes -----------------------------------------------
  
  t <- Sys.time()
  for(i in 1:9){
    for(cluster.what in c("items")){#, "datasets")){
      paste("Plotting density heatmaps for", names(l[i]), "for", cluster.what, technique) %>% print()
      l[i] %>% my.together.plotter(cluster.what, technique)
      # paste("Doing it for", names(l.w[i]), "for", cluster.what, "between") %>% print()
      # l.b[i] %>% my.together.plotter(cluster.what, "between")
    }
  }
  (Sys.time() - t) %>% print()
  
  
  
  t <- Sys.time()
  for(name in list.of.names){
    l %>% l_ply(my.spaghetti.ploter, name, technique)
    print(paste("Spaghetti of", name, technique, "is plotted"))
  }
  (Sys.time() - t) %>% print()

}

# correcting column names of outcomes matrix ------------------------------
# 
# l.w <- l.w %>% llply(function(x){
#   colnames(x)[1:2] <- c("dataset name", "model definition")
#   return(x)})


# my.do.all("p.all")
my.do.all("r.randocc")
