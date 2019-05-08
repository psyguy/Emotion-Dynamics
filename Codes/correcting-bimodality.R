# rm(list=ls())
load("I:/Emotion Dynamics Clean/loadings.within.RData")
load("I:/Emotion Dynamics Clean/loadings.between.RData")
source("I:/Emotion Dynamics Clean/Codes/My-Functions/my.little.functions.R")

l.b <- loadings.between
l.w <- loadings.within

l.b <- l.b %>% llply(function(x){
  colnames(x)[1:2] <- c("dataset name", "model definition")
  return(x)})

l.w <- l.w %>% llply(function(x){
  colnames(x)[1:2] <- c("dataset name", "model definition")
  return(x)})

list.of.names <- list('CLINICAL ESM','Cogito','ELISE ESM14', 'JULIAN EGON',
                      'KATHLEEN STRESSKLINIEK', 'Laura ESM 2014','Laura ESM 2016',
                      'LONGITUDINAL W1', 'LONGITUDINAL W2', 'LONGITUDINAL W3','MARLIES BPD',
                      'MDD BPD TRULL', 'MDD GOTLIB', 'MTURK DAILY DIARY',
                      'PETEMADDY')

my.little.align <- function(d){
  
  items <- d$item %>% unique()
  factors <- colnames(d)[5:(ncol(d)-1)]
  d$seed <- d$seed %>% as.character() %>% as.numeric()
  new.fac.loadings.all <- c()
  for(n.f in 1:length(factors)){
    # d.new <- d %>% pull(f) %>% as.character() %>% as.numeric()
    f <- 4 + n.f
    new.fac.loadings <- c()
    for(s in 1:100){
      fac.loadings <- d %>% filter(seed == s) %>%
        pull(f) %>% as.character() %>% as.numeric()
      first.sign <- (fac.loadings[fac.loadings!=0])[1] %>% sign()
      fac.loadings <- fac.loadings*first.sign
      new.fac.loadings <- new.fac.loadings %>% c(fac.loadings)
    }
    # new.fac.loadings <- new.fac.loadings %>% cbind(fac.loadings)
    d[,f] <- new.fac.loadings
  }
  d %>% return()
}


my.loading.aligner <- function(l){
  
  # l <- loadings.list# <- loadings.within[[1]]
  
  names.datasets <- l$`dataset name` %>% unique() %>% as.character()
  # name.dataset <- names.datasets[[1]]
  
  d <- NULL
  for(name.dataset in names.datasets){
    dd <- l %>% filter(`dataset name` == name.dataset)
    d <- d %>% rbind(my.little.align(dd))
  }
  
  d %>% return()
  }

Sys.time()
corrected.loadings.within <- l.w %>% llply(my.loading.aligner)
corrected.loadings.between <- l.b %>% llply(my.loading.aligner)
Sys.time()

# save(corrected.loadings.within, file = "corrected.loadings.within.RData")
# save(corrected.loadings.between, file = "corrected.loadings.between.RData")
