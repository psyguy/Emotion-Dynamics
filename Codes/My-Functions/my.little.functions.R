# extract fit measures from the folder of processed  --------

my.fit.extractor <- function(sampled.path,
                             # sampled.names,
                         return.failed = FALSE){
  
  sampled.names <- list.files(path = sampled.path, pattern = "*.RData")
  
  outcomes <- data.frame(matrix(nrow = 0, ncol = 8))
  failed.samples <- c()
  
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
  (Sys.time() - t) %>% print()
  
  return(outcomes)
}

# extract loadings/fit measures/etc. from the folder of processed  --------

my.loading.extractor <- function(sampled.path,
                             # sampled.names,
                             return.failed = FALSE){

  sampled.names <- list.files(path = sampled.path, pattern = "*.RData")# %>% head(100)

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
  
  def.colnames <- list(def.A = c("Communal", "h2"),
                       def.B.bare = c("Pos", "Neg", "h2"),
                       def.B.cov = c("Pos", "Neg", "h2"),
                       def.C.bare = c("Communal", "Pos", "Neg", "h2"),
                       def.C.cov = c("Communal", "Pos", "Neg", "h2"),
                       def.D.bare = c("F1", "F2", "h2"),
                       def.D.cov = c("F1", "F2", "h2"),
                       def.E.bare = c("F1", "F2", "F3", "h2"),
                       def.E.cov = c("F1", "F2", "F3", "h2")
                       )
  
  loadings <- def.colnames %>% llply(function(x) c())
  
  extract.append <- function(prev.loadings, model,
                             name.model, name.dataset, seed){
    
    ls.and.h2 <- model@Fit$F %>% cbind(model@Fit$h2)
    colnames(ls.and.h2)[ncol(ls.and.h2)] <- "h2"
    
    name.model <- name.model %>% rep(nrow(ls.and.h2))
    item <- rownames(ls.and.h2)
    seed <- seed %>% rep(nrow(ls.and.h2))
    
    name.dataset <- name.dataset %>% rep(nrow(ls.and.h2))
    
    single.case <- cbind(name.dataset, name.model, item, seed, ls.and.h2)
    rownames(single.case) <- c()
    
    rownames(prev.loadings) <- c()
    out <- rbind(prev.loadings, single.case)
    rownames(out) <- c()
    out %>% return()
  }
  
  make.loadings <- function(num.cols, name.model, m){
    j <- 1
    loadings.temp <- matrix(nrow = 0, ncol = (4+num.cols)) %>% data.frame()
    
    for(sampled in sampled.names){
      
      load(paste(sampled.path, sampled, sep = ""))
      name.dataset <- to.be.saved$name
      
      if(!isS4(to.be.saved$model$def.A)) next()
      
      model <- to.be.saved$model[[m]]
      seed <- to.be.saved$parameters$seed
      print(paste("definition", m, "sample", j))
      
      j <- j + 1
      
      loadings.temp <- extract.append(prev.loadings = loadings.temp,
                                      model = model,
                                      name.model = name.model,
                                      name.dataset = name.dataset,
                                      seed)
    }
    loadings.temp %>% return()
  }
  
  
  t <- Sys.time()
  for(m in 1:length(def.colnames)){
    name.model <- names(def.colnames)[m]
    num.cols <- def.colnames[[m]] %>% length()
    
    loadings[[m]] <- make.loadings(num.cols, name.model, m)
  }
  (Sys.time() - t) %>% print()
  
  return(loadings)
}


# correcting colnames, item names, dropping the three datasets ------------
# correcting col names, item names, removing the 3 datasets ---------------

quick.correct <- function(x, is.loading = FALSE, dropped.data.set = "TGC"){
  colnames(x)[1:2] <- c("dataset name", "model definition")
  if(is.loading){
    x$item <- x$item %>% toupper()
    x$item[x$item == "STRESS"] <- "STRESSED"
    x$item[x$item == "ANGER"] <- "ANGRY"
    x[x == 0] <- -10
    }
  
  if(dropped.data.set == "TGC"){
    dropped.data.set <- c('MDD BPD TRULL', 'MDD GOTLIB', 'Cogito')
    }
  if(!is.null(dropped.data.set)){
    x <- x %>% filter(!(`dataset name` %in% dropped.data.set))
    x$`dataset name` <- x$`dataset name` %>% as.character() %>% as.factor()
  }
  return(x)
}


# spaghetti plot diagnosis ------------------------------------------------

my.spaghetti.ploter <- function(d, name.dataset, wbp){
  
  # debugging ---------------------------------------------------------------
  
  # d <- l$def.B.bare
  # name.dataset <- "PETEMADDY"# list.of.name
  
  # real thing --------------------------------------------------------------
  
  colnames(d)[1:2] <- c("dataset name", "model definition")
  d <- d %>% filter(`dataset name` == name.dataset)
  d.numeric <- d[,-1:-4] %>%
    lapply(function(x) as.numeric(as.character(x)))
  d <- d[,1:4] %>% cbind(d.numeric)
  
  for(i in 5:(ncol(d)-1)){
    
    name.factor <- colnames(d)[i]
    def <- as.character(d$`model definition`[1])
    
    dd <- d %>% select(item, seed, (i)) # %>% filter(item %in% c("Happy", "Angry", "Sad"))
    colnames(dd) <- c("item", "seed", "factor")
    dd$factor <- dd$factor %>% as.character() %>% as.numeric()
    
    d.wide <- dd %>% spread(item, factor) %>% select(-seed)
    d.wide[is.na(d.wide)] <- 0
    
    plot.title <- paste("Laodings of",
                        name.factor,
                        def,
                        "for dataset",
                        name.dataset
    )
    
    ylab <- paste("Factor loading of", name.factor)
    ylim <- c(-1,1)
    threshold <- c(0.6,-0.6)
    
    g <- GGally::ggparcoord(d.wide,
                            title = plot.title,
                            # columns = c(1:8),
                            # groupColumn = '',
                            scale = 'globalminmax')
    
    paste(wbp, name.dataset, def, name.factor, "png", sep = ".") %>% png()
    print(g)
    dev.off()
  }
  
}


# correcting bimodality ---------------------------------------------------

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


