# rm(list=ls())

load("I:/Emotion Dynamics Clean/corrected.loadings.within.RData")
load("I:/Emotion Dynamics Clean/corrected.loadings.between.RData")

l.b <- corrected.loadings.between
l.w <- corrected.loadings.within

dropped.data.set <- c('MDD BPD TRULL', 'MDD GOTLIB', 'Cogito')

quick.correct <- function(x, dropped.data.set = NULL){
  x$item <- x$item %>% toupper()
  x$item[x$item == "STRESS"] <- "STRESSED"
  if(!is.null(dropped.data.set)){
    x <- x %>% filter(!(`dataset name` %in% dropped.data.set))
    x$`dataset name` <- x$`dataset name` %>% as.character() %>% as.factor()
  }
  return(x)
  }

l.b <- l.b %>% llply(quick.correct, dropped.data.set)
l.w <- l.w %>% llply(quick.correct, dropped.data.set)

# items.pos <- l.b$def.B.bare %>% filter(Pos!=0) %>% 
#   pull(item) %>% toupper() %>% unique()
# items.neg <- l.b$def.B.bare %>% filter(Neg!=0) %>% 
#   pull(item) %>% toupper() %>% unique()
# items.all <- items.pos %>% c(items.neg)

rm(list=setdiff(ls(), c("l.b","l.w")))


my.heatmap.plotter <- function(l, which = "factors", what = "mean"){
  l <- l.w[1]
  d <- l[[1]]
  
  items <- d$item %>% unique()
  datasets <- d$`dataset name` %>% as.character() %>% unique()
  factors <- colnames(d)[5:(ncol(d)-1)]
  d$seed <- d$seed %>% as.character() %>% as.numeric()
  
  loadings.and.h2 <- matrix(nrow = length(items), ncol = (length(factors)+1)) %>%
    as.data.frame()
  colnames(loadings.and.h2) <- c(factors, "h2")
  rownames(loadings.and.h2) <- items
  
  for(item in 1:length(factors)){
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
}