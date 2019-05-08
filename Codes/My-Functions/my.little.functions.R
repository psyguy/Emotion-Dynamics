quick.correct <- function(x, dropped.data.set = "TGC"){
  colnames(x)[1:2] <- c("dataset name", "model definition")
  x$item <- x$item %>% toupper()
  x$item[x$item == "STRESS"] <- "STRESSED"
  x$item[x$item == "ANGER"] <- "ANGRY"
  if(dropped.data.set == "TGC"){
    dropped.data.set <- c('MDD BPD TRULL', 'MDD GOTLIB', 'Cogito')}
  if(!is.null(dropped.data.set)){
    x <- x %>% filter(!(`dataset name` %in% dropped.data.set))
    x$`dataset name` <- x$`dataset name` %>% as.character() %>% as.factor()
  }
  return(x)
}

