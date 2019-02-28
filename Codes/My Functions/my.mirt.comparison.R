my.mirt.comparison <- function(sampled.mirt, criterion = "BIC"){
  
  model.fit <- sampled.mirt$model[-1]
  fit.temp <- model.fit %>% ldply(anova)
  fit.abs <- fit.temp[,-1] %>% abs()
  rownames(fit.abs) <- fit.temp[,1]
  rm(fit.temp)
  
  values <- fit.abs %>% select(criterion)
  m <- min(values)
  
  ind <- which(values == max(values), arr.ind=TRUE)
  best <- rownames(values)[ind[2]]
  
  return(best)
  
}