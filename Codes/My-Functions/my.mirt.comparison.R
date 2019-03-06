my.mirt.comparison <- function(sampled.mirt, criterion = "BIC"){
  
  model.fit <- sampled.mirt$model
  dataset <- sampled.mirt$name
  model.seed <- sampled.mirt$parameters$seed
  
  identifiers <- c(dataset, model.seed) %>% t() %>% as.data.frame()
  colnames(identifiers) <- c("dataset", "seed")
  
  fit.temp <- model.fit %>% ldply(anova)
  values <- fit.temp[,-1] %>% abs()
  rownames(values) <- fit.temp[,1]
  
  cn <- values %>% colnames()
  
  bests <- values %>% t() %>% as.data.frame() %>% 
    mutate(min.model = apply(., 1, function(x) names(x)[which.min(x)])) %>% t()
  
  colnames(bests) <- cn
  
  output <- bests["min.model",] %>% t() %>% as.data.frame()
  
  output <- cbind(identifiers, output)
  
  output %>% return()
  
}
