library(yarrr)

my.loadings.plotter <- function(loadings.list){
  
  loadings.list <- loadings
  loading.element <- loadings.list[[1]] -> d
  
  
  colnames(d)[1:2] <- c("dataset name", "model definition")
  d <- d %>% filter(`dataset name` == "CLINICAL ESM")#|`dataset name` =="MDD BPD TRULL")
  d[,5] <- d[,5] %>% as.character %>%  as.numeric
  d[,-1:-4] %>% head()
  
  # filter(name.dataset == "CLINICAL ESM")# %>% 
  
  pirateplot(formula = weight ~ Time, # dv is weight, iv is Diet
             data = ChickWeight,
             main = "Pirateplot of chicken weights",
             xlab = "Diet",
             ylab = "Weight")
  
  par(las=2)
  
  pirateplot(formula = h2 ~ item,
             data = d,           
             # theme = 3,
             main = "Pirate Heights",
             xlab = "")#,

# pull(h2) %>% as.numeric()# %>% hist()
  # 
  # ft <- loading.element %>% group_by_("name.dataset") %>% dplyr::summarise(freq=n())
  # 
  # Frequency <- ft$freq
  #  <- ft$dataset
  # Model <- ft[,2] %>% t() %>% as.vector()
  
  # ggplot(ft,aes(x = , y = Frequency)) + 
  #   geom_bar(aes(fill = item),stat = "identity", position = position_dodge()) +
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #   ggtitle(paste("Majority voting for sample", sample.name,"by criterion",criterion))
  
  
  
}


