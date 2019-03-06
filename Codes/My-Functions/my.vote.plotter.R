my.vote.plotter <- function(outcomes, criterion = "AICc"){
  
  ft <- outcomes %>% group_by_("dataset", criterion) %>% dplyr::summarise(freq=n())

  Frequency <- ft$freq
  Dataset <- ft$model.name
  Model <- ft[,2] %>% t() %>% as.vector()

  ggplot(ft,aes(x = Dataset, y = Frequency)) + 
    geom_bar(aes(fill = Model),stat = "identity")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
}