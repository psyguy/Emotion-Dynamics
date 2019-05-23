my.vote.plotter <- function(outcomes, criterion = "AICc", sample.name = ""){
  
  # criterion <- criterion.list[6]
  sample.name <- technique
  ft <- outcomes %>% group_by_("dataset", criterion) %>% dplyr::summarise(freq=n())

  Frequency <- ft$freq
  Dataset <- ft$dataset
  Model <- ft[,2] %>% t() %>% as.vector()

  png(paste0(sample.name, "-", criterion, ".png"))
  ggplot(ft,aes(x = Dataset, y = Frequency)) +
    geom_bar(aes(fill = Model),stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(paste("Majority voting for sample", sample.name,"by criterion",criterion))
  dev.off()

  
  # png(paste0(sample.name, "-", criterion, ".barplot.png"))
  # ggplot(ft,aes(x = Dataset, y = Frequency)) + 
  #   geom_bar(aes(fill = Model),stat = "identity", position = position_dodge()) +
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #   ggtitle(paste("Majority voting for sample", sample.name,"by criterion",criterion))
  # dev.off()

    
}
