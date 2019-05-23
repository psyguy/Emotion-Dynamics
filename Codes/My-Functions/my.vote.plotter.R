my.vote.plotter <- function(f, criterion = "AICc", sample.name = ""){
  
  # criterion <- criterion.list[6]
  # sample.name <- technique
  ft <- f %>% group_by(`dataset name`, AICc) %>% dplyr::summarise(freq=n()) %>% arrange(`dataset name`)

  Frequency <- ft$freq
  Dataset <- ft$`dataset name`
  Model <- ft[,2] %>% t() %>% as.vector()

  png(paste0(sample.name, "-", criterion, ".png"))
  ggplot(ft,aes(x = Dataset, y = Frequency)) +
    geom_bar(aes(fill = Model),stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(paste("Majority voting for sample", sample.name,"by criterion",criterion))
  dev.off()

  
  png(paste0(sample.name, "-", criterion, ".barplot.png"))
  ggplot(ft,aes(x = Dataset, y = Frequency)) +
    geom_bar(aes(fill = Model),stat = "identity", position = position_dodge()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(paste("Majority voting for sample", sample.name,"by criterion",criterion))
  dev.off()

    
}
