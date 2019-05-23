library(yarrr)

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

# 
# for(dataset in list.of.names){
#   print(dataset)
#   print("within")
#   corrected.loadings.within %>% l_ply(my.spaghetti.ploter, dataset, "C.W")
#   print("now between")
#   corrected.loadings.between %>% l_ply(my.spaghetti.ploter, dataset, "C.B")
# }
