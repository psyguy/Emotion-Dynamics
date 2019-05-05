library(yarrr)

my.loadings.plotter <- function(d, name.dataset, wbp){
  
  # loadings.list <- loadings
  # loading.element <- loadings.list[[5]] -> d
  # 
  # d <- loadings.between$def.B.cov
  # name.dataset <- list.of.names[[1]]
  
  colnames(d)[1:2] <- c("dataset name", "model definition")
  # d <- d %>% filter(`dataset name` == name.dataset)#"MDD BPD TRULL")#|`dataset name` =="MDD BPD TRULL")
  d <- d %>% filter(`dataset name` == name.dataset)#"CLINICAL ESM")#|`dataset name` =="PETEMADDY")
  d.numeric <- d[,-1:-4] %>%
    lapply(function(x) as.numeric(as.character(x)))
  d <- d[,1:4] %>% cbind(d.numeric)
  
  # par(las=2, mfrow = c(1,((ncol(d)-4))))
  
  for(i in 5:(ncol(d)-1)){
    
    name.factor <- colnames(d)[i]
    def <- as.character(d$`model definition`[1])
    # name.dataset <- as.character(d$`dataset name`[1])
    
    dd <- d %>% select(item, seed, (i)) # %>% filter(item %in% c("Happy", "Angry", "Sad"))
    colnames(dd) <- c("item", "seed", "factor")
    dd$factor <- dd$factor %>% as.character() %>% as.numeric()
    
    d.wide <- dd %>% spread(item, factor) %>% select(-seed)
    
    
   
    
    
    plot.title <- paste("Laodings of",
                        factor,
                        def,
                        "for dataset",
                        name.dataset
    )
    
    ylab <- paste("Factor loading of", name.factor)
    ylim <- c(-1,1)
    threshold <- c(0.6,-0.6)
    
    # paste(wbp, name.dataset, def, name.factor, "png", sep = ".") %>% png()
    g <- ggparcoord(d.wide,
               title = plot.title,
               # columns = c(1:8),
               # groupColumn = '',
               scale = 'globalminmax')
    # dev.off()
    
    # if(name.factor=="h2"){
    #   ylab <- paste("Communality of items")
    #   ylim <- c(0,1)
    #   threshold <- 0.6}
    
    # paste(wbp, name.dataset, def, name.factor, "png", sep = ".") %>% png()
    # par(las=1)
    # pirateplot(formula = factor ~ `dataset name` + item,
    #            main = plot.title,
    #            cap.beans = F,
    #            width.max = .1,
    #            data = dd,           
    #            ylim = ylim,
    #            ylab = ylab,
    #            xlab = "",
    #            inf.method = 'sd',
    #            avg.line.fun = mean,
    #            theme = 1,
    #            inf.disp = "line" # or "line"
    # )
    # abline(h = threshold, col = "red", lty=2, lwd=3)
    # dev.off()
    print(g)
  }
  
}


for(dataset in list.of.names){
  print(dataset)
  print("within")
  loadings.within %>% l_ply(my.loadings.plotter, dataset, "W")
  print("now between")
  loadings.between %>% l_ply(my.loadings.plotter, dataset, "B")
}
