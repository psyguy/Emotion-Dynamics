library(yarrr)

my.loadings.plotter <- function(d, name.dataset, wbp){
  
  # loadings.list <- loadings
  # loading.element <- loadings.list[[5]] -> d
  
  colnames(d)[1:2] <- c("dataset name", "model definition")
  d <- d %>% filter(`dataset name` == name.dataset)#"MDD BPD TRULL")#|`dataset name` =="MDD BPD TRULL")
  d.numeric <- d[,-1:-4] %>%
    lapply(function(x) as.numeric(as.character(x)))
  d <- d[,1:4] %>% cbind(d.numeric)

  # par(las=2, mfrow = c(1,((ncol(d)-4))))
  
  for(i in 5:(ncol(d))){
    
    name.factor <- colnames(d)[i]
    def <- as.character(d$`model definition`[1])
    name.dataset <- as.character(d$`dataset name`[1])
    
    dd <- d %>% select(item, i)
    colnames(dd) <- c("item", "factor")
    plot.title <- paste("Laodings of",
                        def,
                        "for dataset",
                        name.dataset
                        )
    
    ylab <- paste("Factor loading of", name.factor)
    ylim <- c(-1,1)
    threshold <- c(0.6,-0.6)
    if(name.factor=="h2"){
      ylab <- paste("Communality of items")
      ylim <- c(0,1)
      threshold <- 0.6}

    paste(wbp, name.dataset, def, name.factor, "png", sep = ".") %>% png()
    par(las=2)
    pirateplot(formula = factor ~ item,
               main = plot.title,
               data = dd,           
               ylim = ylim,
               ylab = ylab,
               xlab = "",
               inf.method = 'sd',
               avg.line.fun = mean,
               theme = 3,
               inf.disp = "line" # or "line"
               )
    abline(h = threshold, col = "red", lty=2, lwd=3)
    dev.off()
  }
  
}


for(dataset in list.of.names){
  print(dataset)
  print("within")
  loadings.within %>% l_ply(my.loadings.plotter, dataset, "W")
  print("now between")
  loadings.between %>% l_ply(my.loadings.plotter, dataset, "B")
}

