library(yarrr)

my.loadings.plotter <- function(d, name.dataset, wbp){
  
  # loadings.list <- loadings
  # loading.element <- loadings.list[[5]] -> d
  
  # d <- loadings.between$def.A
  # name.dataset <- list.of.names[[1]]
  d <- l.b$def.A
  
  colnames(d)[1:2] <- c("dataset name", "model definition")
  # d <- d %>% filter(`dataset name` == name.dataset)#"MDD BPD TRULL")#|`dataset name` =="MDD BPD TRULL")
  d <- d %>% filter(item == 'RELAXED')#"MDD BPD TRULL")#|`dataset name` =="MDD BPD TRULL")
  # d <- d %>% filter(`dataset name` =="CLINICAL ESM")#|`dataset name` =="PETEMADDY")
  d.numeric <- d[,-1:-4] %>%
    lapply(function(x) as.numeric(as.character(x)))
  d <- d[,1:4] %>% cbind(d.numeric)

  # par(las=2, mfrow = c(1,((ncol(d)-4))))
  
  for(i in 5:(ncol(d))){
    
    name.factor <- colnames(d)[i]
    def <- as.character(d$`model definition`[1])
    # name.dataset <- as.character(d$`dataset name`[1])
    
    dd <- d %>% select(item, `dataset name`, (i)) # %>% filter(item %in% c("Happy", "Angry", "Sad"))
    colnames(dd) <- c("item", 'dataset name', "factor")
    dd$factor <- dd$factor %>% as.character() %>% as.numeric()
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
    pirateplot(formula = factor ~ `dataset name`,
               main = plot.title,
               cap.beans = F,
               width.max = .21,
               data = dd,           
               ylim = ylim,
               pal = gray(.1), # Dark gray palette
               point.pch = 16,  # Point specifications...
               point.col = "blue",
               point.bg = "red",
               ylab = ylab,
               xlab = "",
               inf.method = 'sd',
               avg.line.fun = mean,
               point.o = .81, # Point opacity
               bean.f.o = 0*.2, # Turn down bean filling
               inf.f.o = 0*.8, # Turn up inf fillingtheme = 2,
               inf.disp = "line" # or "line"
               )
    # abline(h = threshold, col = "red", lty=2, lwd=3)
    dev.off()
  }
  
}

# 
# for(dataset in list.of.names){
#   print(dataset)
#   print("within")
#   corrected.loadings.within %>% l_ply(my.loadings.plotter, dataset, "C.W")
#   print("now between")
#   corrected.loadings.between %>% l_ply(my.loadings.plotter, dataset, "C.B")
# }

