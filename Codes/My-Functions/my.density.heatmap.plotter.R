library(gridExtra)

my.density.heatmap.plotter <- function(l, which = "h2", cluster.what = c("items", "datasets")){
  # l <- l.b[1]
  d <- l[[1]]
  # which <- "Communal"
  ylim <- c(-1,1)
  if(which == "h2") ylim <- c(0,1)
  

# getting rid of unwanted stuff -------------------------------------------

  d <- d %>% select(`dataset name`, item, seed, which)
  colnames(d)[4] <- "to.be.plotted"
  

# extracting vectors needed later -----------------------------------------
  
  datasets <- d$`dataset name` %>% as.character() %>% unique()
  d$seed <- d$seed %>% as.character() %>% as.numeric()
  d$`dataset name` <- d$`dataset name` %>% as.character()
  # factors <- colnames(d)[5:(ncol(d)-1)]
  items.d <- d$item
  items <- items.d %>% unique() -> items.original
  items.prefixed <- c(rep("p.",3), rep("n.",6), rep("p.",4)) %>% paste0(items.original)
  lookup.table <- cbind(items.original, items.prefixed) %>% as.data.frame()
  

# adding prefixes to items ------------------------------------------------
  
  for(i in 1:length(items.d)){
    for(k in 1:nrow(lookup.table)){
      if(items.d[i]==lookup.table[k,1]) items.d[i] <- lookup.table[k,2] %>% as.character()
    }
  }
  d$item <- items.d
  

# clustering appropriately ------------------------------------------------

  if(cluster.what== "items"){
    d.long <- d %>%
      mutate(itemXdataset = paste(item, `dataset name`, sep = ".")) %>%
      select(itemXdataset, seed, to.be.plotted)
    
    itemXdataset <- d.long$itemXdataset %>% as.character() %>% unique()
    all.itemXall.datasets = paste(rep(items.prefixed,12), rep(datasets,13), sep=".")
    empty.cols <- setdiff(all.itemXall.datasets, itemXdataset)
    my.label.extractor <- function(x) paste(x[1],x[2], sep=".")
    }
  
  if(cluster.what == "datasets"){
    d.long <- d %>%
      mutate(itemXdataset = paste(`dataset name`, item, sep = ".")) %>%
      select(itemXdataset, seed, to.be.plotted)
  
    itemXdataset <- d.long$itemXdataset %>% as.character() %>% unique()
    all.itemXall.datasets = paste(rep(datasets,13), rep(items.prefixed,12), sep=".")
    empty.cols <- setdiff(all.itemXall.datasets, itemXdataset)
    my.label.extractor <- function(x) paste(x[1], sep=".")
  }
  

# building the d.long matrix and labels -----------------------------------

  empty.wide <- matrix(-10, nrow = nrow(d.wide), ncol = length(empty.cols)) %>%
                  as.data.frame()
  empty.wide <- c(1:100) %>% cbind(empty.wide)
  colnames(empty.wide) <- c("seed",empty.cols)
  
  empty.long <- empty.wide %>% 
    gather(itemXdataset, to.be.plotted, 2:69) %>% 
    select(colnames(d.long))
  empty.long$to.be.plotted <- empty.long$to.be.plotted %>% as.character()
  d.long <- d.long %>% rbind(empty.long) %>% arrange(itemXdataset)
  d.long$to.be.plotted <- d.long$to.be.plotted %>% as.character() %>% as.numeric()
  
  d.wide <- d.long %>% spread(itemXdataset, to.be.plotted) %>% select(-seed)
  
  
  labels <- d.wide %>% colnames() %>% 
    strsplit("\\.") %>% llply(my.label.extractor) %>% unlist()
  

# plotting the density histogram ------------------------------------------

  # ha1 = HeatmapAnnotation(foo=labels,
  #                         show_legend=2)
  #dist = c(rep("rnorm", 10), rep("runif", 10)))
  # ha1 = HeatmapAnnotation(dist = labels,
  #                         show_annotation_name=F)
  # HeatmapAnnotation(foo = anno_block(gp = gpar(fill = 1:length(unique(labels))),
 
  hm.height <- 500
  if(which == "h2") hm.height <- hm.height/2
  
  file.name <- paste(names(l), which, cluster.what, "png", sep = ".")
  file.name %>% png(width = 1000, height = hm.height)
  hm <- d.wide %>% densityHeatmap(show_column_names = F,
                            ylim = ylim,
                            column_gap = unit(1, "mm"),
                            ylab = "loading",
                            column_title = paste0("Density heatmap of ", which),
                            show_column_dend = F,
                            column_split = labels,
                            show_quantiles = F)
  print(hm)
  # ggplot2::ggsave(file.name,hm)
  dev.off()
}

plots <- list("def.A.Communal.items.png", "def.A.Communal.items.png") %>% 
  lapply(function(x){
    img <- as.raster(readPNG(x))
    grid::rasterGrob(img, interpolate = FALSE)
  })

l.w[1] %>% my.density.heatmap.plotter("Communal")
l.w[1] %>% my.density.heatmap.plotter("h2")

plots <- list(heat.com,heat.h2)
c.name <- paste0("cssc"," curves ","item.name",".png")
c.name %>% ggplot2::ggsave(gridExtra::marrangeGrob(grobs=plots, nrow=2, ncol=1,
                                                   top = paste0("\n","heyhey")))



# writing names -----------------------------------------------------------
# It's so hard that I give up! Will add the titles manually!

items.sorted <- c("Angry", "Anxious", "Depressed", "Restless",
                  "Sad", "Stressed", "Calm", "Cheerful",
                  "Content", "Euphoric", "Excited", "Happy",
                  "Relaxed")
# loci <- ()

# zx <- {grid.text("vv", 3/15, 6/10, default.units = "npc")
#   grid.text("vv", 3.75/15, 6/10, default.units = "npc")}
  

