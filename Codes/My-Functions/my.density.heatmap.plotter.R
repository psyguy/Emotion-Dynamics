library(gridExtra)

my.density.heatmap.plotter <- function(l, which = "h2", cluster.what = c("items", "datasets")){

# debugging inits ---------------------------------------------------------
  
  # l <- l[2]
  # which <- "Pos"
  cluster.what <- "items" # "datasets"
  
# start of the function ---------------------------------------------------
  
  d <- l[[1]]
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

  empty.wide <- matrix(-10, nrow = 100, ncol = length(empty.cols)) %>%
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
  
  d.wide[is.na(d.wide)] <- -10
  
# plotting the density heatmap ------------------------------------------
  
  labels <- d.wide %>% colnames() %>% 
    strsplit("\\.") %>% llply(my.label.extractor) %>% unlist()
  
  hm.height <- 600
  hm.width <- 1500
  if(which == "h2") hm.height <- hm.height/2
  
  # file.name <- paste(which, "png", sep = ".")
  
  file.name <- paste(names(l), which, cluster.what, "png", sep = ".")
  file.name %>% png(width = hm.width, height = hm.height)
  
  hm <- d.wide %>% densityHeatmap(show_column_names = FALSE,
                            ylim = ylim,
                            column_gap = unit(2, "mm"),
                            border = T,
                            ylab = which,
                            col = (RColorBrewer::brewer.pal(9, "Blues")),
                            column_title = NULL,# "",# paste0("Density heatmap of ", which),
                            show_column_dend = FALSE,
                            column_split = labels,
                            show_quantiles = FALSE)
  print(hm)
  dev.off()
}


my.together.plotter <- function(l, cluster.what = "items", wbp = "within"){
# debugging inits ---------------------------------------------------------
  
  # l <- l.b[3]
  # which <- "Pos"
  cluster.what <- "items"

# start of the function ---------------------------------------------------

  model.name <- l %>% names()
  to.plot <- colnames(l[[1]])[-1:-4] %>% as.list()
  

# making distribution density heatmaps ------------------------------------

  to.plot %>% l_ply(function(x) my.density.heatmap.plotter(l, x, cluster.what))
  
  list.of.pngs <- to.plot %>% unlist() %>% c(cluster.what) %>% paste0(".png")



# making title and file name ----------------------------------------------

  file.name.together <- paste(model.name, wbp, cluster.what, "png", sep = ".")
  plot.title <- paste(
    # "Distribution of loadings and communalities of model",
    "Distributions for model fit of",
    model.name,
    # "on the \n",
    "analyzed with \n",
    wbp,
    # "person sample",
    "technique,",
    "clustered by",
    cluster.what
  )
  
# reading the saved plots and putting them together -----------------------

  plots <- list.of.pngs %>% 
    lapply(function(x){
      img <- as.raster(png::readPNG(x))
      grid::rasterGrob(img, interpolate = FALSE)
    })
  
  # if(cluster.what == "items") last.hight <- 232
  # if(cluster.what == "datasets") last.hight <- 390
  
  g <- gridExtra::marrangeGrob(grobs = plots,
                    nrow = length(list.of.pngs),
                    ncol = 1,
                    heights = c(rep(600,length(to.plot)-1),300,232),
                    widths = c(1500),
                    top = paste0("\n", plot.title),
                    padding = unit(5, "mm"))
  file.name.together %>% ggplot2::ggsave(g)
  
}



# graveyard ---------------------------------------------------------------

# quick.func <- function(l, cluster.what, wbp){
#   # if(!is.list(l)) l <- l %>% as.list()
#   l %>% as.list() %>% my.together.plotter(cluster.what, wbp)
# }
# 
# Sys.time()
# for(clustered.by in c("items","datasets")){
#   for(wbp in c("within","between")){
#     paste("within", clustered.by) %>% print()
#     l.w %>% l_ply(quick.func, clustered.by, "within")
#     paste("between", clustered.by) %>% print()
#     l.b %>% l_ply(quick.func, clustered.by, "between")
#     }
# }
# Sys.time()
