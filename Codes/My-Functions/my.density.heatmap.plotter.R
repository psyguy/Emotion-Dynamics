my.density.heatmap.plotter <- function(l, which = "h2", cluster.what = c("items", "datasets")){
  l <- l.b[1]
  d <- l[[1]]
  which <- "Communal"
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
  
  d.long <- d.long %>% rbind(empty.long) %>% arrange(itemXdataset)
  
  d.wide <- d.long %>% spread(itemXdataset, to.be.plotted) %>% select(-seed)
  
  
  labels <- d.wide %>% colnames() %>% 
    strsplit("\\.") %>% llply(my.label.extractor) %>% unlist()
  

# plotting the density histogram ------------------------------------------

  d.wide %>% densityHeatmap(show_column_names = F,
                            show_quantiles = F,
                            ylim = c(-1,1),
                            # cluster_columns = T,
                            # top_annotation = labels,
                            column_split = labels)#(anno = labels)
  
  
  
}
