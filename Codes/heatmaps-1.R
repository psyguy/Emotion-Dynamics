# source("https://bioconductor.org/biocLite.R")
# biocLite("ComplexHeatmap")

# library(devtools)
# install_github("jokergoo/ComplexHeatmap")
library(ComplexHeatmap)

# rm(list=ls())

load("I:/Emotion Dynamics Clean/corrected.loadings.within.RData")
load("I:/Emotion Dynamics Clean/corrected.loadings.between.RData")
source("I:/Emotion Dynamics Clean/Codes/My-Functions/my.little.functions.R")

l.b <- corrected.loadings.between #corrected.loadings.between
l.w <- corrected.loadings.within #corrected.loadings.within

dropped.data.set <- c('MDD BPD TRULL', 'MDD GOTLIB', 'Cogito')


l.b <- l.b %>% llply(quick.correct)
l.w <- l.w %>% llply(quick.correct)

# items.pos <- l.b$def.B.bare %>% filter(Pos!=0) %>% 
#   pull(item) %>% toupper() %>% unique()
# items.neg <- l.b$def.B.bare %>% filter(Neg!=0) %>% 
#   pull(item) %>% toupper() %>% unique()
# items.all <- items.pos %>% c(items.neg)

rm(list=setdiff(ls(), c("l.b","l.w")))


my.heatmap.plotter <- function(l, which = "factors", what = "mean"){
  l <- l.b[1]
  d <- l[[1]]
  
  items <- d$item %>% unique()
  items.original <- items
  items.prefixed <- c(rep("p.",3), rep("n.",6), rep("p.",4)) %>% paste0(items.original)
  lookup.table <- cbind(items.original, items.prefixed) %>% as.data.frame()
  # 
  di <- d$item %>% as.character()
  # di <- "p." %>% paste0(di)
  
  for(i in 1:length(di)){
    for(l in 1:nrow(lookup.table)){
      if(di[i]==lookup.table[l,1]) di[i] <- lookup.table[l,2] %>% as.character()
    }
  }
  
  # di[di %in% lookup.table$items.original == TRUE] <-
  #   items.prefixed[lookup.table$items.original %in% di == TRUE]
  # 
  d$item <- di# %>% as.factor()
  
  datasets <- d$`dataset name` %>% as.character() %>% unique()
  factors <- colnames(d)[5:(ncol(d)-1)]
  d$seed <- d$seed %>% as.character() %>% as.numeric()
  d$`dataset name` <- d$`dataset name` %>% as.character()
  d$item <- d$item %>% as.character()
  # loadings.and.h2 <- matrix(nrow = length(items), ncol = (length(factors)+1)) %>%
  #   as.data.frame()
  # colnames(loadings.and.h2) <- c(factors, "h2")
  # rownames(loadings.and.h2) <- items
  
  
  # d.tmp <- d #%>% filter(seed == 1)
  # nums <- c()
  # for(i in items){
  #   nums.tmp <- d.tmp %>% filter(item == i) %>% nrow()# pull(item) %>% length()
  #   nums <- nums %>% c(nums.tmp)
  # }
  # 
  # labels <- rep(items,nums)
  
  d$Communal <- d$Communal %>% as.character() %>% as.numeric()
  d$h2 <- d$h2 %>% as.character() %>% as.numeric()
  d.long <- d %>% mutate(itemXdataset = paste(item, `dataset name`, sep = ".")) %>%
    select(itemXdataset, seed, Communal)
  
  d.wide <- d.long %>% spread(itemXdataset, Communal) %>% select(-seed)
  
  # i <- d.long$itemXdataset[12]
  # x <- d.long %>% filter(itemXdataset==i)
  
  labels <- d.wide %>% colnames() %>% strsplit("\\.") %>% llply(function(x) x[2]) %>% unlist
  
  # for(i in d.long$itemXdataset){
  #   
  #  }
  
  d.wide %>% densityHeatmap()#(anno = labels)
  
  
  for(item in 1:length(factors)){
    # d.new <- d %>% pull(f) %>% as.character() %>% as.numeric()
    f <- 4 + n.f
    new.fac.loadings <- c()
    for(s in 1:100){
      fac.loadings <- d %>% filter(seed == s) %>%
        pull(f) %>% as.character() %>% as.numeric()
      first.sign <- (fac.loadings[fac.loadings!=0])[1] %>% sign()
      fac.loadings <- fac.loadings*first.sign
      new.fac.loadings <- new.fac.loadings %>% c(fac.loadings)
    }
    # new.fac.loadings <- new.fac.loadings %>% cbind(fac.loadings)
    d[,f] <- new.fac.loadings
  } 
}


# Complex heatmap examples ----------------------------------------------------------

df <- scale(mtcars)
data("mtcars")
heatmap(df, scale = "none")
densityHeatmap(scale(mtcars), range = c(0,1))




set.seed(123)
m = cbind(matrix(rnorm(10*100), ncol = 10),
          matrix(runif(10*100, min = -2, max = 2) + 0.5, ncol = 10))
colnames(m) = paste0("C", 1:ncol(m))
densityHeatmap(m)



ha1 = HeatmapAnnotation(dist = c(rep("rnorm", 10), rep("runif", 10)))
ha2 = HeatmapAnnotation(foo = anno_points(rnorm(20)))
densityHeatmap(m, top_annotation = ha1)#, bottom_annotation = ha2)



matrix = matrix(rnorm(100), 10); colnames(matrix) = letters[1:10]
densityHeatmap(matrix)
densityHeatmap(matrix, anno = rep(c("A", "B"), each = 5))
densityHeatmap(matrix, col = c("white", "red"), anno = rep(c("A", "B"), each = 5))

ha = HeatmapAnnotation(points = anno_points(runif(10)),
                       anno = rep(c("A", "B"), each = 5), col = list(anno = c("A" = "red", "B" = "blue")))
densityHeatmap(matrix, anno = ha)

lt = list(rnorm(10), rnorm(10))
densityHeatmap(lt)




geneplotter::groupedHeatmap(
  scale(mtcars),
  frow = factor(sapply(strsplit(rownames(mtcars), " "), "[", 1)),
  fcol = factor(round(seq_len(ncol(mtcars))/3)))




