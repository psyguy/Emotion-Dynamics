# library
library(ggplot2)

# create a dataset
specie=c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition=rep(c("normal" , "stress" , "Nitrogen") , 4)
value=abs(rnorm(12 , 0 , 15))
data=data.frame(specie,condition,value)

# Grouped
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")

# Stacked
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar( stat="identity")


# Stacked Percent
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar( stat="identity", position="fill")


ggplot(data=dfm, aes(x=specie, y=value, fill=condition)) +
  geom_bar(position="dodge", stat="identity")


tbl <- with(mydata, table(Species, Depth))

ggplot(dfm,aes(x = Input,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + 
  scale_y_log10()

o <- outcomes
library(ggplot2)
ggplot(o, aes(factor(model.name), Freq, fill = AIC)) +     
  geom_col(position = 'dodge')


food <- data.frame(Condition = c("A", "B", "A", "B", "A"), Stars=c('good','meh','meh','meh','good'))
library(ggplot2)
library(dplyr)
data <- food %>% group_by(Stars,Condition) %>% summarize(n=n()) %>% mutate(freq=n/sum(n)) 

ggplot(data, aes(x=Stars, fill = Condition, group = Condition)) + geom_bar(aes(y=freq), stat="identity", position = "dodge")

# filter(AIC != "def.E.bare")


m <- to.be.saved$model$def.C.cov
m %>% itemplot(c(1:3), "info")#item = c(1:3), type = "SE")




# May 8 -------------------------------------------------------------------

# old crap plotting them together --------------------------------------------------



plots <- list("Pos.png", "h2.png", "Neg.png", "inb.png") %>% 
  lapply(function(x){
    img <- as.raster(readPNG(x))
    grid::rasterGrob(img, interpolate = FALSE)
  })

c.name <- paste0("again",".png")
g <- marrangeGrob(grobs=plots, nrow=4, ncol=1,
                  heights = c(600,300,600,145),
                  widths = c(1500),#,1500,1500), 
                  top = paste0("\n","meh"),
                  padding = unit(.5, "line"))
c.name %>% ggplot2::ggsave(g)


pdf(file="meh.pdf")
par(mfrow=c(3,1))
readPNG("Pos.png")
readPNG("h2.png")
readPNG("Neg.png")
dev.off() 



# writing names -----------------------------------------------------------
# It's so hard that I give up! Will add the titles manually!

items.sorted <- c("Angry", "Anxious", "Depressed", "Restless",
                  "Sad", "Stressed", "Calm", "Cheerful",
                  "Content", "Euphoric", "Excited", "Happy",
                  "Relaxed")
# loci <- ()

# zx <- {grid.text("vv", 3/15, 6/10, default.units = "npc")
#   grid.text("vv", 3.75/15, 6/10, default.units = "npc")}







