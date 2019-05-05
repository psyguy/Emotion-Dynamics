install.packages("freqparcoord")
install.packages("discretize")
library(freqparcoord)
library(discretize)

data(mlb)
# extract height, weight, age
m <- mlb[,4:6]
m %>% head()
# ordinary parallel coordinates 
library(MASS) 
parcoord(m) 


d.wide <- d %>% select(-`dataset name`, -`model definition`, - Communal) %>% spread(item, h2)# %>% 
  dplyr::select(-seed)
# d.wide <- d.wide %>%
#   lapply(function(x) as.numeric(as.character(x)))
d.wide %>% parcoord()

data(prgeng)
pe <- prgeng[,c(1,3,5,7:9)]
pe25 <- pe[pe$wageinc < 250000,]

pe25 <- makeFactor(pe25,c('educ','occ','sex'))
pe25disc <- discretize(pe25,nlevels=5)  



# https://stackoverflow.com/questions/44351127/how-to-plot-paralle --------


library(tidyverse)
library(dplyr)
theme_set(theme_classic())

# Get 20 random rows from the diamonds data frame after limiting
#  to two levels each of cut and color
set.seed(2)
ds <- ggplot2::diamonds %>% 
  filter((cut == "Good" | cut == "Premium") & (color == "D" | color == "J")) %>%
  sample_n(100)

# ds = diamonds %>% 
#   filter(color %in% c("D","J"), cut %in% c("Good", "Premium")) %>%
#   sample_n(20)

ggplot(ds %>% 
         mutate(ID = 1:n()) %>%             # Add ID for each row
         mutate_if(is.numeric, scale) %>%   # Scale numeric columns
         gather(key, value, c(1,5:10)),     # Reshape to "long" format
       aes(key, value, group=ID, colour=color, fill=cut)) +
  geom_line() +
  geom_point(size=2, shape=21, colour="grey50") +
  scale_fill_manual(values=c("black","white"))


ggplot(d %>% 
         mutate_if(is.numeric, scale),
       # ds %>% 
       #   mutate(ID = 1:n()) %>%             # Add ID for each row
       #   mutate_if(is.numeric, scale) %>%   # Scale numeric columns
       #   gather(key, value, c(1,5:10)),     # Reshape to "long" format
       aes(item, h2, group=seed, colour=seed, fill=seed)) +
  geom_line() +
  geom_point(size=2, shape=21, colour="grey50") +
  scale_fill_manual(values=c("black","white"))



# https://www.r-bloggers.com/parallel-coordinate-plots-for-discret --------

install.packages("GGally")
library(GGally)

library(triangle)

set.seed(0)

q1_d1 <- round(rtriangle(1000, 1, 7, 5))
q1_d2 <- round(rtriangle(1000, 1, 7, 6))
q1_d3 <- round(rtriangle(1000, 1, 7, 2))

df_grouped <- data.frame(q1_d1 = factor(q1_d1), q1_d2 = factor(q1_d2), q1_d3 =  factor(q1_d3))

# group by combinations and count
df_grouped <- df_grouped %>% group_by(q1_d1, q1_d2, q1_d3) %>% mutate("count",count)

# set an id string that denotes the value combination
df_grouped <- df_grouped  %>%  mutate(id = factor(paste(q1_d1, q1_d2, q1_d3, sep = '-')))

# sort by count and select top rows
df_grouped <- df_grouped  %>% group_by(count) %>% arrange(desc(n))[1:10,]



d.wide <- d %>% select(item, seed, Communal) %>% spread(item, Communal) %>% select(-seed)


ggparcoord(d.wide,
           columns = c(1:8),
           # groupColumn = '',
           scale = 'globalminmax')
