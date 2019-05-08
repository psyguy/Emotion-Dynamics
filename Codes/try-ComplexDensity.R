# rm(list=ls())
library(ComplexHeatmap)
set.seed(123)
mat = matrix(rnorm(80, 2), 8, 10)
mat = rbind(mat, matrix(rnorm(40, -2), 4, 10))
rownames(mat) = paste0("R", 1:12)
colnames(mat) = paste0("C", 1:10)


#  first try --------------------------------------------------------------


ha_column1 = HeatmapAnnotation(points = anno_points(rnorm(10)), 
                               annotation_name_side = "left")
(ht1 = Heatmap(mat, name = "ht1", km = 2, column_title = "Heatmap 1", 
              top_annotation = ha_column1, row_names_side = "left"))

ha_column2 = HeatmapAnnotation(type = c(rep("a", 5), rep("b", 5)),
                               col = list(type = c("a" = "red", "b" = "blue")))
ht2 = Heatmap(mat, name = "ht2", row_title = "Heatmap 2", column_title = "Heatmap 2",
              bottom_annotation = ha_column2)

ht_list = ht1 + ht2 + 
  rowAnnotation(bar = anno_barplot(rowMeans(mat), width = unit(2, "cm")))
draw(ht_list, row_title = "Heatmap list", column_title = "Heatmap list")
list_components()


ht_list = draw(ht_list, row_title = "Heatmap list", column_title = "Heatmap list", 
               heatmap_legend_side = "right", annotation_legend_side = "left")

decorate_heatmap_body("ht1", {
  grid.text("outlier", 1.5/10, 2.5/4, default.units = "npc")
  grid.lines(c(0.5, 0.5), c(0, 1), gp = gpar(lty = 2, lwd = 2))
}, slice = 2)

decorate_column_dend("ht1", {
  tree = column_dend(ht_list)$ht1[[1]]
  ind = cutree(as.hclust(tree), k = 2)[order.dendrogram(tree)]
  
  first_index = function(l) which(l)[1]
  last_index = function(l) { x = which(l); x[length(x)] }
  x1 = c(first_index(ind == 1), first_index(ind == 2)) - 1
  x2 = c(last_index(ind == 1), last_index(ind == 2))
  grid.rect(x = x1/length(ind), width = (x2 - x1)/length(ind), just = "left",
            default.units = "npc", gp = gpar(fill = c("#FF000040", "#00FF0040"), col = NA))
})

decorate_row_names("ht1", {
  grid.rect(gp = gpar(fill = "#FF000040"))
}, slice = 2)

decorate_row_title("ht1", {
  grid.rect(gp = gpar(fill = "#00FF0040"))
}, slice = 1)

decorate_annotation("points", {
  grid.lines(c(0, 1), unit(c(0, 0), "native"), gp = gpar(col = "red"))
})


# second try --------------------------------------------------------------

heatmap(mat, name = "mat", 
        row_split = rep(LETTERS[1:3], 6),
        column_split = rep(letters[1:6], 4))

Heatmap(mat, name = "mat", 
        row_km = 2, row_title_gp = gpar(col = c("red", "blue"), font = 1:2),
        row_names_gp = gpar(col = c("green", "orange"), fontsize = c(10, 14)),
        column_km = 3, column_title_gp = gpar(fill = c("red", "blue", "green"), font = 1:3),
        column_names_gp = gpar(col = c("green", "orange", "purple"), fontsize = c(10, 14, 8)))



# complexheatmap working --------------------------------------------------

ha_column1 = HeatmapAnnotation(points = anno_points(rnorm(10)), 
                               annotation_name_side = "left")

(ht1 = Heatmap(mat, name = "ht1",
               column_title = "Heatmap 1",
               row_split = c(rep("A", 3), rep("B",9)),
               show_row_dend = F,
               show_column_dend = F))

decorate_heatmap_body("ht1", {
  grid.text("outlier", 1.5/10, 2.5/4, default.units = "npc")
  grid.lines(c(0.5, 0.5), c(0, 1), gp = gpar(lty = 2, lwd = 2))
}, slice = 2)
















decorate_row_names("ht1", {
  grid.rect(gp = gpar(fill = "#FF000040"))
}, slice = 2)

decorate_row_title("ht1", {
  grid.rect(gp = gpar(fill = "#00FF0040"))
}, slice = 1)

# decorate_annotation("points", {
#   grid.lines(c(0, 1), unit(c(0, 0), "native"), gp = gpar(col = "red"))
# })


# https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2/ -----------


# Library
library(tidyverse)

# Data
a <- data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
b <- data.frame( x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
c <- data.frame( x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
data <- rbind(a,b,c)


# Basic scatterplot
ggplot(data, aes(x=x, y=y) ) +
  geom_point()


# Show the contour only
ggplot(data, aes(x=x, y=y) ) +
  geom_density_2d()

# Show the area only
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")

# Area + contour
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")

# Using raster
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )






# Call the palette with a number
ggplot(d.long, aes(x=c(1:nrow(d.long)), y=Communal) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette=4, direction=-1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )


# The direction argument allows to reverse the palette
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette=4, direction=1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

# You can also call the palette using a name.
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral", direction=1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

# Call the palette with a number
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette=4, direction=-1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

# The direction argument allows to reverse the palette
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette=4, direction=1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

# You can also call the palette using a name.
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral", direction=1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )


