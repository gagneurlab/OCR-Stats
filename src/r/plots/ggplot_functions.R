# Different useful ggplot functions
library(ggthemes)

give.n <- function(x, mult = 1.02){
  return(c(y = median(x) * mult, label = length(x))) 
}

increase_labs_ggplot <- function(ggplot_obj){
  g = ggplot_obj + 
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14, face="bold"),
          legend.text=element_text(size=12), 
          legend.title=element_text(size=14))
  g
}

add_theme_bw = function(ggplot_obj, legend_pos = "top"){
  g = ggplot_obj + theme_bw() 
  g = increase_labs_ggplot(g)
  g = g + theme(panel.grid.minor = element_blank(), 
          panel.border = element_blank(), 
          panel.background = element_blank(),
          axis.line.x = element_line(size=0.5, colour = "black"),
          axis.line.y = element_line(size=0.5, colour = "black"),
          legend.position = legend_pos)
  
  g
}


#### Interesting color palettes and themes:
# https://cran.r-project.org/web/packages/ggthemes/vignettes/ggthemes.html
# p_ex = ggplot(data, aes(time,OCR, color = Fibroblast_id)) + geom_point() 
# p_ex + scale_color_ptol("Fibroblast_id")
# p_ex + scale_color_ptol("Fibroblast_id") + theme_minimal()
# p_ex + scale_color_tableau("colorblind10") 
# p_ex + theme_wsj() + scale_colour_wsj("colors6", "")
# p_ex + theme_gdocs() + scale_color_gdocs()


# Adds numbers to either x or y axis of a ggplot object
add_numbers_x_axis <- function(ggplot_obj, numbers){
  n = length(numbers)
  for(i in 1:n){
    t1 = numbers[i]
    gtext = textGrob(t1, y = -0.02)
    gline = linesGrob(y = c(-.01, 0)) 
    ggplot_obj = ggplot_obj + annotation_custom(gtext, xmin=t1, xmax=t1, ymin=-Inf, ymax=Inf) +
      annotation_custom(gline, xmin=t1, xmax=t1, ymin=-Inf, ymax=Inf)
  }
  p = ggplotGrob(ggplot_obj)
  p$layout$clip[p$layout$name=="panel"] <- "off"
  grid.draw(p)
}

# Way to use: add_x_axis_text(g, c(c1,c2))


add_numbers_y_axis <- function(ggplot_obj, numbers){
  g <- copy(ggplot_obj)
  n = length(numbers)
  for(i in 1:n){
    t1 = numbers[i]
    gtext = textGrob(x = -.02, t1)
    gline = linesGrob(x = c(-.01, 0)) 
    g = g + annotation_custom(gtext, xmin=-Inf, xmax=Inf, ymin=t1, ymax=t1) +
      annotation_custom(gline, xmin=-Inf, xmax=Inf, ymin=t1, ymax=t1)
  }
  p = ggplotGrob(g)
  p$layout$clip[p$layout$name=="panel"] <- "off"
  grid.draw(p)
}
# add_numbers_y_axis(g, .15)
