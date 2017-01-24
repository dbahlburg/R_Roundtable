library(ggplot2)
library(dplyr)
library(grid)
library(gtable)
#How to add a second y-axis using the function ggplot_dual_axis from Jon Lefcheck (https://gist.github.com/jslefche/e4c0e9f57f0af49fca87)
ggplot_dual_axis = function(plot1, plot2, which.axis = "x") {
  
  # Update plot with transparent panel
  plot2 = plot2 + theme(panel.background = element_rect(fill = NA))
  
  grid.newpage()
  
  # Increase right margin if which.axis == "y"
  if(which.axis == "y") plot1 = plot1 + theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"))
  
  # Extract gtable
  g1 = ggplot_gtable(ggplot_build(plot1))
  
  g2 = ggplot_gtable(ggplot_build(plot2))
  
  # Overlap the panel of the second plot on that of the first
  pp = c(subset(g1$layout, name == "panel", se = t:r))
  
  g = gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # Steal axis from second plot and modify
  axis.lab = ifelse(which.axis == "x", "axis-b", "axis-l")
  
  ia = which(g2$layout$name == axis.lab)
  
  ga = g2$grobs[[ia]]
  
  ax = ga$children[[2]]
  
  # Switch position of ticks and labels
  if(which.axis == "x") ax$heights = rev(ax$heights) else ax$widths = rev(ax$widths)
  
  ax$grobs = rev(ax$grobs)
  
  if(which.axis == "x") 
    
    ax$grobs[[2]]$y = ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm") else
      
      ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  
  # Modify existing row to be tall enough for axis
  if(which.axis == "x") g$heights[[2]] = g$heights[g2$layout[ia,]$t]
  
  # Add new row or column for axis label
  if(which.axis == "x") {
    
    g = gtable_add_grob(g, ax, 2, 4, 2, 4) 
    
    g = gtable_add_rows(g, g2$heights[1], 1)
    
    g = gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
    
  } else {
    
    g = gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    
    g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b) 
    
    g = gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)
    
  }
  
  # Draw it
  grid.draw(g)
  
}

cars <- cars
cars <- cars %>%
  mutate(x_value = 1:length(cars[,1]))

g1 <- ggplot(data=cars, aes(x=x_value, y=speed))+
  geom_point(shape=15,colour="#C8171C") +
  geom_line(linetype = 3, colour="#C8171C") +
  ylim(0, 25)+ #customized ylims as you want them to be on the dual-axis-plots
  theme_bw()+
  theme(axis.text.y = element_text(colour = "#C8171C")) #change the text colour of the axis tick labels, matching your data

g2<-ggplot(data=cars, aes(x=x_value, y=dist))+
  geom_point(shape=15,colour = "#4699dd") +
  geom_line(linetype = 3, colour = "#4699dd") +
  ylim(0, 120)+  #customized ylims as you want them to be on the dual-axis-plots
  theme_bw()+
  theme(axis.text.y = element_text(colour = "#4699dd")) #change the text colour of the axis tick labels, matching your data

#now ggplot_dual_axis merges/overlays both plots. Unfortunately, ggsave does not work with this kind of plot...
#Nevertheless, you can still use the following procedure to save the plot.
svg("dual_axis_plot.svg", width=9, height=5)
ggplot_dual_axis(g1, g2, "y")
dev.off()
#An alternative...
grid.newpage()
grid.draw(rbind(ggplotGrob(g1), ggplotGrob(g2), size = "last"))

