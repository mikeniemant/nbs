#' gg_black theme
#' 
#' Theme to create dark plots
#' 
#' Version 1.2 2019-11-15
#' 
#' This function allows you to plot ggplots with a black theme
#' @keywords gg_black
#' @export
#' @examples
#' gg_black()

gg_black <- function(lines = "v+h") {
  if(lines == "h") {
    theme(strip.text.x = element_text(colour="white"),
          strip.text.y = element_text(colour="white"),
          axis.text.x = element_text(colour="white"),
          axis.text.y = element_text(colour="white",hjust=1),
          axis.ticks =  element_line(colour = "white"),
          axis.title.x = element_text(colour="white"),
          axis.title.y = element_text(angle=90, colour="white"),
          panel.background = element_rect(fill="black"),
          panel.border = element_blank(),
          plot.background = element_rect(fill="black"),
          # panel.grid.minor.y = element_line(color="white", size = 1, linetype = "dashed"),
          panel.grid.major.x = element_blank(), # remove vertical lines
          panel.grid.minor.x = element_blank(), # remove vertical lines
          # plot.title =element_text(size=28,colour="white"),
          # plot.margin = unit(c(1,  1, 1, 1), "lines"),
          legend.background=element_rect(fill='black'),
          legend.title=element_text(colour="white"),
          legend.text=element_text(colour="white"),
          legend.key = element_rect(fill = "black"),
          axis.line.x = element_line(color="white", size = 1),
          axis.line.y = element_line(color="white", size = 1),
          plot.title = element_text(colour = "white"),
          plot.subtitle = element_text(colour = "white")
    )
  } else if(lines == "v") {
    theme(strip.text.x = element_text(colour="white"),
          strip.text.y = element_text(colour="white"),
          axis.text.x = element_text(colour="white"),
          axis.text.y = element_text(colour="white",hjust=1),
          axis.ticks =  element_line(colour = "white"),
          axis.title.x = element_text(colour="white"),
          axis.title.y = element_text(angle=90, colour="white"),
          panel.background = element_rect(fill="black"),
          panel.border = element_blank(),
          plot.background = element_rect(fill="black"),
          panel.grid.major.x = element_line(size=.1, color="white"),
          panel.grid.major.y = element_blank(), # remove horizontal lines
          panel.grid.minor.y = element_blank(), # remove horizontal lines
          # plot.title =element_text(size=28,colour="white"),
          # plot.margin = unit(c(1,  1, 1, 1), "lines"),
          legend.background=element_rect(fill='black'),
          legend.title=element_text(colour="white"),
          legend.text=element_text(colour="white"),
          legend.key = element_rect(fill = "black"),
          axis.line.x = element_line(color="white", size = 1),
          axis.line.y = element_line(color="white", size = 1),
          plot.title = element_text(colour = "white"),
          plot.subtitle = element_text(colour = "white")
    )    
  } else{
    theme(strip.text.x = element_text(colour="white"),
          strip.text.y = element_text(colour="white"),
          axis.text.x = element_text(colour="white"),
          axis.text.y = element_text(colour="white",hjust=1),
          axis.ticks =  element_line(colour = "white"),
          axis.title.x = element_text(colour="white"),
          axis.title.y = element_text(angle=90, colour="white"),
          panel.background = element_rect(fill="black"),
          panel.border = element_blank(),
          plot.background = element_rect(fill="black"),
          # plot.title =element_text(size=28,colour="white"),
          # plot.margin = unit(c(1,  1, 1, 1), "lines"),
          legend.background=element_rect(fill='black'),
          legend.title=element_text(colour="white"),
          legend.text=element_text(colour="white"),
          legend.key = element_rect(fill = "black"),
          axis.line.x = element_line(color="white", size = 1),
          axis.line.y = element_line(color="white", size = 1),
          plot.title = element_text(colour = "white"),
          plot.subtitle = element_text(colour = "white")
    )    
  }
}
