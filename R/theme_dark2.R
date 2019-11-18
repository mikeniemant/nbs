#' theme_dark2 theme
#' 
#' Theme to create dark plots
#' 
#' Version 1.3 2019-11-18
#' 
#' This function allows you to plot ggplots with a black theme
#' @keywords gg_black
#' @export
#' @examples
#' gg_black()

gg_dark2 <- function(lines = "v+h", background.colour = "gray") {
  
  t <- theme(strip.text.x = element_text(colour="white"),
             strip.text.y = element_text(colour="white"),
             axis.text.x = element_text(colour="white"),
             axis.text.y = element_text(colour="white",hjust=1),
             axis.ticks =  element_line(colour = "white"),
             axis.title.x = element_text(colour="white"),
             axis.title.y = element_text(angle=90, colour="white"),
             panel.background = element_rect(fill=background.colour),
             panel.border = element_blank(),
             plot.background = element_rect(fill="black"),
             legend.background=element_rect(fill='black'),
             legend.title=element_text(colour="white"),
             legend.text=element_text(colour="white"),
             legend.key = element_rect(fill = "black"),
             plot.title = element_text(colour = "white"),
             plot.subtitle = element_text(colour = "white"))
  
  if(lines == "h") {
    t <- t + theme(# panel.grid.minor.y = element_line(color="white", size = 1, linetype = "dashed"),
               panel.grid.major.x = element_blank(), # remove vertical lines
               panel.grid.minor.x = element_blank(), # remove vertical lines
               # plot.title =element_text(size=28,colour="white"),
               # plot.margin = unit(c(1,  1, 1, 1), "lines"),
    )
  } else if(lines == "v") {
    t <- t + theme(panel.border = element_blank(),
               plot.background = element_rect(fill="black"),
               panel.grid.major.x = element_line(size=.1, color="white"),
               panel.grid.major.y = element_blank(), # remove horizontal lines
               panel.grid.minor.y = element_blank() # remove horizontal lines
               # plot.title =element_text(size=28,colour="white"),
               # plot.margin = unit(c(1,  1, 1, 1), "lines"),
    )
  }
  
  return(t)
}
