

# Themes

theme_custom <- function(fig_size = "auto") {

  if(fig_size == "auto") {
    theme_fig <- theme_light() +
      theme(text = element_text(size = 12),
            legend.background = element_rect(fill="white",color=NA),
            legend.title=element_blank(),
            legend.position="right",
            plot.margin = margin(8,10,8,8),
            axis.text.y = element_text(size=9),
            axis.text.x = element_text(size=9),
            #axis.line = element_line(color="black"),
            axis.title.y = element_text(face="bold",vjust=2),
            strip.text.x = element_text(size = 10, color = "black",face = "bold"),
            strip.text.y = element_text(size = 9, color = "black",face = "bold"),
            strip.background = element_rect(fill=NA, size=1))#C5CFE3
  }
  else if(fig_size == "small") {
    theme_fig <- theme_light() +
      theme(text = element_text(size = 11),
            plot.subtitle = element_text(face="bold",size = 10,hjust=0.5),
            axis.text.y = element_text(size=10),
            axis.title.y = element_text(size=10,face="bold",vjust=2))
    theme_fig
  }}

