#' This function masks ggplot2 with Brookings Metro theme
#' @return ggplot2 object
#' @export

bbplot <- function(...){
  ggplot2::ggplot(...)+
    ggplot2::theme(rect = element_rect(fill = NA, colour=NA),
          panel.background = element_rect(fill = NA,colour = NA),
          plot.background = element_rect(fill = NA, colour = NA),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent", color = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA),
          text = element_text(size = 15,family ="sans" ),
          axis.text = element_text(size = 12, family = "sans"),
          plot.title = element_text(hjust = 0.5),
          axis.ticks = element_blank()
    )
}
