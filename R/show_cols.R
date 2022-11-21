#' Function to plot hex colors as barplot to visualize the colors
#'
#' @param hex_cols a character vector of hex colors
#'
#' @return
#' @export
#'
#' @examples
show_cols<-function(hex_cols){
  tibble(
    x=fct_reorder(hex_cols,1:length(hex_cols)),
    y=rep(1/length(hex_cols),length(hex_cols))
  )%>%
    ggplot(.,aes(x=x,y=y,fill=x))+
    geom_bar(stat = "identity")+
    scale_fill_manual(values = hex_cols)+
    coord_flip()+
    theme_bw(base_size = 18)+
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.ticks.x=element_blank())+
    scale_y_continuous(name = NULL)+
    xlab(NULL)
}