
#' Create a quick and beautiful barplot
#'
#' @param df A dataframe 
#' @param x The X aesthetic
#' @param y The Y aesthetic
#' @param fill An optional fill aesthetic
#' @param labels Boolean to determine whether to include labels
#' @param digits Number of digits after the comma
#' @param scaling scaling function provided by the scales package or user defined functions
#' @param position position function provided by the ggplot2 package
#' @param fontsize base_size of the plot (applied via theme_bw(base_size=fontsize))
#' @param legend_title Label for the legend
#' @param labelsize Size of the labels if included
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(magrittr)
#' library(scales)
#' tab <- diamonds%>%group_by(cut)%>%summarise(n=n())
#' vis_barplot(tab, x=cut, y=n, fill=cut, scaling=comma)
#' 
vis_barplot<-function(df,x,y,fill=NULL,labels=T,digits=2,
                      scaling=function(x){return(x)},position=ggplot2::position_stack(),
                      fontsize=22,legend_title="",labelsize=3.5){
  warnings("This function is deprecated. Please use vis_col instead")
  # fill<-ifelse(length(deparse(substitute(fill)))==0,"NULL",as_name(substitute(fill)))  # used to work
  fill<-ifelse(deparse(substitute(fill))=="NULL","NULL",as_name(substitute(fill)))
  gg<-ggplot2::ggplot(df,aes(x=!!enexpr(x),y=!!enexpr(y),fill=!!parse_expr(fill)))+
    ggplot2::geom_bar(stat = "identity",position = position)+
    ggplot2::scale_y_continuous(labels = scaling)+ 
    ggplot2::guides(fill=guide_legend(title=legend_title))+
    ggthemes::scale_fill_tableau()
  if(labels==T){
    gg<-gg+
      ggplot2::geom_label(aes(x=!!enexpr(x),y=!!enexpr(y),label=scaling(round(!!enexpr(y),digits))),
                 data = df,show.legend = F,position = position,size=labelsize)
  }
  return(gg+ggplot2::theme_bw(base_size =fontsize))
}

