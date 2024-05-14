
#' @title Histogram ggplot.
#'
#' @description Create a Histogram
#' @param data A data frame or tibble.
#' @param x Unquoted x aesthetic variable.
#' @param x_label label function for x
#' @param x_breaks break function for x
#' @param y_label label function for y
#' @param y_breaks break function for x
#' @param fontsize general fontsize of labels
#' @param col color of bars
#' @param bins number of bins
#' @return A ggplot object.
#' 
#' @export
vis_histogram <- function(data, x, 
                          x_label=function(x){x}, 
                          x_breaks = pretty_breaks(n = 5),
                          y_label=function(x){x}, 
                          y_breaks = pretty_breaks(n = 5),
                          fontsize = 14, 
                          col = "#4E79A7", 
                          bins = 30){
  ggplot(data = data, aes(x=x, fill=I(col), color=I("black")))+
    geom_histogram(bins = bins)+
    theme_bw(base_size = fontsize)+
    scale_y_continuous(labels = y_label, breaks = y_breaks)+
    scale_x_continuous(labels = x_label, breaks = x_breaks)
}

#' @title Density ggplot.
#'
#' @description Create a Density plot
#' @param data A data frame or tibble.
#' @param x Unquoted x aesthetic variable.
#' @param x_label label function for x
#' @param x_breaks break function for x
#' @param y_label label function for y
#' @param y_breaks break function for x
#' @param fontsize general fontsize of labels
#' @param col color of density
#' @param density_factor adjust the granularity of the density plot, between 1 and 10
#' @return A ggplot object.
#' 
#' @export
vis_density <- function(data, x, 
                        x_label=function(x){x}, 
                        x_breaks = pretty_breaks(n = 5),
                        y_label=function(x){x}, 
                        y_breaks = pretty_breaks(n = 5),
                        fontsize = 14, 
                        col = "#4E79A7", 
                        density_factor = 1){
  ggplot(data = data, aes(x=x, fill=I(col), color=I("black")))+
    geom_density(adjust = density_factor)+
    theme_bw(base_size = fontsize)+
    scale_y_continuous(labels = y_label, breaks = y_breaks)+
    scale_x_continuous(labels = x_label, breaks = x_breaks)
}
