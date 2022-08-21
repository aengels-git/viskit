#' D3 palette reordered.
#'
#' @description A function to retreive a vector of hex codes for a non-numeric (or non-ordererd) variable.
#'
#' @param n The number of colours (excluding an NA colour).
#'
#' @return A character vector of hex codes.
#' @export
#' @examples
#' scales::show_col(pal_d3_mix(9))
pal_d3_mix <- function(n) {
  c("#17BECF", "#BCBD22", "#8C564B", "#E377C2", "#1F77B4", "#FF7F0E", "#2CA02C", "#9467BD", "#D62728")[1:n]
}

#' Tableau palette (10 or 20)
#'
#' @description A function to retreive a vector of hex codes for a non-numeric (or non-ordererd) variable.
#'
#' @param n The number of colours (excluding an NA colour).
#'
#' @return A character vector of hex codes.
#' @export
#' @examples
#' scales::show_col(pal_tableau(15))
pal_tableau <- function(n) {
  if(n>10){
    c("#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F",
      "#8CD17D", "#B6992D", "#F1CE63", "#499894", "#86BCB6", 
      "#E15759", "#FF9D9A", "#79706E", "#BAB0AC", "#D37295",
      "#FABFD2", "#B07AA1", "#D4A6C8", "#9D7660", "#D7B5A6")[1:n]
  }else{
    c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
      "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC")[1:n]
  }
}

#' Viridis palette reordered.
#'
#' @description A function to retreive a vector of hex codes for a numeric (or ordererd) variable.
#'
#' @param n The number of colours (excluding an NA colour).
#'
#' @return A character vector of hex codes.
#' @export
#' @examples
#' scales::show_col(pal_viridis_mix(9))
pal_viridis_mix <- function(n) {
  
  if(n == 1) viridis::viridis(4)[2]
  else if(n == 2) viridis::viridis(4)[c(2, 3)]
  else if(n >= 3) viridis::viridis(n)
}

#' NA palette.
#'
#' @description A function to retreive a hex code for a colour to use for NA values.
#'
#' @param pal The hex code or name of the NA colour. Defaults to "#7F7F7FFF".
#'
#' @return A character vector.
#' @export
#' @examples
#' scales::show_col(pal_na())
pal_na <- function(pal = "#7F7F7F") {
  return(pal)
}