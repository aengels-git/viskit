#' @title Bar ggplot.
#'
#' @description Create a ranked bar plot 
#' @param data A data frame or tibble.
#' @param x Unquoted x aesthetic variable.
#' @param y Unquoted y aesthetic variable, used for sorting
#' @param descending boolean to indicate whether to sort values in descending order
#' @return A ggplot object.
#' 
#' @export
vis_ranked_barplot<-function(data, x, y,descending = TRUE, ...){
  x <- deparse(substitute(x))
  y <- deparse(substitute(y))
  
  data <- data%>%
    mutate(!!parse_expr(x) := fct_reorder(!!parse_expr(x), !!parse_expr(y),.desc=descending))
  
  vis_barplot(data = data, x=!!parse_expr(x),y=!!parse_expr(y),...)
}



#' @title Bar ggplot.
#'
#' @description Create a bar plot with a wrapper around the ggplot2::geom_bar function.
#' @param data A data frame or tibble.
#' @param x Unquoted x aesthetic variable.
#' @param y Unquoted y aesthetic variable.
#' @param col Unquoted col and fill aesthetic variable.
#' @param facet Unquoted facet aesthetic variable.
#' @param facet2 Unquoted second facet variable for a facet grid of facet by facet2 variables.
#' @param group Unquoted group aesthetic variable.
#' @param text Unquoted text aesthetic variable, which is used for the labels and in combination with plotly::ggplotly(., tooltip = "text").
#' @param stat Statistical transformation. A character string (e.g. "identity").
#' @param position Position adjustment. Either a character string (e.g."identity"), or a function (e.g. ggplot2::position_identity()).
#' @param pal Colours to use. A character vector of hex codes (or names).
#' @param pal_na Colour to use for NA values. A character vector of a hex code (or name).
#' @param alpha Opacity. A number between 0 and 1.
#' @param width Width. A number 0 upwards.
#' @param digits Numbers after the decimal point to rount to
#' @param ... Other arguments passed to the relevant ggplot2::geom_* function.
#' @param titles A function to format the x, y and col titles, including in rlang lambda format. Defaults to snakecase::to_sentence_case.
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param coord Coordinate system.
#' @param x_breaks A function that takes the limits as input (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param x_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param x_include For a numeric or date variable, any values that the scale should include (e.g. 0).
#' @param x_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param x_limits A vector of length 2 to determine the limits of the axis.
#' @param x_oob A scales::oob_* function for how to deal with out-of-bounds values.
#' @param x_sec_axis A secondary axis specified by the ggplot2::sec_axis or ggplot2::dup_axis function.
#' @param x_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param x_trans For a numeric variable, a transformation object (e.g. "log10").
#' @param y_breaks A function that takes the limits as input (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param y_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param y_include For a numeric or date variable, any values that the scale should include (e.g. 0).
#' @param y_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param y_limits A vector of length 2 to determine the limits of the axis.
#' @param y_oob A scales::oob_* function for how to deal with out-of-bounds values.
#' @param y_sec_axis A secondary axis specified by the ggplot2::sec_axis or ggplot2::dup_axis function.
#' @param y_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param y_trans For a numeric variable, a transformation object (e.g. "log10").
#' @param col_breaks A function that takes the limits as input (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param col_include For a numeric or date variable, any values that the scale should include (e.g. 0).
#' @param col_intervals A function to cut or chop the numeric variable into intervals (e.g. ~ santoku::chop_mean_sd(.x, drop = FALSE)).
#' @param col_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels. Note this does not affect where col_intervals is not NULL.
#' @param col_limits A vector to determine the limits of the axis.
#' @param col_legend_ncol The number of columns for the legend elements.
#' @param col_legend_nrow The number of rows for the legend elements.
#' @param col_legend_place The place for the legend. "b" for bottom, "r" for right, "t" for top, or "l" for left.
#' @param col_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param facet_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a named vector of labels (e.g. c(value = "label", ...)).
#' @param facet_ncol The number of columns of facetted plots.
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_scales Whether facet_scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param caption Caption title string.
#' @param fontsize Fontsize of the entire plot
#' @param labelsize Size of the labels. By default a function of the overall fontsize, but it can be set separately.
#' @param theme A ggplot2 theme.
#' @return A ggplot object.
#' 
#' @export
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(magrittr)
#' tab <- diamonds%>%group_by(cut)%>%summarise(n=n())
#' vis_barplot(tab, x=cut, y=n, fill=cut,text=n)
#' 
#' tab<-diamonds%>%group_by(cut)%>%summarise(n=n())%>%
#'        mutate(prop=n/sum(n))
#' vis_barplot(tab,x = cut,y=prop,text = prop,
#'         col = cut,y_title = "Percentage",
#'         y_include = 0.45,y_breaks = scales::breaks_width(0.05),
#'         y_labels = function(x){scales::percent(x,accuracy = 1)})
#'


vis_barplot <- function(
    data = NULL,
    x = NULL,
    y = NULL,
    col = NULL,
    facet = NULL,
    facet2 = NULL,
    group = NULL,
    text = NULL,
    stat = "count",
    position = "stack",
    pal = NULL,
    pal_na = "#7F7F7F",
    alpha = 0.9,
    #linewidth = 0.5,
    width = 0.9,
    digits = 2,
    ...,
    titles = NULL,
    title = NULL,
    subtitle = NULL,
    coord = NULL,
    x_breaks = NULL,
    x_expand = NULL,
    x_include = NULL,
    x_labels = NULL,
    x_limits = NULL,
    x_oob = scales::oob_censor,
    x_sec_axis = ggplot2::waiver(),
    x_title = NULL,
    x_trans = "identity",
    y_breaks = NULL,
    y_expand = NULL,
    y_include = NULL,
    y_labels = NULL,
    y_limits = NULL,
    y_oob = scales::oob_censor,
    y_sec_axis = ggplot2::waiver(),
    y_title = NULL,
    y_trans = "identity",
    col_breaks = NULL,
    col_include = NULL,
    col_intervals = NULL,
    col_labels = NULL,
    col_legend_place = "b",
    col_legend_ncol = NULL,
    col_legend_nrow = NULL,
    col_limits = NULL,
    col_title = "",
    col_title_position = NULL,
    facet_labels = NULL,
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_scales = "fixed",
    caption = NULL,
    fontsize = 18,
    labelsize = NULL,
    theme = NULL) {
  
  #quote
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  col <- rlang::enquo(col)
  facet <- rlang::enquo(facet)
  facet2 <- rlang::enquo(facet2)
  group <- rlang::enquo(group)
  text <- rlang::enquo(text)
  
  #stop, warn or message
  if (rlang::is_null(data)) rlang::abort("data is required.")
  if (rlang::is_null(titles)) rlang::inform(c("i" = "{ggblanket} converts unspecified titles using snakecase::to_sentence_case. Use titles = ~.x to leave unspecified titles as is, and/or specify individual titles manually using *_title arguments."))
  if (!rlang::quo_is_null(col)) rlang::inform(c("i" = "{ggblanket} merges col and fill aesthetics into a single col aesthetic."))
  if (!rlang::quo_is_null(facet)) rlang::inform(c("i" = "{ggblanket} treats faceting as an aesthetic."))
  
  ###ungroup
  data <- dplyr::ungroup(data)
  
  ###get default NULL values
  if (rlang::quo_is_null(x)) {
    if (rlang::is_null(x_title)) {
      if (stat %in% c("bin", "count")) {
        if (rlang::is_null(titles)) x_title <- purrr::map_chr("count", snakecase::to_sentence_case)
        else x_title <- purrr::map_chr("count", titles)
      }
      else if (stat %in% c("density", "ydensity")) {
        if (rlang::is_null(titles)) x_title <- purrr::map_chr("density", snakecase::to_sentence_case)
        else x_title <- purrr::map_chr("density", titles)
      }
      else if (stat == "function") {
        if (rlang::is_null(titles)) x_title <- purrr::map_chr("x", snakecase::to_sentence_case)
        else x_title <- purrr::map_chr("x", titles)
      }
      else if (stat == "qq") {
        if (rlang::is_null(titles)) x_title <- purrr::map_chr("theoretical", snakecase::to_sentence_case)
        else x_title <- purrr::map_chr("theoretical", titles)
      }
    }
  }
  else if (rlang::is_null(x_title)) {
    if (rlang::is_null(titles)) x_title <- purrr::map_chr(rlang::as_name(x), snakecase::to_sentence_case)
    else x_title <- purrr::map_chr(rlang::as_name(x), titles)
  }
  
  if (rlang::quo_is_null(y)) {
    if (rlang::is_null(y_title)) {
      if (stat %in% c("bin", "count")) {
        if (rlang::is_null(titles)) y_title <- purrr::map_chr("count", snakecase::to_sentence_case)
        else y_title <- purrr::map_chr("count", titles)
      }
      else if (stat %in% c("density", "ydensity")) {
        if (rlang::is_null(titles)) y_title <- purrr::map_chr("density", snakecase::to_sentence_case)
        else y_title <- purrr::map_chr("density", titles)
      }
      else if (stat == "function") {
        if (rlang::is_null(titles)) y_title <- purrr::map_chr("y", snakecase::to_sentence_case)
        else y_title <- purrr::map_chr("y", titles)
      }
      else if (stat == "qq") {
        if (rlang::is_null(titles)) y_title <- purrr::map_chr("sample", snakecase::to_sentence_case)
        else y_title <- purrr::map_chr("sample", titles)
      }
    }
  }
  else if (rlang::is_null(y_title)) {
    if (rlang::is_null(titles)) y_title <- purrr::map_chr(rlang::as_name(y), snakecase::to_sentence_case)
    else y_title <- purrr::map_chr(rlang::as_name(y), titles)
  }
  
  xy_numeric_date <- ifelse(((
    is.numeric(rlang::eval_tidy(x, data)) |
      rlang::quo_is_null(x) |
      lubridate::is.Date(rlang::eval_tidy(x, data))
  ) &
    (
      is.numeric(rlang::eval_tidy(y, data)) |
        rlang::quo_is_null(y) |
        lubridate::is.Date(rlang::eval_tidy(y, data))
    )),
  TRUE,
  FALSE
  )
  
  # if (rlang::is_null(theme)) {
  #   # if (xy_numeric_date) {
  #   #   grid_v <- FALSE
  #   #   grid_h <- TRUE
  #   # }
  #   # else {
  #   #   grid_v <-
  #   #     ifelse(is.numeric(rlang::eval_tidy(x, data)) |
  #   #              lubridate::is.Date(rlang::eval_tidy(x, data)) |
  #   #              rlang::quo_is_null(x),
  #   #            TRUE,
  #   #            FALSE)
  #   #   grid_h <-
  #   #     ifelse(is.numeric(rlang::eval_tidy(y, data)) |
  #   #              lubridate::is.Date(rlang::eval_tidy(y, data)) |
  #   #              rlang::quo_is_null(y),
  #   #            TRUE,
  #   #            FALSE)
  #   # }
  #   # 
  #   # theme <- gg_theme(grid_v = grid_v, grid_h = grid_h)
  # 
  # }
  if (rlang::is_null(theme)) {
    theme <- create_theme(fontsize = fontsize)
  }
  if (rlang::is_null(width)) {
    if (lubridate::is.Date(rlang::eval_tidy(x, data)) |
        lubridate::is.Date(rlang::eval_tidy(y, data)) |
        (rlang::quo_is_null(y) & is.numeric(rlang::eval_tidy(x, data))) |
        (rlang::quo_is_null(x) & is.numeric(rlang::eval_tidy(y, data))) |
        (is.numeric(rlang::eval_tidy(x, data)) &
         is.numeric(rlang::eval_tidy(y, data)))) {
      width <- NULL
    }
    else
      width <- 0.75
  }
  
  if (rlang::is_null(coord)) coord <- ggplot2::coord_cartesian(clip = "off")
  
  ###process plot data
  ###factorise logical, reverse for horizontal, and chop intervals
  if (!rlang::quo_is_null(x)) {
    if (is.logical(rlang::eval_tidy(x, data))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!x, ~ factor(.x, levels = c("FALSE", "TRUE"))))
    }
  }
  
  if (!rlang::quo_is_null(y)) {
    if (is.logical(rlang::eval_tidy(y, data))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!y, ~ factor(.x, levels = c("FALSE", "TRUE"))))
    }
    
    if (is.character(rlang::eval_tidy(y, data)) | is.factor(rlang::eval_tidy(y, data))) {
      
      if (!rlang::quo_is_null(col) &
          (identical(rlang::eval_tidy(y, data), rlang::eval_tidy(col, data)))) {
      }
      else {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!y, ~ forcats::fct_rev(.x)))
      }
    }
  }
  
  if (!rlang::quo_is_null(col)) {
    
    if (is.logical(rlang::eval_tidy(col, data))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!col, ~ factor(.x, levels = c("FALSE", "TRUE"))))
    }
    
    if (is.character(rlang::eval_tidy(col, data)) | is.factor(rlang::eval_tidy(col, data))) {
      if (is.character(rlang::eval_tidy(y, data)) | is.factor(rlang::eval_tidy(y, data))) {
        data <- data %>%
          dplyr::mutate(dplyr::across(!!col, ~ forcats::fct_rev(.x)))
      }
    }
  }
  
  if (!rlang::quo_is_null(facet)) {
    if (is.logical(class(rlang::eval_tidy(facet, data)))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!facet, ~ factor(.x, levels = c("FALSE", "TRUE"))))
    }
  }
  if (!rlang::quo_is_null(facet2)) {
    if (is.logical(class(rlang::eval_tidy(facet2, data)))) {
      data <- data %>%
        dplyr::mutate(dplyr::across(!!facet2, ~ factor(.x, levels = c("FALSE", "TRUE"))))
    }
  }

  ###make col scale
  if (rlang::quo_is_null(col)) {
    if (rlang::is_null(pal)) pal <-  pal_viridis_mix(1)
    else pal <- pal[1]
    
    col_scale <- list(
      ggplot2::scale_colour_manual(
        values = pal,
        na.value = pal_na,
      ),
      ggplot2::scale_fill_manual(
        values = pal,
        na.value = pal_na,
      )
    )
    
    
    col_legend_place <- "n"
  } else {
    if (rlang::is_null(col_title)) {
      if (rlang::is_null(titles)) col_title <- purrr::map_chr(rlang::as_name(col), snakecase::to_sentence_case)
      else col_title <- purrr::map_chr(rlang::as_name(col), titles)
    }
    if(is_null(col_title_position)){
      col_title_position <- ifelse(col_legend_place %in% c("t","b"), "left", "top")
    }
    
    
    if (rlang::is_null(col_legend_place)) {
      if (!rlang::quo_is_null(x) &
          (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(x, data)))) {
        col_legend_place <- "n"
      }
      else if (!rlang::quo_is_null(y) &
               (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(y, data)))) {
        col_legend_place <- "n"
      }
      else if (!rlang::quo_is_null(facet) &
               (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(facet, data)))) {
        col_legend_place <- "n"
      }
      else if (!rlang::quo_is_null(facet2) &
               (identical(rlang::eval_tidy(col, data), rlang::eval_tidy(facet2, data)))) {
        col_legend_place <- "n"
      }
      else
        col_legend_place <- "b"
    }
    
    if (is.numeric(rlang::eval_tidy(col, data))) {
      if (rlang::is_null(col_intervals)) { #continuous col
        col_min <- data %>% dplyr::pull(!!col) %>% min(na.rm = TRUE)
        col_max <- data %>% dplyr::pull(!!col) %>% max(na.rm = TRUE)
        
        if (!rlang::is_null(col_limits)) {
          if (is.na(col_limits)[1]) col_limits[1] <- col_min
          if (is.na(col_limits)[2]) col_limits[2] <- col_max
        }
        
        if (rlang::is_null(col_limits)) col_limits <- c(col_min, col_max)
        if (!rlang::is_null(col_include)) col_limits <- range(c(col_include, col_limits))
        
        if (rlang::is_null(col_breaks)) {
          if (col_legend_place %in% c("b", "t")) col_breaks_n <- 3
          else col_breaks_n <- 4
          col_breaks <- scales::breaks_pretty(n = col_breaks_n)(col_limits)
        }
        
        if (rlang::is_null(pal)) pal <- viridis::viridis(100)
        if (rlang::is_null(col_labels)) col_labels <- scales::label_comma()
        
        col_scale <- list(
          ggplot2::scale_colour_gradientn(
            colors = pal,
            labels = col_labels,
            breaks = col_breaks,
            limits = col_limits,
            na.value = pal_na,
            guide = ggplot2::guide_colorbar(title.position = col_title_position)
          ),
          col_scale <- ggplot2::scale_fill_gradientn(
            colors = pal,
            labels = col_labels,
            breaks = col_breaks,
            limits = col_limits,
            na.value = pal_na,
            guide = ggplot2::guide_colorbar(title.position = col_title_position)
          )
          
        )
      }
      else { #intervals col
        data <- data %>%
          dplyr::mutate(dplyr::across(!!col, col_intervals))
        
        col_levels <- levels(rlang::eval_tidy(col, data))
        col_n <- length(col_levels)
        
        if (rlang::is_null(pal)) pal <- pal_viridis_mix(col_n)
        else pal <- pal[1:col_n]
        
        if (is.numeric(rlang::eval_tidy(y, data)) |
            lubridate::is.Date(rlang::eval_tidy(y, data))) {
          
          if (col_legend_place %in% c("b", "t")) col_legend_rev <- FALSE
          else col_legend_rev <- TRUE
        }
        else if (is.character(rlang::eval_tidy(y, data)) | is.factor(rlang::eval_tidy(y, data))) {
          if (col_legend_place %in% c("b", "t")) col_legend_rev <- TRUE
          else col_legend_rev <- FALSE
          pal <- rev(pal)
        }
        else col_legend_rev <- FALSE
        
        if (rlang::is_null(col_breaks)) col_breaks <- ggplot2::waiver()
        if (rlang::is_null(col_labels)) col_labels <- ggplot2::waiver()
        
        col_scale <- list(
          ggplot2::scale_colour_manual(
            values = pal,
            breaks = col_levels,
            limits = col_levels,
            labels = col_labels,
            na.value = pal_na,
            guide = ggplot2::guide_legend(
              reverse = col_legend_rev,
              title.position = col_title_position,
              ncol = col_legend_ncol,
              nrow = col_legend_nrow,
              byrow = TRUE
            )
          ),
          ggplot2::scale_fill_manual(
            values = pal,
            breaks = col_levels,
            limits = col_levels,
            labels = col_labels,
            na.value = pal_na,
            # aesthetics = c("col", "fill"),
            guide = ggplot2::guide_legend(
              reverse = col_legend_rev,
              title.position = col_title_position,
              ncol = col_legend_ncol,
              nrow = col_legend_nrow,
              byrow = TRUE
            )
          )
        )
      }
    }
    else { #categorical col
      if (!rlang::is_null(col_limits)) col_n <- length(col_limits)
      else if (!rlang::is_null(col_breaks)) col_n <- length(col_breaks)
      else {
        if (is.factor(rlang::eval_tidy(col, data))) {
          col_n <- length(levels(rlang::eval_tidy(col, data)))
        }
        else col_n <- length(unique(rlang::eval_tidy(col, data)))
      }
      
      if (rlang::is_null(pal)) pal <- pal_tableau(col_n)
      else pal <- pal[1:col_n]
      
      if (is.numeric(rlang::eval_tidy(y, data)) |
          lubridate::is.Date(rlang::eval_tidy(y, data))) {
        
        if (is.character(rlang::eval_tidy(col, data)) | is.factor(rlang::eval_tidy(col, data))) {
          col_legend_rev <- FALSE
        }
        else if (col_legend_place %in% c("b", "t")) col_legend_rev <- FALSE
        else col_legend_rev <- TRUE
      }
      else if (is.character(rlang::eval_tidy(y, data)) | is.factor(rlang::eval_tidy(y, data))) {
        if (is.character(rlang::eval_tidy(col, data)) | is.factor(rlang::eval_tidy(col, data))) {
          col_legend_rev <- TRUE
        }
        else if (col_legend_place %in% c("b", "t")) col_legend_rev <- TRUE
        else col_legend_rev <- FALSE
        pal <- rev(pal)
      }
      else col_legend_rev <- FALSE
      
      if (rlang::is_null(col_breaks)) col_breaks <- ggplot2::waiver()
      if (rlang::is_null(col_labels)) col_labels <- ggplot2::waiver()
      
      col_scale <- list(
        ggplot2::scale_colour_manual(
          values = pal,
          breaks = col_breaks,
          limits = col_limits,
          labels = col_labels,
          na.value = pal_na,
          guide = ggplot2::guide_legend(
            reverse = col_legend_rev,
            title.position = col_title_position,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE)
        ),
        ggplot2::scale_fill_manual(
          values = pal,
          breaks = col_breaks,
          limits = col_limits,
          labels = col_labels,
          na.value = pal_na,
          guide = ggplot2::guide_legend(
            reverse = col_legend_rev,
            title.position = col_title_position,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE)
        )
      )
    }
  }
  
  ###make plot
  if (!rlang::quo_is_null(x) & !rlang::quo_is_null(y)) {
    if (!rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          y = !!y,
          fill = !!col,
          group = !!group
        ))
    }
    else if (rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          y = !!y,
          fill = "",
          group = !!group
        ))
    }
  } else if (!rlang::quo_is_null(x) & rlang::quo_is_null(y)) {
    if (!rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          fill = !!col,
          group = !!group
        ))
    }
    else if (rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = !!x,
          fill = "",
          group = !!group
        ))
    }
  } else if (rlang::quo_is_null(x) & !rlang::quo_is_null(y)) {
    if (!rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          y = !!y,
          fill = !!col,
          group = !!group
        ))
    }
    else if (rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          y = !!y,
          fill = "",
          group = !!group
        ))
    }
  } else if (rlang::quo_is_null(x) & rlang::quo_is_null(y)) {
    if (!rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          fill = !!col,
          group = !!group
        ))
    }
    else if (rlang::quo_is_null(col)) {
      plot <- data %>%
        ggplot2::ggplot(mapping = ggplot2::aes(
          fill = "",
          group = !!group
        ))
    }
  }
  
  plot <- plot +
    ggplot2::geom_col(
      ggplot2::aes(text = !!text),
      stat = stat,
      position = position,
      alpha = alpha,
      #linewidth = linewidth,
      width = width,
      ...
    )

  
  if (!rlang::quo_is_null(facet)) {
    if (rlang::quo_is_null(facet2)) {
      plot <- plot +
        ggplot2::facet_wrap(
          ggplot2::vars(!!facet),
          scales = facet_scales, labeller = ggplot2::as_labeller(facet_labels),
          ncol = facet_ncol,
          nrow = facet_nrow
        )
    }
    else {
      plot <- plot +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!facet2),
          cols = ggplot2::vars(!!facet),
          labeller = ggplot2::as_labeller(facet_labels),
          scales = facet_scales
        )
    }
  }

  if (!rlang::is_null(x_include)) {
    plot <- plot +
      ggplot2::expand_limits(x = x_include)
  }
  if (!rlang::is_null(y_include)) {
    plot <- plot +
      ggplot2::expand_limits(y = y_include)
  }

  ###Get layer plot
  layer_data <- ggplot2::layer_data(plot)
  
  ###Make x scale based on layer_data
  if (is.character(rlang::eval_tidy(x, data)) | is.factor(rlang::eval_tidy(x, data))) {
    if (rlang::is_null(x_expand)) x_expand <- ggplot2::waiver()
    if (rlang::is_null(x_labels)) x_labels <- ggplot2::waiver()
    
    x_scale <- ggplot2::scale_x_discrete(expand = x_expand, labels = x_labels)
  } else {
    if (facet_scales %in% c("fixed", "free_y")) {
      
      x_vctr <- layer_data %>%
        dplyr::select(tidyselect::starts_with("x")) %>%
        tidyr::pivot_longer(cols = tidyselect::everything()) %>%
        dplyr::pull(.data$value)
      
      if (lubridate::is.Date(rlang::eval_tidy(x, data))) {
        x_vctr <- as.Date(x_vctr, origin = "1970-01-01")
      }
      
      x_min <- x_vctr %>% min(na.rm = TRUE)
      x_max <- x_vctr %>% max(na.rm = TRUE)
      
      if (rlang::is_null(x_limits)) {
        x_limits <- range(c(x_min, x_max))
        if (!rlang::is_null(x_include)) x_limits <- range(c(x_limits, x_include))
        
        if (rlang::is_null(x_breaks)) {
          x_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 3)
          if (x_trans != c("identity")) x_breaks <- scales::breaks_log(n = x_breaks_n, base = 10)(x_limits)
          else x_breaks <- scales::breaks_pretty(n = x_breaks_n)(x_limits)
          
          if (xy_numeric_date) x_limits <- NULL
          else {
            if (x_trans != "identity") x_limits <- NULL
            else if (class(position)[1] == "PositionFill") x_limits <- NULL
            else if (class(position)[1] == "character") {
              if (position[1] == "fill") x_limits <- NULL
              else x_limits <- c(min(x_breaks), max(x_breaks))
            }
            else x_limits <- c(min(x_breaks), max(x_breaks))
          }
        }
        else if (!rlang::is_null(x_breaks)) {
          if (xy_numeric_date) x_limits <- NULL
          else {
            if (is.vector(x_breaks)) {
              if (x_trans != "identity") x_limits <- NULL
              else if (class(position)[1] == "PositionFill") x_limits <- NULL
              else if (class(position)[1] == "character") {
                if (position[1] == "fill") x_limits <- NULL
                else x_limits <- c(min(x_breaks), max(x_breaks))
              }
              else x_limits <- c(min(x_breaks), max(x_breaks))
            }
            else {
              if (x_trans != "identity") x_limits <- NULL
              else if (class(position)[1] == "PositionFill") x_limits <- NULL
              else if (class(position)[1] == "character") {
                if (position[1] == "fill") x_limits <- NULL
                else {
                  x_limits <- list(x_limits) %>%
                    purrr::map(.f = x_breaks) %>%
                    unlist() %>%
                    range()
                }
              }
              else {
                x_limits <- list(x_limits) %>%
                  purrr::map(.f = x_breaks) %>%
                  unlist() %>%
                  range()
              }
            }
          }
        }
      }
      else if (!rlang::is_null(x_limits)) {
        if (is.na(x_limits)[1]) x_limits[1] <- x_min
        if (is.na(x_limits)[2]) x_limits[2] <- x_max
        if (!rlang::is_null(x_include)) x_limits <- range(c(x_limits, x_include))
        
        if (rlang::is_null(x_breaks)) {
          x_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 4)
          if (x_trans != "identity") x_breaks <- scales::breaks_log(n = x_breaks_n, base = 10)(x_limits)
          else x_breaks <- scales::breaks_pretty(n = x_breaks_n)(x_limits)
        }
      }
    }
    else if (facet_scales %in% c("free", "free_x")) {
      if (rlang::is_null(x_breaks)) x_breaks <- ggplot2::waiver()
    }
    
    if (rlang::is_null(x_expand)) {
      if (facet_scales %in% c("fixed", "free_y")) {
        if (xy_numeric_date) {
          x_expand <- c(0.05, 0.05)
        }
        else x_expand <- c(0, 0)
      }
      else x_expand <- c(0.05, 0.05)
    }
    
    if (rlang::is_null(x_labels)) {
      if (is.numeric(rlang::eval_tidy(x, data)) | rlang::quo_is_null(x)) x_labels <- scales::label_comma()
      else if (lubridate::is.Date(rlang::eval_tidy(x, data))) x_labels <- scales::label_date_short()
      else x_labels <- ggplot2::waiver()
    }
    
    if (is.numeric(rlang::eval_tidy(x, data)) | rlang::quo_is_null(x)) {
      x_scale <- ggplot2::scale_x_continuous(
        breaks = x_breaks,
        limits = x_limits,
        expand = x_expand,
        labels = x_labels,
        oob = x_oob, sec.axis = x_sec_axis,
        trans = x_trans
      )
    } else if (lubridate::is.Date(rlang::eval_tidy(x, data))) {
      x_scale <- ggplot2::scale_x_date(
        breaks = x_breaks,
        limits = x_limits,
        expand = x_expand,
        labels = x_labels,
        oob = x_oob, sec.axis = x_sec_axis
      )
    }
  }
  
  plot <- plot +
    x_scale
  
  ###Make y scale based on layer_data
  if (is.character(rlang::eval_tidy(y, data)) | is.factor(rlang::eval_tidy(y, data))) {
    if (rlang::is_null(y_expand)) y_expand <- ggplot2::waiver()
    if (rlang::is_null(y_labels)) y_labels <- ggplot2::waiver()
    
    y_scale <- ggplot2::scale_y_discrete(expand = y_expand, labels = y_labels)
  } else {
    if (facet_scales %in% c("fixed", "free_x")) {
      y_vctr <- layer_data %>%
        dplyr::select(tidyselect::starts_with("y")) %>%
        tidyr::pivot_longer(cols = tidyselect::everything()) %>%
        dplyr::pull(.data$value)
      
      if (lubridate::is.Date(rlang::eval_tidy(y, data))) {
        y_vctr <- as.Date(y_vctr, origin = "1970-01-01")
      }
      
      y_min <- y_vctr %>% min(na.rm = TRUE)
      y_max <- y_vctr %>% max(na.rm = TRUE)
      
      if (rlang::is_null(y_limits)) {
        y_limits <- range(c(y_min, y_max))
        if (!rlang::is_null(y_include)) y_limits <- range(c(y_limits, y_include))
        
        if (rlang::is_null(y_breaks)) {
          y_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 3)
          if (y_trans != c("identity")) y_breaks <- scales::breaks_log(n = y_breaks_n, base = 10)(y_limits)
          else y_breaks <- scales::breaks_pretty(n = y_breaks_n)(y_limits)
          
          if (y_trans != "identity") y_limits <- NULL
          else if (class(position)[1] == "PositionFill") y_limits <- NULL
          else if (class(position)[1] == "character") {
            if (position[1] == "fill") y_limits <- NULL
            else y_limits <- c(min(y_breaks), max(y_breaks))
          }
          else y_limits <- c(min(y_breaks), max(y_breaks))
        }
        else if (!rlang::is_null(y_breaks)) {
          if (is.vector(y_breaks)) {
            if (y_trans != "identity") y_limits <- NULL
            else if (class(position)[1] == "PositionFill") y_limits <- NULL
            else if (class(position)[1] == "character") {
              if (position[1] == "fill") y_limits <- NULL
              else y_limits <- c(min(y_breaks), max(y_breaks))
            }
            else y_limits <- c(min(y_breaks), max(y_breaks))
          }
          else {
            if (y_trans != "identity") y_limits <- NULL
            else if (class(position)[1] == "PositionFill") y_limits <- NULL
            else if (class(position)[1] == "character") {
              if (position[1] == "fill") y_limits <- NULL
              else {
                y_limits <- list(y_limits) %>%
                  purrr::map(.f = y_breaks) %>%
                  unlist() %>%
                  range()
              }
            }
            else {
              y_limits <- list(y_limits) %>%
                purrr::map(.f = y_breaks) %>%
                unlist() %>%
                range()
            }
          }
        }
      }
      else if (!rlang::is_null(y_limits)) {
        if (is.na(y_limits)[1]) y_limits[1] <- y_min
        if (is.na(y_limits)[2]) y_limits[2] <- y_max
        if (!rlang::is_null(y_include)) y_limits <- range(c(y_limits, y_include))
        
        if (rlang::is_null(y_breaks)) {
          y_breaks_n <- ifelse(rlang::quo_is_null(facet), 5, 4)
          if (y_trans != "identity") y_breaks <- scales::breaks_log(n = y_breaks_n, base = 10)(y_limits)
          else y_breaks <- scales::breaks_pretty(n = y_breaks_n)(y_limits)
        }
      }
    }
    else if (facet_scales %in% c("free", "free_y")) {
      if (rlang::is_null(y_breaks)) y_breaks <- ggplot2::waiver()
    }
    
    if (rlang::is_null(y_expand)) {
      if (facet_scales %in% c("fixed", "free_x")) {
        y_expand <- c(0, 0)
      }
      else if (!rlang::is_null(y_include)) {
        if (min(y_include) == 0 | max(y_include) == 0) y_expand <- ggplot2::expansion(mult = c(0, 0.05))
      }
      else y_expand <- c(0.05, 0.05)
    }
    
    if (rlang::is_null(y_labels)) {
      if (is.numeric(rlang::eval_tidy(y, data)) | rlang::quo_is_null(x)) y_labels <- scales::label_comma()
      else if (lubridate::is.Date(rlang::eval_tidy(y, data))) y_labels <- scales::label_date_short()
      else y_labels <- ggplot2::waiver()
    }
    
    if (is.numeric(rlang::eval_tidy(y, data)) | rlang::quo_is_null(y)) {
      y_scale <- ggplot2::scale_y_continuous(
        breaks = y_breaks,
        limits = y_limits,
        expand = y_expand,
        labels = y_labels,
        oob = y_oob, sec.axis = y_sec_axis,
        trans = y_trans
      )
    }
    else if (lubridate::is.Date(rlang::eval_tidy(y, data))) {
      y_scale <- ggplot2::scale_y_date(
        breaks = y_breaks,
        limits = y_limits,
        expand = y_expand,
        labels = y_labels,
        oob = y_oob, sec.axis = y_sec_axis
      )
    }
  }
  
  plot <- plot +
    y_scale
  
  #make the plot
  plot <- plot +
    coord +
    col_scale+
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_title,
      y = y_title,
      col = col_title,
      fill = col_title,
      caption = caption
    )+
    theme

  ###adjust legend
  if (col_legend_place %in% c("b", "t")) {
    plot <- plot +
      ggplot2::theme(legend.direction = "horizontal")
    
    if (is.numeric(rlang::eval_tidy(col, data))) {
      plot <- plot +
        ggplot2::theme(legend.key.width = grid::unit(0.66, "cm")) +
        ggplot2::theme(legend.text.align = 0.5)
    }
    
    if (col_legend_place == "b") {
      plot <- plot +
        ggplot2::theme(legend.position = "bottom")
    }
    else if (col_legend_place == "t") {
      plot <- plot +
        ggplot2::theme(legend.position = "top")
    }
  }
  
  else if (col_legend_place == "n" | rlang::quo_is_null(col)) {
    plot <- plot +
      ggplot2::theme(legend.position = "none")
  }
  else if (col_legend_place == "l") {
    plot <- plot +
      ggplot2::theme(legend.position = "left")
  }
  
  else if (col_legend_place == "r") {
    plot <- plot +
      ggplot2::theme(legend.position = "right")
  }
  #ce<-current_env()
  #poke_to_global(environment = ce)
  if(rlang::quo_is_null(text)==F){
    if(is.numeric(data%>%pull(!!enexpr(text)))){
      data <- data%>%mutate(
        !!enexpr(text) := round(!!enexpr(text),digits = digits)
      )
    }
    plot<-plot+
      ggplot2::geom_label(aes(label=y_labels(!!enexpr(text))),
                          col="black",#x=!!enexpr(x),y=!!enexpr(y),
                          data = data,size=ifelse(is.null(labelsize),fontsize/4.5,labelsize),
                          show.legend = F,position = position)
  }
  #return beautiful plot
  return(plot)
}
