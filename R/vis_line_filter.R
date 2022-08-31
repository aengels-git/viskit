#' @title Shiny App to filter a line ggplot by an arbitrary variable before displaying the result.
#'
#' @description Create a Line plot with a wrapper around the ggplot2::geom_line function.
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
#' @param linewidth Width of the line. A number 0 upwards.
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
#' @param filter_var A numeric or categorial Variable to filter by before plotting  
#' @param aggregate Aggregate by col and x value and calculate the mean for each combination (default=True)
#' 
#' @export
#' @examples
#' vis_line_filter(economics,x = date ,y = unemploy,linewidth = 1,y_include = 0,
#' x_breaks = scales::breaks_width(width = "5 year"),filter_var = uempmed )

vis_line_filter<-function(...){
  require(viskit)
  require(prepkit)
  filter_var<-ifelse(deparse(substitute(filter_var)) == "NULL", "NULL", as_name(substitute(filter_var)))
  if(filter_var=="NULL"){
    stop("Please provide a filter_var argument!")
  }
  #Obtain the list of arguments with which the function was called!
  list_of_arguments<-as.list(match.call())
  
  #filter_var should be the same as either x or col
  # print(filter_var)
  # if((filter_var == as_name(substitute(x)) || filter_var == as_name(substitute(col)))==F){
  #   stop("filter_var should be the same as either x or col")
  # }
  #Modify the values of the arguments
  var<-data%>%pull(!!parse_expr(filter_var))
  
  if(is.factor(var) || is.character(var)){
    filter_element <- selectInput(
      inputId = "filter",
      multiple = T,
      label = "Filter variable:",
      choices = unique(data%>%pull(!!parse_expr(filter_var))),
      selected = unique(data%>%pull(!!parse_expr(filter_var)))[1])
  } else {
    filter_element <- sliderInput(
      inputId = "filter",
      label = "Filter variable:",
      min = min(var),
      max = max(var),
      value = c(min(var),max(var)))
  }
  ui <- fluidPage(
    column(2,
           filter_element,
           numericInput(inputId = "width",label = "Width of the exported plot:",value = 10,min = 1,max = 20),
           numericInput(inputId = "height",label = "Height of the exported plot:",value = 6,min = 1,max = 20),
           downloadButton('downloadPlot','Download Plot')
    ),
    column(10,
           plotOutput(outputId = "plot")
    )
  )
  server <- function(input, output, session) {
    appData  <- reactiveValues(index=0) 

    observeEvent(list(input$filter,input$width,input$height),{
      output$plot<-renderPlot({
        if(is.factor(var) || is.character(var)){
          data <- data%>%filter(!!parse_expr(filter_var) %in% input$filter)
        } else {
          data <- data%>%
            filter(!!parse_expr(filter_var) >= as.numeric(input$filter[1]) & 
                     !!parse_expr(filter_var) <= as.numeric(input$filter[2]))
        }
        expressions<-enexprs(x,col)
        expressions<-expressions[map_lgl(expressions,~is_expression(.x) & is_null(.x)==F)]
        
        if(aggregate==T){
          data <- prep_mean_table(data,!!!expressions,outcomes = as_string(list_of_arguments$y))
        }
        list_of_arguments$data<-expr(data)
        list_of_arguments$filter_var<-NULL
        appData$plot<-do.call("vis_line",list_of_arguments[names(list_of_arguments)!=""])
        return(appData$plot)
      },height = input$height*72,width=input$width*72)
    })
    output$downloadPlot <- downloadHandler(
      filename = function(){
        appData$index<-appData$index+1
        return(paste0("vis_line_plot_",appData$index,'.png'))
        },
      content = function(file){
        ggsave(file,plot=appData$plot,height = input$height,width=input$width)
      }
    )
  }
  shinyApp(ui, server)
  
}

new_formals<-as.list(formals(vis_line))
new_formals<-append(new_formals,list("filter_var"=NULL,"aggregate"=TRUE))
current_enviroment <- rlang::current_env()
formals(vis_line_filter, envir = current_enviroment)<-new_formals