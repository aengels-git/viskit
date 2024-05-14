library(viskit)
library(prepkit)
library(tidyverse)
library(shiny)
data <- diamonds

# To Do 
# 1) selected item übergeben
# 2) test if modified plotting function can be used. 

generate_shiny <- function(data,ui_init, plot_function,run=TRUE,...){
  plot_function = substitute(plot_function)
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          tabPanel("Aesthetics", 
                   br(),
                   uiOutput("ui"),
                   actionButton("render",label = "plot")),
          tabPanel("Plot Settings", 
                   br(),
                   numericInput(inputId = "height",
                                label = "Height:",
                                value = 400,
                                min = 100,
                                max = 1500,
                                step = 50),
                   numericInput(inputId = "width",
                                label = "Width:",
                                value = 800,
                                min = 100,
                                max = 1500,
                                step = 50),
                   actionButton("render",label = "plot")
                   ),
        )
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  )
  
  server <- function(input, output, session) {
    ui_list <- pmap(ui_init,function(id,choices, selected){
      selectInput(inputId = id,
                  choices = choices,
                  selected = selected,
                  label = glue("{id}-Attribut"))
    })
    output$ui <- renderUI({
      ui_list
    })
    
    observeEvent(input$render, {
      output$plot <- renderPlot({
        dynamic_code <- pmap(ui_init,~glue("{.x} = !!parse_expr(input${.x})"))%>%str_c(.,collapse = ",")
        print(glue("vis_scatter(data,{dynamic_code},...)"))
        parse_expr(glue("{plot_function}(data,{dynamic_code},...)"))%>%eval()
      }, height = input$height, width=input$width)
    })
    
  }
  if(run==TRUE){
    shinyApp(ui, server)
  } else {
    return(list(ui,server))
  }
}


tab <- prep_freq_table(diamonds,cut)
vis_modified <- function(...){
  vis_barplot(...)+geom_point()
}
generate_shiny(data=tab,
               ui_init = list(
                 list("x","y","col","text"),
                 list(names(tab),names(tab),names(tab),names(tab)),
                 list("cut","prop","cut","prop")
               ),
               plot_function = vis_modified,
               y_labels=scales::percent)
