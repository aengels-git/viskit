library(viskit)
library(prepkit)
library(tidyverse)
library(shiny)
data <- diamonds






print(plot_function)


generate_shiny <- function(data,ui_init, plot_function,...){
  plot_function = substitute(plot_function)

  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        uiOutput("ui"),
        numericInput(inputId = "height",
                     label = "Height:",
                     value = 400,
                     min = 100,
                     max = 1500,
                     step = 50),
        numericInput(inputId = "width",
                     label = "Width:",
                     value = 400,
                     min = 100,
                     max = 1500,
                     step = 50),
        actionButton("render",label = "plot")
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  )
  
  server <- function(input, output, session) {
    ui_list <- pmap(ui_init,function(id,choices){
      selectInput(inputId = id,
                  choices = choices,
                  selected = choices[1],
                  label = glue("{id}-Attribut"))
    })
    output$ui <- renderUI({
      ui_list
    })
    
    observeEvent(input$render, {
      output$plot <- renderPlot({
        dynamic_code <- pmap(init,~glue("{.x} = !!parse_expr(input${.x})"))%>%str_c(.,collapse = ",")
        print(glue("vis_scatter(data,{dynamic_code},...)"))
        parse_expr(glue("{plot_function}(data,{dynamic_code},...)"))%>%eval()
      }, height = input$height, width=input$width)
    })
    
  }
  shinyApp(ui, server)
}


tab <- prep_freq_table(diamonds,cut)
generate_shiny(data=tab,
               ui_init = list(
                 list("x","y","col"),
                 list(names(tab),names(tab),names(tab))
               ),
               plot_function = vis_barplot,
               y_include=10000)








run_data <- read_csv("../viskit/runs.csv")
run_data <- run_data%>%filter(`Top n most probable cases`=="None")

vis_scatter_interactive <- function(data,x,y){
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "x",
                    choices = names(data),
                    selected = x,
                    label = "X-Dimension:"),
        selectInput(inputId = "y",
                    choices = names(data),
                    selected = y,
                    label = "Y-Dimension:"),
        selectInput(inputId = "col",
                    choices = c("NULL",names(data)),
                    selected = "NULL",
                    label = "Color:"),
        selectInput(inputId = "facet",
                    choices = c("NULL",names(data)),
                    selected = "NULL",
                    label = "Facet X-Dimension:"),
        selectInput(inputId = "facet2",
                    choices = c("NULL",names(data)),
                    selected = "NULL",
                    label = "Facet Y-Dimension:"),
        numericInput(inputId = "height",
                     label = "Height:",
                     value = 400,
                     min = 100,
                     max = 1500,
                     step = 50),
        numericInput(inputId = "width",
                     label = "Width:",
                     value = 400,
                     min = 100,
                     max = 1500,
                     step = 50)
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  )
  
  server <- function(input, output, session) {
    observeEvent(list(input$facet, input$x, input$y, input$height, input$width),{
      output$plot <- renderPlot({
        vis_scatter(data, 
                    x = !!parse_expr(input$x),
                    y = !!parse_expr(input$y),
                    col = !!parse_expr(input$col),
                    facet = !!parse_expr(input$facet), 
                    facet2 = !!parse_expr(input$facet2),
                    y_include = 0.6)
      }, height = input$height, width=input$width)
    })
    
  }
  
  shinyApp(ui, server)
}
names(run_data) <- str_replace_many(names(run_data),patterns = c(" "),c("_"))
vis_scatter_interactive(run_data,x = "ATC_Relator_Settings",y = "AUC")

vis_scatter(data, x = cut,y = price,facet = !!parse_expr("color"), facet2 = clarity)
