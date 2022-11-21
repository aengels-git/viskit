
######################################################
# Eigentliche Farb App
######################################################


colorUI <- function(id) {
  tagList(
    uiOutput(NS(id,"palette_ui")),
    uiOutput(NS(id,"color_ui")),
    uiOutput(NS(id,"hex_button"))
  )
}


colorServer <- function(id,variable) {
  moduleServer(id, function(input, output, session) {
    #Einzigartige Ausprägungen werden die Label
    returnValues <- reactiveValues()
    old_value<-NULL
    last_click<-Sys.time()
    current_enviroment<-current_env()
    #Select acceptable color palettes:
    output$palette_ui<-renderUI({
      accepted_pal<-accepted_palettes(palette_data,
                                      length(extract_codes(variable)))%>%pull(palette)
      tagList(
        selectInput(NS(id,"palette"),
                    label = "Colorscheme:",
                    choices = accepted_pal,
                    selected = accepted_pal[1]),
        selectInput(NS(id,"invert_status"),
                    label = "Reverse Color Order:",
                    choices = c(TRUE,FALSE),
                    selected = FALSE)
      )
    })
    
    
    observeEvent(
      list(input$palette,input$invert_status),{
        req(input$palette)
        #print("Observer 1")
        palette<-accepted_palettes(palette_data,length(extract_codes(variable)))%>%
          filter(palette==input$palette)%>%
          pull(colors)%>%unlist()
        if(input$invert_status){
          palette<-rev(palette)
        }
        returnValues$hex_colors<-palette
        output$color_ui<-renderUI({
          palette_colors<-palette
          
          pmap(list(extract_codes(variable),
                    palette_colors,
                    seq(1,length(palette_colors))),function(lab,val,i){
                      colourInput(inputId = NS(id,paste0("col",i)),
                                  label = lab,
                                  value = val)
                    })
          
        })
        
      })
    
    try(
      observeEvent(map(map_chr(seq(1,length(unique(variable))),~paste0("col",.x)), function(x) {
        input[[x]]
      }),{
        
        req(input$palette)
        req(input$col1)
        #Nur aktualisieren wenn der letzte Klick mindestens 2 Sekunden zurück liegt
        #print(Sys.time()-last_click)
        req((Sys.time()-last_click)>2)
        #print("Observer 2a")
        env_poke(env = current_enviroment,nm = "last_click",value = Sys.time())
        if(is_null(old_value)== FALSE && old_value==input$palette){
          returnValues$hex_colors<-map_chr(map_chr(seq(1,length(returnValues$hex_colors)),~paste0("col",.x)), function(x) input[[x]])
          req(FALSE)
        }
        #print("Observer 2b")
        #old_value<-input$palette
        env_poke(env = current_enviroment,nm = "old_value",value = input$palette)
        output$color_ui<-renderUI({
          palette_colors<-returnValues$hex_colors
          pmap(list(extract_codes(variable),
                    palette_colors,
                    seq(1,length(palette_colors))),function(lab,val,i){
                      
                      colourInput(inputId = NS(id,paste0("col",i)),
                                  label = lab,
                                  value = val)
                    })
          
        })
        returnValues$hex_colors<-map_chr(map_chr(seq(1,length(returnValues$hex_colors)),~paste0("col",.x)), function(x) input[[x]])
        
      })
    )
    
    # observeEvent(returnValues$hex_colors,{
    #   print("Observer 3")
    #   req(returnValues$hex_colors)
    #   output$hex_button<-renderUI({
    #     string<-to_r_vector(returnValues$hex_colors)
    #  
    #     rclipButton(NS(id,"button"), "Hex Werte kopieren", 
    #                 clipText = process("string"))
    #     
    #   })
    # })
    return(returnValues)
  })
}  

#' color_fct
#'
#' @param variable a factor or character variable
#' @param include_color_previews boolean whether to include a preview table (takes a couple of seconds to render)
#'
#' @return
#' @export
#'
#' @examples
color_fct<- function(variable,include_color_previews=T) {
  if(include_color_previews){
    preview_table <- get_preview_table(palette_data = palette_data,n_cols = 5)
  } else {
    preview_table <-flextable(data.frame(Info="Set include_color_previews=T to see previews!"))
  }
  ui <- fluidPage(
    column(2,
           h3("Colors:"),
           colorUI(id = "color1")),
    column(10,
           tabsetPanel(type = "tabs",
                       tabPanel("Plot", 
                                h3("Figure:"),
                                plotOutput("color_plot"),
                                br(),
                                h3("Code and hex values:"),
                                verbatimTextOutput("hex_text"),
                                verbatimTextOutput("scale_code")
                                
                       ),
                       tabPanel("Color Palettes", 
                                uiOutput("preview_table")
                       )))
  )
  server <- function(input, output, session) {
    output$preview_table <- renderUI({
      preview_table %>%
        htmltools_value()
    })
    
    reactives <- reactiveValues(
      hex_colors = NULL
    )
    hex_cols<-colorServer(id = "color1",variable=variable)
    observe({
      reactives$hex_colors <- hex_cols$hex_colors
    })
    observeEvent(reactives$hex_colors,{
      req(reactives$hex_colors)
      output$hex_text<-renderPrint({
        cat(to_r_vector(reactives$hex_colors))
      })
      # Create the scale code
      output$scale_code<-renderPrint({
        parse_expr(glue("scale_fill_manual(values={to_r_vector(reactives$hex_colors)})"))
      })
      
      output$color_plot<-renderPlot({
        show_cols(rev(reactives$hex_colors))
      })
    })
    
  }
  shinyApp(ui, server)
}



#' color_gg
#'
#' @param gg_plot a  gg plot 
#' @param include_color_previews boolean whether to include a preview table (takes a couple of seconds to render)
#'
#' @return
#' @export
#'
#' @examples
color_gg<- function(gg_plot,include_color_previews=T) {
  if(include_color_previews){
    preview_table <- get_preview_table(palette_data = palette_data,n_cols = 5)
  } else {
    preview_table <-flextable(data.frame(Info="Set include_color_previews=T to see previews!"))
  }
  
  tibble_data<-extract_col_fill_var(gg_plot)
  variable<-tibble_data%>%pull(1)
  
  
  ui <- fluidPage(
    fluidRow(
      column(2,
             h3("Colors:"),
             colorUI(id = "color1")),
      column(10,
             tabsetPanel(type = "tabs",
                         tabPanel("Plot", 
                                  
                                  h3("Figure:"),
                                  plotOutput("color_plot"),
                                  br(),
                                  h3("Code and hex values:"),
                                  verbatimTextOutput("hex_text"),
                                  verbatimTextOutput("scale_code")
                                  
                         ),
                         tabPanel("Color Palettes", 
                                  uiOutput("preview_table")
                         )))
    )
  )
  server <- function(input, output, session) {
    output$preview_table <- renderUI({
      preview_table %>%
        htmltools_value()
    })
    reactives <- reactiveValues(
      hex_colors = NULL
    )
    hex_cols<-colorServer(id = "color1",variable=variable)
    observe({ 
      reactives$hex_colors <- hex_cols$hex_colors 
      
    })
    observeEvent(reactives$hex_colors,{
      req(reactives$hex_colors)
      #Create the hex value vector
      output$hex_text<-renderPrint({
        cat(to_r_vector(reactives$hex_colors))
      })
      # Create the scale code
      output$scale_code<-renderPrint({
        col_fill_var_to_scale(tibble_data = tibble_data,hex_cols = reactives$hex_colors)
      })
      # Create the plot
      output$color_plot<-renderPlot({
        
        gg_plot+
          eval(col_fill_var_to_scale(tibble_data = tibble_data,hex_cols = reactives$hex_colors))
      })
    })
    
  }
  shinyApp(ui, server)
}