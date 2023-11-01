#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("selectize Behavior"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput("dropdown0",
                     label = "Uses Observe Event, Trigger onChange",
                     choices = c(0, 1, 2),
                     multiple = TRUE,
                     options = list(onDropdownOpen = I("function(value) { Shiny.setInputValue('ev_click', Math.random()); }"))
                       
      ),
      selectizeInput("dropdown0a",
                     label = "Uses Observe Event, Trigger onDropdown",
                     choices = 0,
                     multiple = TRUE,
                     options = list(onDropdownOpen = I("function(value) { Shiny.setInputValue('ev_drop', Math.random()); }"))

      ),
      selectizeInput("dropdown1",
                     label = "Dropdown 1",
                     choices = 0,
                     multiple = TRUE
      ),
      selectizeInput("dropdown1a",
                     label = "Dropdown 1a",
                     choices = 0,
                     multiple = TRUE
      ),
      selectizeInput("dropdown2",
                     label = "Dropdown 2",
                     choices = 0,
                     multiple = TRUE
      )
    ),
    
    # Display the input values
    mainPanel(
      textOutput("sel_0"),
      textOutput("sel_0a"),
      textOutput("sel_1"),
      textOutput("sel_1a"),
      textOutput("sel_2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observeEvent(input$ev_click, {
    choices <- ifelse(is.null(input$dropdown0), 0, as.numeric(input$dropdown0) + 1)
    updateSelectizeInput(session,
                         "dropdown0",
                         choices = choices,
                         selected = input$dropdown0,
                         server = TRUE)
  })
  
  observeEvent(input$ev_drop, {
    choices_0a <- ifelse(is.null(input$dropdown0a), 0, as.numeric(input$dropdown0a) + 1)
    updateSelectizeInput(session,
                         "dropdown0a",
                         choices = choices_0a,
                         selected = input$dropdown0a,
                         server = TRUE)
  })
  
  observe({
    choices <- ifelse(is.null(input$dropdown1), 0, as.numeric(input$dropdown1) + 1)
    updateSelectizeInput(session,
                         "dropdown1",
                         choices = choices,
                         selected = NULL,
                         server = TRUE)
  })
  
  observe({
    choices <- ifelse(is.null(input$dropdown1a), 0, as.numeric(input$dropdown1a) + 1)
    updateSelectizeInput(session,
                         "dropdown1a",
                         choices = choices,
                         selected = input$dropdown1a,
                         server = TRUE)
  })
  
  observe({
    updateSelectizeInput(session,
                         "dropdown2",
                         choices = c(1,2),
                         selected = 1,
                         server = TRUE)
  })
  
  output$sel_0 <- renderText({
    ifelse(is.null(input$dropdown0), "Dropdown 0 value: NULL", paste("Dropdown 0 value:", paste(input$dropdown0, collapse = ", ")))
  })
  
  output$sel_0a <- renderText({
    ifelse(is.null(input$dropdown0a), "Dropdown 0a value: NULL", paste("Dropdown 0a value:", paste(input$dropdown0a, collapse = ", ")))
  })
  
  output$sel_1 <- renderText({
    ifelse(is.null(input$dropdown1), "Dropdown 1 value: NULL", paste("Dropdown 1 value:", paste(input$dropdown1, collapse = ", ")))
  })
  
  output$sel_1a <- renderText({
    ifelse(is.null(input$dropdown1a), "Dropdown 1a value: NULL", paste("Dropdown 1a value:", paste(input$dropdown1a, collapse = ", ")))
  })
  
  output$sel_2 <- renderText({
    ifelse(is.null(input$dropdown2), "Dropdown 2 value: NULL", paste("Dropdown 2 value:", paste(input$dropdown2, collapse = ", ")))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
