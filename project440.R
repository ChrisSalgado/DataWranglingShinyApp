library(shiny)
library(tidyverse)
library(readr)

delimiters = c("Comma" = ",", "Tab" = "\t", "Semicolon" = ";", "Pipe" = "|")

ui <- fluidPage(
  
  titlePanel("Wrangle Your Data!"),
  
  sidebarLayout(
    sidebarPanel(
      # Let user input their data url
      textInput(inputId = "user_data",
                label = "Paste CSV URL"),
      # Let user choose appropriate delimiter
      selectInput(inputId = "choose_delimiter",
                  label = "Choose Delimiter",
                  choices = delimiters),
      # Let user load in the data
      actionButton(inputId = "load_data",
                   label = "Load Data"),
      br(),
      br(),
      br(),
      # Let user choose a starting row
      numericInput(inputId = "start",
                   label = "Start Row",
                   value = 1,
                   min = 1),
      # Let user choose an end row
      numericInput(inputId = "end",
                   label = "End Row",
                   value = 1,
                   min = 1),
      # Let user choose a starting column
      numericInput(inputId = "start_col",
                   label = "Start Column",
                   value = 1,
                   min = 1),
      # Let user choose an end column
      numericInput(inputId = "end_col",
                   label = "End Column",
                   value = 1,
                   min = 1),
      # Allow user to remove rows with NA values
      checkboxInput(inputId = "remove_na",
                    label = "Remove Rows With NA Values"),
      # Allow user to remove duplicate rows
      checkboxInput(inputId = "remove_duplicates",
                    label = "Remove Duplicates")
    ),
    mainPanel(
      tableOutput("view_table")
    )
  )
)

server <- function(input, output) {
  
  # Read in the data
  read_data = eventReactive(input$load_data, {
    read_delim(input$user_data,
             delim = input$choose_delimiter)
  })
  
  data = reactive({
    df = read_data()
    
    # Remove any rows that have NA in any column
    if (input$remove_na) {
      df = df %>%
        drop_na()
    }
    
    # Remove any rows that are duplicates
    if (input$remove_duplicates) {
      df = df %>%
        distinct()
    }
    
    # Dictates which rows are shown
    df = df[min(input$start, nrow(df)):
              min(input$end, nrow(df)), , drop = FALSE]
    
    # Dictates which columns are shown
    df = df[, min(input$start_col, ncol(df)):
              min(input$end_col, ncol(df)), drop = FALSE]
    
    df
  })
  
  # Output the wrangled data table
  output$view_table = renderTable({
    data()
  })

}

shinyApp(ui = ui, server = server)
