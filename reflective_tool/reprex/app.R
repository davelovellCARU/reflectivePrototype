library(shiny)
library(stringr)




ui <- fluidPage(
  titlePanel("What's going on here?"),
  h4("If you put matching responses in these selectize boxes, they both break. Try it:"),
  uiOutput("example")
  )

server <- function(input, output) {
  
  ### VARIABLES AND FUNCTIONS ####
  ## Variables -------------------------------------------------------------
  n_rows <- 5
  
  ## Functions ------------------------------------------------------------------
  
  ### makeFeelingInputs() generates input rows for the feelings colum :::::::::::
  makeFeelingInputs <- function(tab = NULL, rows = NULL, choices = NULL, values = NULL) {
    # Stop conditions
    stopifnot(is.character(tab), is.numeric(rows), rows > 0)
    
    # make list
    lapply(1:rows,
           function(number) {
             selectizeInput(inputId = paste0(
               str_replace_all(tab, "[:space:]", "_"),
               "_input_",
               as.character(number)),
               label = NULL,
               choices = choices,
               options = list(create = TRUE),
               multiple = FALSE,
               selected = values[number])
           }) -> inputList
    
    return(inputList)
  }
  
  ### Selectize Reactives
  
  vars <- reactiveValues()
  
  ## make userChoices - a reactive variable that updates when selectize inputs are used
  vars$userChoices <- character()
  choice_added <- reactive(vars$userChoices)

  #### Observe both selectize inputs #### ----------------------------------------  
  observeEvent(input$example_input_1, {
    vars$userChoices <- unique(c(vars$userChoices, input$example_input_1))
    vars$example_input_1 <- input$example_input_1})
  
  observeEvent(input$example_input_2, {
    vars$userChoices <- unique(c(vars$userChoices, input$example_input_2))
    vars$example_input_2 <- input$example_input_2})
  
  output$example <- renderUI({
    makeFeelingInputs(
      tab = "example",
      rows = 2,
      choices = choice_added(),
      values = 
        c(vars$example_input_1,
          vars$example_input_2
          )
      )
  })
  
  outputOptions(output, "example", suspendWhenHidden = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
