library(shiny)
library(stringr)


## Variables -------------------------------------------------------------
statusChoices <- c("started", "ended", "changed", "stayed the same")

## Functions ------------------------------------------------------------------
### whitespace() generates whitespace. A bit hacky but stops some ugliness:::::
whitespace <- function() column(1, div(style = "height:100px"))

### makeNameInputs() generates input rows for the activty name column :::::::::
makeNameInputs <- function(tab = NULL, rows = 5) {
    # Stop conditions
    stopifnot(is.character(tab), is.numeric(rows), rows > 0)
    
    # make list
    
    lapply(1:rows,
           function(number) {
               textInput(inputId = paste0(str_replace_all(tab, "[:space:]", "-"),
                                          "_name_",
                                          as.character(number)),
                         label = {
                             if(number == 1) "Activity"
                             else NULL
                                 })
           }) -> inputList
    
    return(inputList)
}

### makeStatusInputs() generates input rows for the activty status column :::::
makeStatusInputs <- function(tab = NULL, rows = 5, choices = statusChoices) {
    # Stop conditions
    stopifnot(is.character(tab), is.numeric(rows), rows > 0, is.character(choices))
    
    # make list
        lapply(1:rows,
           function(number) {
               selectInput(inputId = paste0(
                   str_replace_all(tab, "[:space:]", "-"),
                   "_status_",
                   as.character(number)),
                         label = {
                             if(number == 1) "This activity has:"
                             else NULL
                         },
                         choices = statusChoices)
           }) -> inputList
    
    return(inputList)
}

### makeFeelingInputs() generates input rows for the feelings colum :::::::::::
makeFeelingInputs <- function(tab = NULL, rows = 5) {
    # Stop conditions
    stopifnot(is.character(tab), is.numeric(rows), rows > 0)
    
    # make list
    lapply(1:rows,
           function(number) {
               selectizeInput(inputId = paste0(
                   str_replace_all(tab, "[:space:]", "-"),
                   "_feeling_",
                   as.character(number)),
                           label = {
                               if(number == 1) "I feel:"
                               else NULL
                           },
                           choices = NULL,
                           options = list(create = TRUE))
           }) -> inputList
    
    return(inputList)
}

### makeInputsGrid() generates the grid of all the inputs :::::::::::::::::::::
makeInputsTab <- function(tabName) {
    stopifnot( is.character(tabName) )
    
    renderUi({
        tabPanel(str_to_title(tabName),
                fixedRow(
                column(4, makeNameInputs(tab = str_to_lower(tabName))),
                column(5, makeStatusInputs(tab = str_to_lower(tabName))),
                column(3, makeFeelingInputs(tab = str_to_lower(tabName))),
                whitespace()
            )
        )
    }) -> myTabPanel
    
    return(myTabPanel)
    
}

ui <- fluidPage(
    
    titlePanel("Reflecting on Change"),
    
    splitLayout(
        
        tabsetPanel(
            makeInputsTab("community"),
            makeInputsTab("Discipleship"),
            makeInputsTab("Communal Worship"),
            makeInputsTab("Sacraments"),
            makeInputsTab("Evangelism"),
            makeInputsTab("Prayer")
        ),
        
        imageOutput("viz", width = "300px", height = "300px")
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {

    ### Placeholder Graphics --------------------------------
    output$viz <- renderPlot(plot(1:50, 1:50))
    output$test <- renderPlot(plot(50:1, 1:50))
    
    ### Selectize Reactives
    
    userChoices <- reactiveValues(character())
    
    ### Watch all selectize inputs
    
    observeEvent({input$})
    userChoices <- c(userChoices, )
    
    ### Render UI
    
}

# Run the application 
shinyApp(ui = ui, server = server)
