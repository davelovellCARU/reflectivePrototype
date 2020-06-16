library(shiny)


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
               textInput(inputId = paste0(tab, "_name_", as.character(number)),
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
               selectInput(inputId = paste0(tab, "_status_", as.character(number)),
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
               selectizeInput(inputId = paste0(tab, "_feeling_", as.character(number)),
                           label = {
                               if(number == 1) "I feel:"
                               else NULL
                           },
                           choices = NULL,
                           options = list(create = TRUE))
           }) -> inputList
    
    return(inputList)
}


ui <- fluidPage(
    
    titlePanel("Reflecting on Change"),
    
    splitLayout(
        
        tabsetPanel(
            ## Community Tab ------------------------------------------------
            tabPanel("Community",
                     fixedRow(
                         column(4, makeNameInputs(tab = "community")),
                         column(5, makeStatusInputs(tab = "community")),
                         column(3, makeFeelingInputs(tab = "community")),
                         whitespace()
                         )
                     ),
            ## Dixipleship Tab ----------------------------------------------
            tabPanel("Discipleship"),
            ## Communal Worship Tab -----------------------------------------
            tabPanel("Communal Worship"),
            ## Sacraments Tab -----------------------------------------------
            tabPanel("Sacraments"),
            ## Evangelism Tab -----------------------------------------------
            tabPanel("Evangelism"),
            ## Prayer Tab ---------------------------------------------------
            tabPanel("Prayer")
        ),
        
        imageOutput("viz", width = "300px", height = "300px")
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$viz <- renderPlot(plot(1:50, 1:50))
    output$test <- renderPlot(plot(50:1, 1:50))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
