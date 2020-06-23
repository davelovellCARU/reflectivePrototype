library(shiny)
library(stringr)
library(tibble)
library(dplyr)


### VARIABLES AND FUNCTIONS ####
## Variables -------------------------------------------------------------
statusChoices <- c("started", "ended", "changed", "stayed the same")
n_rows <- 5

## Functions ------------------------------------------------------------------
### whitespace() generates whitespace. A bit hacky but stops some ugliness:::::
whitespace <- function() column(1, div(style = "height:100px"))

### makeNameInputs() generates input rows for the activty name column :::::::::
makeNameInputs <- function(tab = NULL, rows = NULL) {
   # Stop conditions
   stopifnot(is.character(tab), is.numeric(rows), rows > 0)
   
   # make list
   
   lapply(1:rows,
          function(number) {
             textInput(inputId = paste0(str_replace_all(tab, "[:space:]", "_"),
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
makeStatusInputs <- function(tab = NULL, rows = NULL, choices = statusChoices) {
   # Stop conditions
   stopifnot(is.character(tab), is.numeric(rows), rows > 0, is.character(choices))
   
   # make list
   lapply(1:rows,
          function(number) {
             selectInput(inputId = paste0(
                str_replace_all(tab, "[:space:]", "_"),
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

### makeInputsGrid() generates the grid of all the inputs :::::::::::::::::::::
makeInputsTab <- function(tabName, choices = NULL, values = NULL) {
   stopifnot( is.character(tabName) )
   
   # tabPanel(id = str_to_title(tabName),
            fixedRow(
               column(4, makeNameInputs(tab = str_to_lower(tabName), rows = n_rows)),
               column(5, makeStatusInputs(tab = str_to_lower(tabName), rows = n_rows)),
               column(3, uiOutput(tabName %>% str_to_lower %>% str_replace_all("[:space:]", "_")) ),
               # column(3, makeFeelingInputs(tab = str_to_lower(tabName), rows = n_rows, choices = choices, values = values)),
               whitespace()
            # )
   ) -> myTabPanel
   
   return(myTabPanel)
}




ui <- fluidPage(
    
    titlePanel("Reflecting on Change"),
    
    tabsetPanel(
       tabPanel("Community", makeInputsTab("Community")),
       tabPanel("Discipleship", makeInputsTab("Discipleship")),
       tabPanel("Communal Worship", makeInputsTab("Communal Worship")),
       tabPanel("Sacraments", makeInputsTab("Sacraments")),
       tabPanel("Evangelism", makeInputsTab("Evangelism")),
       tabPanel("Social Action", makeInputsTab("Social Action")),
       tabPanel("Prayer", makeInputsTab("Prayer"))
            )
    )

server <- function(input, output) {
   
   ### makeFeelingInputs() generates input rows for the feelings colum :::::::::::
   makeFeelingInputs <- function(tab = NULL, rows = NULL, choices = NULL, values = NULL) {
      # Stop conditions
      stopifnot(is.character(tab), is.numeric(rows), rows > 0)
      
      # make list
      lapply(1:rows,
             function(number) {
                selectizeInput(inputId = paste0(
                   str_replace_all(tab, "[:space:]", "_") %>% 
                      str_to_lower,
                   "_feeling_",
                   as.character(number)),
                   label = {
                      if(number == 1) "I feel:"
                      else NULL
                   },
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
   
   #### Event observers for selectize inputs #######
   observeEvent(input$communal_worship_feeling_1, {
       vars$userChoices <- c(vars$userChoices, input$communal_worship_feeling_1) %>% unique
       vars$communal_worship_feeling_1 <- input$communal_worship_feeling_1})
   observeEvent(input$communal_worship_feeling_2, {
       vars$userChoices <- c(vars$userChoices, input$communal_worship_feeling_2) %>% unique
       vars$communal_worship_feeling_2 <- input$communal_worship_feeling_2})
   observeEvent(input$communal_worship_feeling_3, {
       vars$userChoices <- c(vars$userChoices, input$communal_worship_feeling_3) %>% unique
       vars$communal_worship_feeling_3 <- input$communal_worship_feeling_3})
   observeEvent(input$communal_worship_feeling_4, {
       vars$userChoices <- c(vars$userChoices, input$communal_worship_feeling_4) %>% unique
       vars$communal_worship_feeling_4 <- input$communal_worship_feeling_4})
   observeEvent(input$communal_worship_feeling_5, {
       vars$userChoices <- c(vars$userChoices, input$communal_worship_feeling_5) %>% unique
       vars$communal_worship_feeling_5 <- input$communal_worship_feeling_5})
   observeEvent(input$community_feeling_1, {
       vars$userChoices <- c(vars$userChoices, input$community_feeling_1) %>% unique
       vars$community_feeling_1 <- input$community_feeling_1})
   observeEvent(input$community_feeling_2, {
       vars$userChoices <- c(vars$userChoices, input$community_feeling_2) %>% unique
       vars$community_feeling_2 <- input$community_feeling_2})
   observeEvent(input$community_feeling_3, {
       vars$userChoices <- c(vars$userChoices, input$community_feeling_3) %>% unique
       vars$community_feeling_3 <- input$community_feeling_3})
   observeEvent(input$community_feeling_4, {
       vars$userChoices <- c(vars$userChoices, input$community_feeling_4) %>% unique
       vars$community_feeling_4 <- input$community_feeling_4})
   observeEvent(input$community_feeling_5, {
       vars$userChoices <- c(vars$userChoices, input$community_feeling_5) %>% unique
       vars$community_feeling_5 <- input$community_feeling_5})
   observeEvent(input$discipleship_feeling_1, {
       vars$userChoices <- c(vars$userChoices, input$discipleship_feeling_1) %>% unique
       vars$discipleship_feeling_1 <- input$discipleship_feeling_1})
   observeEvent(input$discipleship_feeling_2, {
       vars$userChoices <- c(vars$userChoices, input$discipleship_feeling_2) %>% unique
       vars$discipleship_feeling_2 <- input$discipleship_feeling_2})
   observeEvent(input$discipleship_feeling_3, {
       vars$userChoices <- c(vars$userChoices, input$discipleship_feeling_3) %>% unique
       vars$discipleship_feeling_3 <- input$discipleship_feeling_3})
   observeEvent(input$discipleship_feeling_4, {
       vars$userChoices <- c(vars$userChoices, input$discipleship_feeling_4) %>% unique
       vars$discipleship_feeling_4 <- input$discipleship_feeling_4})
   observeEvent(input$discipleship_feeling_5, {
       vars$userChoices <- c(vars$userChoices, input$discipleship_feeling_5) %>% unique
       vars$discipleship_feeling_5 <- input$discipleship_feeling_5})
   observeEvent(input$evangelism_feeling_1, {
       vars$userChoices <- c(vars$userChoices, input$evangelism_feeling_1) %>% unique
       vars$evangelism_feeling_1 <- input$evangelism_feeling_1})
   observeEvent(input$evangelism_feeling_2, {
       vars$userChoices <- c(vars$userChoices, input$evangelism_feeling_2) %>% unique
       vars$evangelism_feeling_2 <- input$evangelism_feeling_2})
   observeEvent(input$evangelism_feeling_3, {
       vars$userChoices <- c(vars$userChoices, input$evangelism_feeling_3) %>% unique
       vars$evangelism_feeling_3 <- input$evangelism_feeling_3})
   observeEvent(input$evangelism_feeling_4, {
       vars$userChoices <- c(vars$userChoices, input$evangelism_feeling_4) %>% unique
       vars$evangelism_feeling_4 <- input$evangelism_feeling_4})
   observeEvent(input$evangelism_feeling_5, {
       vars$userChoices <- c(vars$userChoices, input$evangelism_feeling_5) %>% unique
       vars$evangelism_feeling_5 <- input$evangelism_feeling_5})
   observeEvent(input$prayer_feeling_1, {
       vars$userChoices <- c(vars$userChoices, input$prayer_feeling_1) %>% unique
       vars$prayer_feeling_1 <- input$prayer_feeling_1})
   observeEvent(input$prayer_feeling_2, {
       vars$userChoices <- c(vars$userChoices, input$prayer_feeling_2) %>% unique
       vars$prayer_feeling_2 <- input$prayer_feeling_2})
   observeEvent(input$prayer_feeling_3, {
       vars$userChoices <- c(vars$userChoices, input$prayer_feeling_3) %>% unique
       vars$prayer_feeling_3 <- input$prayer_feeling_3})
   observeEvent(input$prayer_feeling_4, {
       vars$userChoices <- c(vars$userChoices, input$prayer_feeling_4) %>% unique
       vars$prayer_feeling_4 <- input$prayer_feeling_4})
   observeEvent(input$prayer_feeling_5, {
       vars$userChoices <- c(vars$userChoices, input$prayer_feeling_5) %>% unique
       vars$prayer_feeling_5 <- input$prayer_feeling_5})
   observeEvent(input$sacraments_feeling_1, {
       vars$userChoices <- c(vars$userChoices, input$sacraments_feeling_1) %>% unique
       vars$sacraments_feeling_1 <- input$sacraments_feeling_1})
   observeEvent(input$sacraments_feeling_2, {
       vars$userChoices <- c(vars$userChoices, input$sacraments_feeling_2) %>% unique
       vars$sacraments_feeling_2 <- input$sacraments_feeling_2})
   observeEvent(input$sacraments_feeling_3, {
       vars$userChoices <- c(vars$userChoices, input$sacraments_feeling_3) %>% unique
       vars$sacraments_feeling_3 <- input$sacraments_feeling_3})
   observeEvent(input$sacraments_feeling_4, {
       vars$userChoices <- c(vars$userChoices, input$sacraments_feeling_4) %>% unique
       vars$sacraments_feeling_4 <- input$sacraments_feeling_4})
   observeEvent(input$sacraments_feeling_5, {
       vars$userChoices <- c(vars$userChoices, input$sacraments_feeling_5) %>% unique
       vars$sacraments_feeling_5 <- input$sacraments_feeling_5})
   observeEvent(input$social_action_feeling_1, {
       vars$userChoices <- c(vars$userChoices, input$social_action_feeling_1) %>% unique
       vars$social_action_feeling_1 <- input$social_action_feeling_1})
   observeEvent(input$social_action_feeling_2, {
       vars$userChoices <- c(vars$userChoices, input$social_action_feeling_2) %>% unique
       vars$social_action_feeling_2 <- input$social_action_feeling_2})
   observeEvent(input$social_action_feeling_3, {
       vars$userChoices <- c(vars$userChoices, input$social_action_feeling_3) %>% unique
       vars$social_action_feeling_3 <- input$social_action_feeling_3})
   observeEvent(input$social_action_feeling_4, {
       vars$userChoices <- c(vars$userChoices, input$social_action_feeling_4) %>% unique
       vars$social_action_feeling_4 <- input$social_action_feeling_4})
   observeEvent(input$social_action_feeling_5, {
       vars$userChoices <- c(vars$userChoices, input$social_action_feeling_5) %>% unique
       vars$social_action_feeling_5 <- input$social_action_feeling_5})
   
   
   ### Pass a vector of 5 things to generateFeelingsInputs
   
  ### Activities Tibble ###### 
   vars$activities <- tibble(
       type = rep(
           c("community",
             "discipleship",
             "communal_worship",
             "sacraments",
             "evangelism",
             "social_action",
             "prayer"),
           n_rows
           ),
       number = rep(1:n_rows, 7),
       activity = character(7 * n_rows),
       status = factor(rep(NA, 7 * n_rows),
                       levels = statusChoices),
       feeling = character(7 * n_rows)) %>% 
       arrange(type, number)
   
   output$community <- renderUI({makeFeelingInputs("Community", choices = choice_added(), 
                                               values = c(
                                                   vars$community_feeling_1,
                                                   vars$community_feeling_2,
                                                   vars$community_feeling_3,
                                                   vars$community_feeling_4,
                                                   vars$community_feeling_5
                                               ), rows = n_rows)})
   output$discipleship <- renderUI({makeFeelingInputs("Discipleship", choices = choice_added(),
                                                  values = c(
                                                      vars$discipleship_feeling_1,
                                                      vars$discipleship_feeling_2,
                                                      vars$discipleship_feeling_3,
                                                      vars$discipleship_feeling_4,
                                                      vars$discipleship_feeling_5
                                                  ), rows = n_rows)})
   output$communal_worship <- renderUI({makeFeelingInputs("Communal Worship", choices = choice_added(),
                                                      values = c(
                                                          vars$communal_worship_feeling_1,
                                                          vars$communal_worship_feeling_2,
                                                          vars$communal_worship_feeling_3,
                                                          vars$communal_worship_feeling_4,
                                                          vars$communal_worship_feeling_5
                                                      ), rows = n_rows)})
   output$sacraments <- renderUI({makeFeelingInputs("Sacraments", choices = choice_added(),
                                                values = c(
                                                    vars$sacraments_feeling_1,
                                                    vars$sacraments_feeling_2,
                                                    vars$sacraments_feeling_3,
                                                    vars$sacraments_feeling_4,
                                                    vars$sacraments_feeling_5
                                                ), rows = n_rows)})
   output$evangelism <- renderUI({makeFeelingInputs("Evangelism", choices = choice_added(),
                                                values = c(
                                                    vars$evangelism_feeling_1,
                                                    vars$evangelism_feeling_2,
                                                    vars$evangelism_feeling_3,
                                                    vars$evangelism_feeling_4,
                                                    vars$evangelism_feeling_5
                                                ), rows = n_rows)})
   output$social_action <- renderUI({makeFeelingInputs("Social Action", choices = choice_added(),
                                                   values =  c(
                                                       vars$social_action_feeling_1,
                                                       vars$social_action_feeling_2,
                                                       vars$social_action_feeling_3,
                                                       vars$social_action_feeling_4,
                                                       vars$social_action_feeling_5
                                                   ), rows = n_rows)})
   output$prayer <- renderUI({makeFeelingInputs("Prayer", choices = choice_added(),
                                            values = c(
                                                vars$prayer_feeling_1,
                                                vars$prayer_feeling_2,
                                                vars$prayer_feeling_3,
                                                vars$prayer_feeling_4,
                                                vars$prayer_feeling_5
                                            ), rows = n_rows)})
   
   outputOptions(output, "community", suspendWhenHidden = FALSE)
   outputOptions(output, "discipleship", suspendWhenHidden = FALSE)
   outputOptions(output, "communal_worship", suspendWhenHidden = FALSE)
   outputOptions(output, "sacraments", suspendWhenHidden = FALSE)
   outputOptions(output, "evangelism", suspendWhenHidden = FALSE)
   outputOptions(output, "social_action", suspendWhenHidden = FALSE)
   outputOptions(output, "prayer", suspendWhenHidden = FALSE)
   
   output$debug <- renderText({toString(c(vars$community_feeling_1,
   vars$community_feeling_2,
   vars$community_feeling_3,
   vars$community_feeling_4,
   vars$community_feeling_5))
      })
   

    
}

# Run the application 
shinyApp(ui = ui, server = server)
