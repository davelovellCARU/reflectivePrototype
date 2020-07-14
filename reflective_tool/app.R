library("shiny")
library("shinythemes")
library("stringr")
library("dplyr")
library("purrr")
library("readr")
library("here")
library("magrittr")
library("dplyr")
library("tidyr")
library("forcats")
library("ggplot2")
library("gganimate")
library("carutools")
library("Cairo")
library("shinybusy")
library("gifski")


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
             selectizeInput(inputId = paste0(str_replace_all(tab, "[:space:]", "_"),
                                        "_name_",
                                        as.character(number)),
                       label = {
                          if(number == 1) "Activity"
                          else NULL
                       },
                       choices = NULL,
                       options = list(create = TRUE))
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
               # column(3, uiOutput(tabName %>% str_to_lower %>% str_replace_all("[:space:]", "_")) ),
               # column(3, makeFeelingInputs(tab = str_to_lower(tabName), rows = n_rows, choices = choices, values = values)),
               whitespace()
            # )
   ) -> myTabPanel
   
   return(myTabPanel)
}




ui <- fluidPage(
   theme = shinytheme("lumen"),
    
    titlePanel("Hello"),
   # h1("Hello"),
   p("Church Army think that you're probably one of the leading experts on your own missional context. That's why we've built this reflective tool."),
   p("As a result of the lockdown, some aspects of your ministry might have changed drastically. Some things may have stayed the same, and other things might have stopped altogether. It may even be that some entirely new things have begun.
     In the tables below, we use these basic categories (started, stopped, changed, stayed the same) to help you to build up a picture of what's happened in your context.
     Additionally, we've divided the entry fields into seven broadly defined aspects of ministry: Community, Discipleship, Communal Worship, Sacraments, Evangelism, Social Action and Prayer. If you're not sure which one of these aspects describes a particular activity, just put it where you think it fits best."),
   p("Filling in the tables below might only take five minutes, but we recommend going slowly; Revisiting these areas of change may be an opportune time to reflect, pray or even grieve."),
   p("When you've finished updating the activity tables, you can press the 'Show me my Graph' button to see a graph of your responses. There's a lot that numbers can't capture, so your graph will only tell part of the story. Nevertheless, we hope that a visual overview of the 'shape' of change might grant you another perspective on your experience."),
    verticalLayout(
    
    tabsetPanel(
       tabPanel("Community Activities", makeInputsTab("Community Activities")),
       tabPanel("Discipleship", makeInputsTab("Discipleship")),
       tabPanel("Communal Worship", makeInputsTab("Communal Worship")),
       tabPanel("Sacraments", makeInputsTab("Sacraments")),
       tabPanel("Evangelism", makeInputsTab("Evangelism")),
       tabPanel("Social Action", makeInputsTab("Social Action")),
       tabPanel("Prayer", makeInputsTab("Prayer"))
            ),
    
    # Download Button ------------------------- 
    # downloadButton("downloadData", "Download"),
    # textOutput("dev_commentary_2"),
    
    actionButton("graphButton", "Show me my Graph"),
    add_busy_spinner(spin = "fading-circle"),
    imageOutput("vis")
    
    )
    )

server <- function(input, output) {
   
   output$dev_commentary_2 <- renderText("You can download a csv of your own responses. Not very useful, but it proves the concept.")
   output$dev_commentary <- renderText("This takes about 2 minutes. But the beauty is that we can gift that time to the user for personal reflection.")
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
   
#### status_split used to reformat data when making visualisation ####
   status_split <- function(statusValue) {
      
      statusValue %<>% as.character
      statusValue %<>% str_to_lower()
      
      if (statusValue == "ended") {
         beforeStatus <- "existent"
         afterStatus <- "non-existent"
      } else if (statusValue == "started") {
         beforeStatus <- "non-existent"
         afterStatus <- "new"
      } else if (statusValue == "stayed the same") {
         beforeStatus <- "existent"
         afterStatus <- "existent"
      } else if (statusValue == "changed") {
         beforeStatus <- "existent"
         afterStatus <- "different"
      } else {
         # stop("Activity status must be one of: 'started', ended', 'changed' or 'stayed the same'")
         NA
      }
      
      statusTibble <- tibble(beforeStatus = beforeStatus,
                             afterStatus = afterStatus) %>%
         mutate(across(everything(),
                       ~ factor(.,levels = rev(c("existent", "different", "new", "non-existent")),
                                ordered = TRUE)
         ))
      
      return(statusTibble)
      
   }
      

### Selectize Reactives
   vars <- reactiveValues()
   
   ## make userChoices - a reactive variable that updates when selectize inputs are used
   vars$userChoices <- character()
   choice_added <- reactive(vars$userChoices)
   
   #### Event observers for selectize inputs #######
   observeEvent(input$communal_worship_feeling_1, {
       vars$userChoices <- c(vars$userChoices, input$communal_worship_feeling_1) %>% unique})
   observeEvent(input$communal_worship_feeling_2, {
       vars$userChoices <- c(vars$userChoices, input$communal_worship_feeling_2) %>% unique})
   observeEvent(input$communal_worship_feeling_3, {
       vars$userChoices <- c(vars$userChoices, input$communal_worship_feeling_3) %>% unique})
   observeEvent(input$communal_worship_feeling_4, {
       vars$userChoices <- c(vars$userChoices, input$communal_worship_feeling_4) %>% unique})
   observeEvent(input$communal_worship_feeling_5, {
       vars$userChoices <- c(vars$userChoices, input$communal_worship_feeling_5) %>% unique})
   observeEvent(input$community_activities_feeling_1, {
       vars$userChoices <- c(vars$userChoices, input$community_activities_feeling_1) %>% unique})
   observeEvent(input$community_activities_feeling_2, {
       vars$userChoices <- c(vars$userChoices, input$community_activities_feeling_2) %>% unique})
   observeEvent(input$community_activities_feeling_3, {
       vars$userChoices <- c(vars$userChoices, input$community_activities_feeling_3) %>% unique})
   observeEvent(input$community_activities_feeling_4, {
       vars$userChoices <- c(vars$userChoices, input$community_activities_feeling_4) %>% unique})
   observeEvent(input$community_activities_feeling_5, {
       vars$userChoices <- c(vars$userChoices, input$community_activities_feeling_5) %>% unique})
   observeEvent(input$discipleship_feeling_1, {
       vars$userChoices <- c(vars$userChoices, input$discipleship_feeling_1) %>% unique})
   observeEvent(input$discipleship_feeling_2, {
       vars$userChoices <- c(vars$userChoices, input$discipleship_feeling_2) %>% unique})
   observeEvent(input$discipleship_feeling_3, {
       vars$userChoices <- c(vars$userChoices, input$discipleship_feeling_3) %>% unique})
   observeEvent(input$discipleship_feeling_4, {
       vars$userChoices <- c(vars$userChoices, input$discipleship_feeling_4) %>% unique})
   observeEvent(input$discipleship_feeling_5, {
       vars$userChoices <- c(vars$userChoices, input$discipleship_feeling_5) %>% unique})
   observeEvent(input$evangelism_feeling_1, {
       vars$userChoices <- c(vars$userChoices, input$evangelism_feeling_1) %>% unique})
   observeEvent(input$evangelism_feeling_2, {
       vars$userChoices <- c(vars$userChoices, input$evangelism_feeling_2) %>% unique})
   observeEvent(input$evangelism_feeling_3, {
       vars$userChoices <- c(vars$userChoices, input$evangelism_feeling_3) %>% unique})
   observeEvent(input$evangelism_feeling_4, {
       vars$userChoices <- c(vars$userChoices, input$evangelism_feeling_4) %>% unique})
   observeEvent(input$evangelism_feeling_5, {
       vars$userChoices <- c(vars$userChoices, input$evangelism_feeling_5) %>% unique})
   observeEvent(input$prayer_feeling_1, {
       vars$userChoices <- c(vars$userChoices, input$prayer_feeling_1) %>% unique})
   observeEvent(input$prayer_feeling_2, {
       vars$userChoices <- c(vars$userChoices, input$prayer_feeling_2) %>% unique})
   observeEvent(input$prayer_feeling_3, {
       vars$userChoices <- c(vars$userChoices, input$prayer_feeling_3) %>% unique})
   observeEvent(input$prayer_feeling_4, {
       vars$userChoices <- c(vars$userChoices, input$prayer_feeling_4) %>% unique})
   observeEvent(input$prayer_feeling_5, {
       vars$userChoices <- c(vars$userChoices, input$prayer_feeling_5) %>% unique})
   observeEvent(input$sacraments_feeling_1, {
       vars$userChoices <- c(vars$userChoices, input$sacraments_feeling_1) %>% unique})
   observeEvent(input$sacraments_feeling_2, {
       vars$userChoices <- c(vars$userChoices, input$sacraments_feeling_2) %>% unique})
   observeEvent(input$sacraments_feeling_3, {
       vars$userChoices <- c(vars$userChoices, input$sacraments_feeling_3) %>% unique})
   observeEvent(input$sacraments_feeling_4, {
       vars$userChoices <- c(vars$userChoices, input$sacraments_feeling_4) %>% unique})
   observeEvent(input$sacraments_feeling_5, {
       vars$userChoices <- c(vars$userChoices, input$sacraments_feeling_5) %>% unique})
   observeEvent(input$social_action_feeling_1, {
       vars$userChoices <- c(vars$userChoices, input$social_action_feeling_1) %>% unique})
   observeEvent(input$social_action_feeling_2, {
       vars$userChoices <- c(vars$userChoices, input$social_action_feeling_2) %>% unique})
   observeEvent(input$social_action_feeling_3, {
       vars$userChoices <- c(vars$userChoices, input$social_action_feeling_3) %>% unique})
   observeEvent(input$social_action_feeling_4, {
       vars$userChoices <- c(vars$userChoices, input$social_action_feeling_4) %>% unique})
   observeEvent(input$social_action_feeling_5, {
       vars$userChoices <- c(vars$userChoices, input$social_action_feeling_5) %>% unique})
   
   
   ### Pass a vector of 5 things to generateFeelingsInputs
   
  ### Activities Tibble ###### 
   activities <- reactive({tibble(
       type = rep(
           c("community_activities",
             "discipleship",
             "communal_worship",
             "sacraments",
             "evangelism",
             "social_action",
             "prayer"),
           n_rows
           ),
       number = rep(1:n_rows, 7)) %>% 
         mutate(activity = 
                   map2_chr(type, number, ~ parse(text = paste0("input$", .x, "_name_", .y)) %>% 
                           {.[[1]]} %>% 
                           eval),
                status = 
                   map2_chr(type, number, ~ parse(text = paste0("input$", .x, "_status_", .y)) %>% 
                               {.[[1]]} %>% 
                               eval),
                feeling = 
                   map2_chr(type, number, ~ parse(text = paste0("input$", .x, "_feeling_", .y)) %>% 
                               {.[[1]]} %>% 
                               eval %>% 
                               {if(is.null(.)) NA
                                  else(.)})) %>%  
       arrange(type, number)})
   
   output$community_activities <- renderUI({makeFeelingInputs("Community Activities", choices = choice_added(), 
                                               values = c(
                                                   input$community_activities_feeling_1,
                                                   input$community_activities_feeling_2,
                                                   input$community_activities_feeling_3,
                                                   input$community_activities_feeling_4,
                                                   input$community_activities_feeling_5
                                               ), rows = n_rows)})
   output$discipleship <- renderUI({makeFeelingInputs("Discipleship", choices = choice_added(),
                                                  values = c(
                                                      input$discipleship_feeling_1,
                                                      input$discipleship_feeling_2,
                                                      input$discipleship_feeling_3,
                                                      input$discipleship_feeling_4,
                                                      input$discipleship_feeling_5
                                                  ), rows = n_rows)})
   output$communal_worship <- renderUI({makeFeelingInputs("Communal Worship", choices = choice_added(),
                                                      values = c(
                                                          input$communal_worship_feeling_1,
                                                          input$communal_worship_feeling_2,
                                                          input$communal_worship_feeling_3,
                                                          input$communal_worship_feeling_4,
                                                          input$communal_worship_feeling_5
                                                      ), rows = n_rows)})
   output$sacraments <- renderUI({makeFeelingInputs("Sacraments", choices = choice_added(),
                                                values = c(
                                                    input$sacraments_feeling_1,
                                                    input$sacraments_feeling_2,
                                                    input$sacraments_feeling_3,
                                                    input$sacraments_feeling_4,
                                                    input$sacraments_feeling_5
                                                ), rows = n_rows)})
   output$evangelism <- renderUI({makeFeelingInputs("Evangelism", choices = choice_added(),
                                                values = c(
                                                    input$evangelism_feeling_1,
                                                    input$evangelism_feeling_2,
                                                    input$evangelism_feeling_3,
                                                    input$evangelism_feeling_4,
                                                    input$evangelism_feeling_5
                                                ), rows = n_rows)})
   output$social_action <- renderUI({makeFeelingInputs("Social Action", choices = choice_added(),
                                                   values =  c(
                                                       input$social_action_feeling_1,
                                                       input$social_action_feeling_2,
                                                       input$social_action_feeling_3,
                                                       input$social_action_feeling_4,
                                                       input$social_action_feeling_5
                                                   ), rows = n_rows)})
   output$prayer <- renderUI({makeFeelingInputs("Prayer", choices = choice_added(),
                                            values = c(
                                                input$prayer_feeling_1,
                                                input$prayer_feeling_2,
                                                input$prayer_feeling_3,
                                                input$prayer_feeling_4,
                                                input$prayer_feeling_5
                                            ), rows = n_rows)})
   
   outputOptions(output, "community_activities", suspendWhenHidden = FALSE)
   outputOptions(output, "discipleship", suspendWhenHidden = FALSE)
   outputOptions(output, "communal_worship", suspendWhenHidden = FALSE)
   outputOptions(output, "sacraments", suspendWhenHidden = FALSE)
   outputOptions(output, "evangelism", suspendWhenHidden = FALSE)
   outputOptions(output, "social_action", suspendWhenHidden = FALSE)
   outputOptions(output, "prayer", suspendWhenHidden = FALSE)
   
   output$downloadData <- downloadHandler(
      filename = "activities.csv",
      content = function(file) {write.csv(activities(), file, row.names = FALSE)}
   )
    
   ### Make the Graphic ####
   
   observeEvent(input$graphButton,
                {
                   visData <- isolate(activities())
                   
                   visData %<>% group_by(type, number, activity)
                   visData %<>% summarise(status_split(status))
                   visData %<>% ungroup
                   
                   visData %<>% pivot_longer(all_of(c("beforeStatus", "afterStatus")),
                                             names_to = "time",
                                             names_pattern = "(.*)(?=Status)",
                                             values_to = "status",
                                             names_transform = 
                                                list(time = 
                                                        ~ factor(., levels = c("before", "after"), 
                                                                 ordered = TRUE)))
                   
                   visData %<>% group_by(type, time)
                   visData %<>% summarise(existent = sum(status == "existent" & !is.na(activity) & activity != ""),
                                          different = sum(status == "different" & !is.na(activity) & activity != ""),
                                          new = sum(status == "new" & !is.na(activity) & activity != ""), 
                                          non_existent = sum(status == "non-existent" & !is.na(activity) & activity != "")) %>% 
                      pivot_longer(all_of(c("existent", "different", "non_existent", "new")),
                                   names_to = "status",
                                   values_to = "occurence")
                   
                   donutHole <- 0.5
                   
                   make_radial <- function(visData) {
                      
                      visData %<>% group_by(type, time)
                      visData %<>% arrange(status)
                      visData %>%
                         mutate(
                            occurence = 
                               ### clever snippet gets heights for big circle
                               occurence %>% 
                               {tmp <- sqrt(cumsum(c(donutHole, .)))
                               tmp <- tmp - c(0, tmp[-length(tmp)])
                               tmp[-1]})
                   }
                   
                   visData %<>% make_radial
                   
                   visData %<>% mutate(type = 
                                          type %>% 
                                          str_replace_all("_", " ") %>% 
                                          str_to_title())
                   visData %<>% mutate(status = 
                                          status %>% 
                                          fct_recode("Stayed the same" = "existent", 
                                                     "Changed" = "different",
                                                     "Ended" = "non_existent",
                                                     "New" = "new") %>% 
                                          fct_relevel(rev(
                                             c("Stayed the same",
                                               "Changed",
                                               "New",
                                               "Ended")
                                             )))
                   visData %<>% rename("Activity status:" = "status")
                   visData %<>% filter(time == "after")
                   
                   ### Remove zero-count 'ended's (remove empty 'outline' box in plot)
                   visData %<>% mutate(occurence = 
                                          replace(occurence, occurence == 0, NA))
                   
                   q <- ggplot(visData, aes(
                      x = type,
                      y = occurence
                   )) +
                      geom_bar(stat = "identity",
                               position = "stack",
                               width = 1,
                               col = "black",
                               aes(group = `Activity status:`,
                                   fill = `Activity status:`,
                                   # col = `Activity status:`,
                                   linetype = `Activity status:`,
                                   size = `Activity status:`)) +
                      coord_polar(theta = "x") +
                      theme_minimal() +
                      scale_y_continuous(breaks = NULL, limits = c(- donutHole, NA)) +
                      scale_fill_manual(values = c("Stayed the same" = ct_darkteal(),
                                                   "Changed" = ct_cyan(),
                                                   "New" = ct_purple(),
                                                   "Ended" = NA),
                                        na.translate = FALSE) +
                      # scale_color_manual(values = c("Stayed the same" = "black",
                      #                               "Changed" = "black",
                      #                               "New" = "black",
                      #                               "Ended" = alpha("grey", .7))) +
                      scale_linetype_manual(values = c("Stayed the same" = "solid",
                                                       "Changed" = "solid",
                                                       "New" = "solid",
                                                       "Ended" = "dotted")) +
                      scale_size_manual(values = c("Stayed the same" = .6,
                                                       "Changed" = .6,
                                                       "New" = .6,
                                                       "Ended" = .4)) +
                      xlab(NULL) +
                      ylab(NULL) +
                      theme(axis.text.x = element_text(size = 15,
                                                       angle = 
                                                          360 / (2 * pi) * seq(2 * pi - pi / 7, pi / 7, len = 7),
                                                       hjust = 1),
                            plot.title = element_text(size = 17),
                            panel.grid = element_blank()) +
                      annotate("text", x = 1,  y = -.5, label = "Church\nArmy",
                               hjust=1.1, vjust=-1.1, col="black", cex=4,
                               fontface = "bold", alpha = 0.8)
                   
                   output$vis <- renderPlot({q})
                })
}

# Run the application 
shinyApp(ui = ui, server = server)
