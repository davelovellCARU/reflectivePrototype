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
library("shinybusy")
library("gifski")
library("httr")
library("rtweet")
library("markdown")
library("digest")
library("rdrop2")
library("glue")


### VARIABLES AND FUNCTIONS ####
## Variables -------------------------------------------------------------
statusChoices <- c("started", "suspended/ended", "changed", "stayed the same")
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
                       choices = 
                          if(number == 1){
                           case_when(
                           str_detect(tab, "communal worship") ~ "Congregational  singing",
                           str_detect(tab, "community activities") ~ "Board games night",
                           str_detect(tab, "discipleship") ~ "Student 1-to-1's",
                           str_detect(tab, "evangelism") ~ "Board games night",
                           str_detect(tab, "prayer") ~ "Wednesday AM prayer",
                           str_detect(tab, "life events") ~ "Confirmations",
                           str_detect(tab, "social action") ~ "Food bank volunteering",
                           TRUE ~ ""
                              )
                             },
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
   
            fixedRow(
               column(4, makeNameInputs(tab = str_to_lower(tabName), rows = n_rows)),
               column(5, makeStatusInputs(tab = str_to_lower(tabName), rows = n_rows))
   ) -> myTabPanel
   
   return(myTabPanel)
}

#### Twitter Oath stuff -----

keys <- list(
   consumerKey = read.table("credentials/twitter_consumer_key.txt")[[1,1]],
   consumerSecret = read.table(file = "credentials/twitter_consumer_secret.txt")[[1,1]]
)
app <- 
   oauth_app(
      app = "Church Army's Reflective Tool",
      key = keys$consumerKey,
      secret = keys$consumerSecret
   )

oauth_sig <- function(url, method, token = NULL, token_secret = NULL, private_key = NULL, ...){
   httr::oauth_header(
      httr::oauth_signature(url, 
                            method,
                            app,
                            token,
                            token_secret, 
                            private_key,
                            other_params = list(...)))
} #### Stole this from JN: https://medium.com/@skyetetra/how-to-make-rtweet-r-shiny-apps-with-user-credentials-48acca246b58 ----

get_authorization_url <- 
   function(app, callback_url, permission=NULL){
      private_key <- NULL
      
      response <- 
         httr::POST(
            "https://api.twitter.com/oauth/request_token",
            oauth_sig(
               "https://api.twitter.com/oauth/request_token",
               "POST", 
               private_key = NULL,
               oauth_callback = callback_url)
         )
      
      httr::stop_for_status(response)
      
      params <- 
         httr::content(
            response, 
            type = "application/x-www-form-urlencoded")
      authorize_url <- 
         httr::modify_url(
            "https://api.twitter.com/oauth/authenticate",
            query = list(oauth_token = params$oauth_token, 
                         permission = permission))
      authorize_url 
   } # also stole this lol

url <- get_authorization_url(app, "https://churcharmy.shinyapps.io/reflective-resource/")

ui <- fluidPage(
   theme = shinytheme("lumen"),
   add_busy_spinner(spin = "fading-circle"),
   column(width = 12,
   tags$image(src = 'resources/carulogo.jpg', height = "20%", width = "20%", align = "left")),
 column(
    titlePanel(h1("Reflecting on Change", style = "font-family: 'Impact';"), windowTitle = "Church Army's Reflective Tool"),
    width = 6, offset = 3,
    p("At Church Army, we believe that local church practitioners are the leading experts on their own missional setting. That's why we've created this tool: to help you to reflect on how your church has responded to its new context.", style = "font-family: 'Trebuchet MS';"),
    p("Over the last few months, many of the activities, events, and gatherings that our churches participate in have changed beyond recognition. In the midst of that change, it may have been difficult to find time to reflect on what has taken place.", style = "font-family: 'Trebuchet MS';"),
    p("Our hope is that this tool might be another step on your reflective journey. By looking at your ministry in terms of rythms that have changed, stayed the same, or ended, we hope that you will be able to build a picture of the areas of life that your church has focused on during the Coronavirus crisis. It may be that new things have begun as well, and this will help you to see which areas of church life have flourished during this unique time.", style = "font-family: 'Trebuchet MS';"),
    p("We have divided church life into seven broad areas. In no particular order these are:", style = "font-family: 'Trebuchet MS';"),
    tags$div(
       tags$ul(
          tags$li("Community Activities"),
          tags$li("Discipleship"), 
          tags$li("Communal Worship"),
          tags$li("Life Events"), 
          tags$li("Evangelism"),
          tags$li("Social Action"),
          tags$li("Prayer"),
          style = "font-family: 'Trebuchet MS';"
       )
    ),
    p("If an activity fits under two categories, you may wish to count it twice. This tool is for your benefit, so do whatever makes the most sense to you.", style = "font-family: 'Trebuchet MS';"),
    h3("How to use this tool:", style = "font-family: 'Trebuchet MS';"),
    p("The interface below consists of seven tabs, each of which represents an area of ministry. These tabs each contain one example activity, which you may delete.", style = "font-family: 'Trebuchet MS';"),
    p("To add an activity, simply type its name into the one of the 'Activity' fields and press 'Enter' on your keyboard. You will also need to press enter when removing activities. Once you've entered an activity, choose an option from the adjacent list to describe how that activity has changed. At any time you can press 'Create my visualisation' to visualise your results.", style = "font-family: 'Trebuchet MS';"),
    h3("What can I use this for?", style = "font-family: 'Trebuchet MS';"),
    p("The visualisation of your data could be useful in a variety of ways. It may:", style = "font-family: 'Trebuchet MS';"),
    tags$div(
       tags$ul(
          tags$li("Help you to think open endedly about how your church is responding"),
          tags$li("Give you something to reflect on with your Parochial Church Council (PCC)"), 
          tags$li("Form part of a Mission Action Plan (MAP) process"),
          style = "font-family: 'Trebuchet MS';"
       )
    ),
    h3("Twitter Sharing", style = "font-family: 'Trebuchet MS';"),
      p(
         t("You can take part in the wider conversation about change by sharing your results on Twitter. You'll need to"),
         strong("sign in with Twitter before you fill in the table below"),
         t("because the sign-in process will refresh the page. You'll be in full control of what gets tweeted."), style = "font-family: 'Trebuchet MS';"),
      p(),
      tags$a(
         href = url,
         tags$img(src =  "resources/twitter_auth.png",
                  height = "20px")
      ),
      p(),
   ),
    verticalLayout(
    
       column(width = 8, offset = 2,
              h3("Church/community name:"),
              textInput("communityName", "What is the name of your church/community?"),
              h3("Summary of activities by type:"),
    tabsetPanel(
       tabPanel("Community Activities", makeInputsTab("Community Activities")),
       tabPanel("Discipleship", makeInputsTab("Discipleship")),
       tabPanel("Communal Worship", makeInputsTab("Communal Worship")),
       tabPanel("Life Events", makeInputsTab("Life Events")),
       tabPanel("Evangelism", makeInputsTab("Evangelism")),
       tabPanel("Social Action", makeInputsTab("Social Action")),
       tabPanel("Prayer", makeInputsTab("Prayer"))
            )
    ),
   
   fluidRow( 
      column(width = 8, offset = 2,
    actionButton("graphButton", "Create my visualisation"),
    p(),
    textOutput("tweetDescription")
      )
    ),
   column(10, offset = 1, align = "center",
    imageOutput("vis", width = "100%", height = "600px"),
   downloadButton("savePlot", "Save visualisation"),
   downloadButton("downloadData", "Download response data")),
    uiOutput("tweetButton"),
    uiOutput("tweetInput")
    )
    )

server <- function(input, output, session) {
   #Dropbox stuff:

   set.seed(Sys.time())
   drop_auth(rdstoken = "credentials/dropbox_token.rdat")
   instance <- digest::sha1(runif(100))
   
   ### Watch Twitter button ------
   
   if(!dir.exists("www/plots")) dir.create("www/plots")
   
   imageName <- reactiveVal(character(0))
   tweetImageName <- reactiveVal(character(0))
   
   observeEvent(input$showModal,
                {
                   output$tweetBodyOut <- renderText({input$tweetBody})
                   showModal(
                      modalDialog(
                      h3("Send this Tweet?"),
                      textOutput("tweetBodyOut"),
                      imageOutput("tweetVis", height = "250px"),
                      actionButton("tweet", "Send Tweet")
                      )
                      )
                })
   output$tweetButton <- renderUI({
      if (length(getQueryString(session)) > 0) {
         actionButton("showModal", "Preview and Send Tweet")
      } else NULL
   })
   
   output$tweetDescription <- renderText({
      if(length(getQueryString(session)) > 0) {
         "You are currently signed in with Twitter! You can customise your Tweet using the text box below. When you're ready, hit the 'Send Tweet' button below to Tweet a copy of your visulaisation."
      } else "You are not signed in with Twitter, and will not be able to share your visualisation. If you would like to sign in, you must do so before you enter your responses"
   })
   
   output$tweetInput <- renderUI({
      if(length(getQueryString(session)) > 0) {
            textAreaInput("tweetBody",
                          label = "Customise Tweet:",
                          width = "400px",
                          height = "150px",
               value = 
                  (
                  if (is.null(input$communityName)|input$communityName == "")
                  {
               "I've been using @ChurchArmy's new reflective tool to review how ministry has changed during COVID-19.
Take some time to reflect and discover your ministerial visualisation here:
https://churcharmy.shinyapps.io/reflective-resource/"
                  } else {
                     paste0("I've been using Church Army's new reflective tool to review how ministry at ",
                            str_trim(input$communityName),
                            " has changed during COVID-19.
Take some time to reflect and discover your ministerial visualisation here:
https://churcharmy.shinyapps.io/reflective-resource/")
                  }) %>% 
                        str_sub(1,280)
               )
      } else NULL
   })
   
   observeEvent(input$tweet,
                {
   query <- getQueryString(session)
   
   get_access_token <- 
      function(app, 
               oauth_token,
               oauth_verifier){
         
         url <- 
            paste0(
               "https://api.twitter.com/oauth/access_token?oauth_token=",
               oauth_token, "&oauth_verifier=", oauth_verifier)
         
         response <- 
            httr::POST(url, 
                       oauth_sig(url,
                                 "POST",
                                 private_key = NULL))
         
         if(response$status_code == 200L){
            results <- 
               content(
                  response,
                  type = "application/x-www-form-urlencoded",
                  encoding = "UTF-8")
            
            # since storing the username might be creepy
            results[["screen_name"]] <- NULL 
            
            # since storing the user id might be creepy
            results[["user_id"]] <- NULL     
            
            results
         } else {
            NULL
         }
      }
   
   access_token <- 
      get_access_token(app, query$oauth_token, query$oauth_verifier)
   
   user_token <- create_token(app="Church Army's Reflective Tool", 
                              consumer_key = keys$consumerKey, 
                              consumer_secret = keys$consumerSecret, 
                              access_token = access_token$oauth_token, 
                              access_secret = access_token$oauth_token_secret)
   
                   rtweet::post_tweet(token = user_token,
                                      status = input$tweetBody,
                                      media = paste0("www/plots/", imageName(),".jpeg"))
                   
                   removeModal()
                })
   
#### status_split used to reformat data when making visualisation ####
   status_split <- function(statusValue) {
      
      statusValue %<>% str_to_lower()
      statusValue %<>% as.character
      
      if (statusValue == "suspended/ended") {
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
   
  ### Activities Tibble ###### 
   activities <- reactive({tibble(
       type = rep(
           c("community_activities",
             "discipleship",
             "communal_worship",
             "life_events",
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
                               eval)) %>%  
       arrange(type, number)})
   
   output$downloadData <- downloadHandler(
      filename = paste0(isolate(input$communityName) %>%
                                   str_squish %>% str_to_lower %>% str_replace_all("[:space:]", "_"),
                        "_activity_data.csv"),
      content = function(file) {write.csv(activities() %>% 
                                             select(-number) %>% 
                                             filter(!(activity %in% "")), file, row.names = FALSE)}
   )
   
   output$savePlot <- downloadHandler(
      filename = paste0(isolate(input$communityName) %>%
                                   str_squish %>% str_to_lower %>% str_replace_all("[:space:]", "_"),
                        "_visualisation.jpeg"),
      content = function(file) {if(file.exists(paste0("www/plots/", imageName(),".jpeg"))){
         file.copy(paste0("www/plots/", imageName(),".jpeg"), file)
      }
      }
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

                   visData %<>% filter(time == "after")
                   
                   make_radial <- function(visData) {
                      
                      visData %<>% mutate(status = 
                                             status %>% 
                                             fct_recode("Stayed the same" = "existent", 
                                                        "Changed" = "different",
                                                        "(Susp)ended" = "non_existent",
                                                        "New" = "new") %>% 
                                             fct_relevel(rev(
                                                c("Stayed the same",
                                                  "Changed",
                                                  "New",
                                                  "(Susp)ended")
                                             )))
                      
                      visData %<>% group_by(type)
                      
                      # Temporary variable for arranging by factor level
                      visData %<>% mutate(statLevels = as.numeric(status))
                      
                      visData %<>% arrange(desc(statLevels), by_group = TRUE)
                      
                      visData %<>%
                         mutate(
                            occurence = 
                               ### clever snippet gets heights for big circle
                               occurence %>% 
                               {tmp <- sqrt(cumsum(c(donutHole, .)))
                               tmp <- tmp - c(0, tmp[-length(tmp)])
                               tmp[-1]})
                      return(visData)
                   }

                   visData %<>% make_radial
                   
                   
                   
                   visData %<>% rename("Activity status:" = "status")
                   
                   visData %<>% mutate(type = 
                                          type %>% 
                                          str_replace_all("_", " ") %>% 
                                          str_to_title())
                   
                   ### Remove zero-count 'ended's (remove empty 'outline' box in plot)
                   visData %<>% mutate(occurence = 
                                          replace(occurence, occurence == 0, NA))
                   
                   q <- ggplot(visData %>% 
                                  rename("Activity type" = "Activity status:"), aes(
                      x = type,
                      y = occurence
                   )) +
                      geom_bar(stat = "identity",
                               position = "stack",
                               width = 1,
                               col = "black",
                               aes(group = `Activity type`,
                                   fill = `Activity type`,
                                   linetype = `Activity type`,
                                   size = `Activity type`)) +
                      coord_polar(theta = "x", clip = "off") +
                      theme_minimal() +
                      scale_y_continuous(breaks = NULL, limits = c(- donutHole, NA)) +
                      scale_x_discrete(labels = function(kek) str_replace_all(kek,"[:space:]", "\n") %>% 
                                          paste0("\n")) +
                      scale_fill_manual(values = c("Stayed the same" = ct_darkteal(),
                                                   "Changed" = ct_cyan(),
                                                   "New" = ct_purple(),
                                                   "(Susp)ended" = NA),
                                        na.translate = FALSE) +
                      scale_linetype_manual(values = c("Stayed the same" = "solid",
                                                       "Changed" = "solid",
                                                       "New" = "solid",
                                                       "(Susp)ended" = "dotted")) +
                      scale_size_manual(values = c("Stayed the same" = .9,
                                                       "Changed" = .9,
                                                       "New" = .9,
                                                       "(Susp)ended" = .6)) +
                      xlab(NULL) +
                      ylab(NULL) +
                      theme(axis.text.x = element_text(size = 32,
                                                       angle = 
                                                          360 / (2 * pi) * seq(2 * pi - pi / 7, pi / 7, len = 7)),
                            plot.title = element_text(size = 37,
                                                      margin = unit(c(0,0,10,0), "mm")),
                            panel.grid = element_blank(),
                            plot.caption = element_text(size = 25, colour = "grey45"),
                            plot.margin = unit(c(0, 105, 0, 15), "mm"),
                            legend.text = element_text(size = 28),
                            legend.title = element_text(size = 35),
                            legend.position = c(1.3,.75)) +
                      labs(caption = "Church Army") +
                      ggtitle(
                         case_when(
                            input$communityName != "" ~ paste0("The shape of change at\n",
                                str_trim(input$communityName)),
                            TRUE ~ "The shape of Change"
                         )
                      )
                   
                   #output$vis <- renderPlot({q})
                   
                   imageName(sha1(toString(isolate(activities()))))
                   
                   jpeg(paste0("www/plots/", imageName(),".jpeg"),
                        res = 450, 
                        height = 2400 * (450/250),
                        width = 3000 * (450/250), 
                        units = "px")
                   print(q)
                   dev.off()

                   
                   output$vis <- renderImage({
                      outfile <- paste0("www/plots/", imageName(),".jpeg")
                      
                      list(src = outfile,
                           alt = "Visualisation of user input",
                           width = 750)
                   }, deleteFile = FALSE)
                   
                   output$tweetVis <- renderImage({
                      outfile <- paste0("www/plots/", imageName(),".jpeg")
                      
                      list(src = outfile,
                           alt = "Visualisation of user input",
                           width = 250)
                   }, deleteFile = FALSE)
                   
                   ### Dropbox stuff
                   if(!dir.exists("www/csvs")) dir.create("www/csvs")
                   
                   timeString <- Sys.time() %>% str_replace_all("[:[:space:]]", "-")
                   write.csv(activities() %>% select(-number) %>% filter(activity != ""),
                             glue("www/csvs/response_{instance}_{timeString}.csv"),
                             row.names = FALSE)
                   
                   drop_upload( file = glue("www/csvs/response_{instance}_{timeString}.csv"),
                                path = "reflective_responses",
                                mode = "overwrite"
                   )
                }) # watch graphButton input
}

# Run the application
shinyApp(ui = ui, server = server)
