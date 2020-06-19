## useful tools ####

tibble(firstbit = "observeEvent(input$",
       secondbit = ", {vars$userChoices <- c(vars$userChoices, input$",
       thirdbit = ")
       vars$",
       fourthbit = " <- input$",
       endbit = "})",
       num = rep(1:5,7),
       tabName = rep(c("community", "discipleship", "communal_worship",
                       "sacraments", "evangelism", "social_action", "prayer"), 5)) -> makeTib

writeClipboard(
  makeTib %>% 
    arrange(tabName, num) %>% 
    transmute(paste0(firstbit, tabName, "_feeling_", num,
                     secondbit, tabName, "_feeling_", num, 
                     thirdbit, tabName, "_feeling_", num,
                     fourthbit, tabName, "_feeling_", num,
                     endbit)) %>% 
                pull
)

