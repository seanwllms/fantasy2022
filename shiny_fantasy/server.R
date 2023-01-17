
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #########################################################
  ############## logic for conditional UI elements ########
  #########################################################
  
  #logic for player dropdown
  output$playerselect <- reactive({
    if (input$top_tabs %in% c("Best Left, NFBC", "Player Pool", "Targets")) {
      TRUE
    } else { FALSE }
  })
  outputOptions(output, "playerselect", suspendWhenHidden = FALSE)  
  
  
  #logic for roster dropdown
  output$teamselect <- reactive({
    if (input$top_tabs == "Rosters") {
      TRUE
    } else { FALSE }
  })
  outputOptions(output, "teamselect", suspendWhenHidden = FALSE) 
  
  
 
  
  #league params
  output$dollars_spent <- renderUI({
    HTML(paste0("<h3><b>Dollars Spent: </b>", scales::dollar(total_dollars_spent), "</h3>"))
  })
  
  output$dollars_remaining <- renderUI({
    HTML(paste0("<h3><b>Dollars Remaining: </b>", scales::dollar(total_dollars_remaining), "</h3>"))
  })
  
  output$value_remaining <- renderUI({
    HTML(paste0("<h3><b>Value Remaining: </b>", scales::dollar(round(total_value_remaining)), "</h3>"))
  })
  
  output$average_price <- renderUI({
    HTML(paste0("<h3><b>Average Price, Remaining Players: </b>", scales::dollar(average_price_remaining), "</h3>"))
  })
  
  output$inflation <- renderUI({
    HTML(paste0("<h3><b>Projected Inflation: </b>", scales::percent(inflation_adjustment-1), "</h3>"))
  })
  
  output$players_left_to_auction <- renderUI({
    HTML(paste0("<h3><b>Players Left to Auction: </b>", open_roster_spots, "</h3>"))
  })
  

  #full playerpool for testing
  output$playerpooltest <- renderTable({player_pool})
  

  
  #player pool dynamic table
  output$players <- renderTable({
    
    #filter output table by position
    output_table <- filter_position(player_pool, input$selected_pos)

    #build output table for pitchers
    if (input$selected_pos == "Pitchers") {

      output_table <- output_table %>% 
        select(Name, Team, status, IP:K, Points, Dollars, Auction, NFBC)
    #build output table for hitters
    } else {
      output_table <- select(output_table, Name, Team, position, status, PA:AVG, Points, Dollars, Auction, NFBC)
    }
    
    if (input$playersearch != "") {
      output_table <- output_table %>% 
        mutate(lowercasename = tolower(Name)) %>% 
        filter(str_detect(lowercasename, tolower(input$playersearch))) %>% 
        select(-lowercasename)
    }
    
    #filter out drafted players
    if (input$drafted == TRUE) {
      output_table %>% 
        mutate(Rank = row_number()) %>% 
        relocate(Rank)
    } else {
      output_table %>%  filter(!str_detect(status, "[0-9]")) %>% 
        mutate(Rank = row_number()) %>% 
        relocate(Rank)
    }

  })
  
  #standings table
  output$standings <- renderTable({standings})
  
  #roster table
  output$rostertable <- renderTable({
    rosters %>% 
      filter(team == input$teamselect) %>% 
      select(-team) 
  })
  
  #targets table
  output$targets <- renderTable({
    targets %>% 
      filter_position(input$selected_pos) %>% 
      filter(str_detect(tolower(Name), tolower(input$playersearch)))
  })
  
  #best remaining NFBC
  output$nfbc_best <- renderTable({
    
    nfbc <- player_pool %>%
      filter_position(input$selected_pos) 
    
    
    if (input$drafted == FALSE) {
      nfbc <- nfbc %>% filter(!str_detect(status, "[0-9]"))
    }
    
    nfbc %>% 
      select(Name, Team, position, Dollars, Auction, NFBC, status) %>% 
      arrange(as.numeric(NFBC)) %>% 
      filter(str_detect(tolower(Name), tolower(input$playersearch)))
  })
  
  output$today <- renderTable({
    player_pool %>% 
      filter(status == "") %>% 
      filter(row_number() < 26) %>% 
      select(Name, Team, Points, Dollars, Auction, NFBC) 
      
  })
  
  output$pointsgraph1 <- renderPlot({
    
    plot1 <- plotstandings %>%  
      ggplot(aes(x=spent, y=Points)) + 
      geom_point() +
      xlab("Dollars Spent") +
      ylab("Projected Points")+
      geom_text_repel(aes(x=spent, y=Points, label = team))  +
      geom_smooth(method='lm', se =FALSE, color = "black", size = .6) +
      fantasy_theme
    
    plot1
  })
  
  
  output$pointsgraph2 <- renderPlot({
    
    plot <- plotstandings %>%  
      ggplot(aes(x=left/picks_left, y=Points)) + 
      geom_point() +
      xlab("Dollars Left Per Pick") +
      ylab("Projected Points")+
      geom_text_repel(aes(x=left/picks_left, y=Points, label = team))  +
      geom_smooth(method='lm', se =FALSE, color = "black", size = .6) +
      fantasy_theme
    
    plot
  })

  
}



