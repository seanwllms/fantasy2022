# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("FBBL 2022"),
  
  sidebarPanel(
    
    #UI panel for player pool
      conditionalPanel(
        condition = "output.playerselect",
        selectInput("selected_pos", "Position", c("Hitters", "Pitchers", ALL_POSITIONS), selected="Hitters"),
        checkboxInput("drafted", "Show Drafted", value = FALSE),
        textInput(inputId = "playersearch",
                  label = "Search for Player",
                  value = "")
      ),
      #UI panel for roster tab
      conditionalPanel(
        condition = "output.teamselect",
        selectInput("teamselect", "Team", TEAMS, selected="marmaduke")
      )

  ),
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      id = "top_tabs",
      type = "tabs",
      tabPanel("Dashboard",
               br(),
               h3("Best Remaining"),
               fluidRow(
                 column(12, tableOutput("today"))
               ),
               fluidRow(
                 column(6, htmlOutput("dollars_spent")),
                 column(6, htmlOutput("dollars_remaining"))
               ),
               fluidRow(
                 column(6, htmlOutput("value_remaining")),
                 column(6, htmlOutput("players_left_to_auction"))
               ),
               fluidRow(
                 column(6, htmlOutput("average_price")),
                 column(6, htmlOutput("inflation"))
               )
 
      ),
      
      tabPanel("Player Pool", 
               tableOutput("players")),
      tabPanel("Standings", 
               tableOutput("standings")),
      tabPanel("Rosters", 
               tableOutput("rostertable")),
      tabPanel("Best Left, NFBC", 
               tableOutput("nfbc_best")), 
      tabPanel("Targets",
               tableOutput("targets")),
      tabPanel("Graphs",
               plotOutput("pointsgraph1", width="600px", height ="400px"),
               plotOutput("pointsgraph2", width="600px", height ="400px"))
    )
  )
)

