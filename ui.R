shinyUI( 
  
  fluidPage(
    
    # Apply free bootswatch theme
    theme = shinytheme("flatly"),
    
    # Include Google Analytics
    tags$head(includeScript("www/google-analytics.js")),
    
    h2(titlePanel("National Rifle Association of Australia")),
    h3(titlePanel("Target Rifle Honour Board")),
    br(),
    
    sidebarLayout(

      sidebarPanel(width = 4,
                   
        radioButtons(inputId = "selView"
                     , label = "Select Honour Board to view:"
                     , choices = c("Kings/Queens Prizes"
                                   , "Commonwealth Games"
                                   , "World Long Range Championships"
                                   , "Overseas Championships")
                     , selected = "Kings/Queens Prizes"
        ),
        
        conditionalPanel(condition = "input.selView == 'Kings/Queens Prizes'",
          radioButtons(inputId = "selHnrBrd"
                       , label = "View by name, year or state:"
                       , choices = c("Name", "Year", "State")
                       , inline = TRUE)
        ),
          
        conditionalPanel(condition = "input.selHnrBrd == 'Name' && input.selView == 'Kings/Queens Prizes'",         
          selectInput(inputId = "selHnrBrdNm"
                      , label = "Choose name to list win(s):"
                      , choices = c("Select name from list", sort(unique(df_kings_queens$Name))))
        ),
        
        conditionalPanel(condition = "input.selHnrBrd == 'Year'",
          selectInput(inputId = "selHnrBrdYr"
                      , label = "Choose year to list winners:"
                      , choices = sort(unique(df_kings_queens$Year), decreasing = TRUE))
        ),
        
        conditionalPanel(condition = "input.selHnrBrd == 'State'",         
          selectInput(inputId = "selHnrBrdSt"
                      , label = "Choose state to list winners:"
                      , choices = c("ACT", "NAT", "NSW", "NQ", "NT", "QLD", "SA", "TAS", "VIC", "WA"))
        ),
        
        conditionalPanel(condition = "input.selView == 'Commonwealth Games'",
          selectInput(inputId = "selCommTyp"
                      , label = "Choose Individual or Pairs results:"
                      , choices = c("Both", sort(df_comm_games$Type)))
        ),
        
        conditionalPanel(condition = "input.selView == 'Kings/Queens Prizes' || 
                                      input.selView == 'Commonwealth Games'",
                         br(),
                         plotlyOutput("plot", width = "100%", height = "300px"),
                         br()
        ),
        
        downloadButton(outputId = "download"
                       , label  = "Download Data"
                       , class  = "btn-info"
        ),
        
        br(),
        br(),
        
        p("Data updated on 12.08.2018 and now includes 2018 QLD Championships"),
        
        a(href="https://trentham3269.shinyapps.io/shinyrangemap/", "NRAA Ranges")
      ),
      
      # Data Table
      mainPanel( 
        dataTableOutput(outputId = "results")   
      )
    ) 
  )
)
