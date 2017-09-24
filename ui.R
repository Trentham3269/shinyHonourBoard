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
                       , label = "View by name or year:"
                       , choices = c("Name", "Year")
                       , inline = TRUE)
        ),
          
        conditionalPanel(condition = "input.selHnrBrd == 'Name'",         
          selectInput(inputId = "selHnrBrdNm"
                      , label = "Choose name to list win(s):"
                      , choices = c("Select name from list", sort(unique(df_kings_queens$Name))))
        ),
        
        conditionalPanel(condition = "input.selHnrBrd == 'Year'",
          selectInput(inputId = "selHnrBrdYr"
                      , label = "Choose year to list wins:"
                      , choices = sort(unique(df_kings_queens$Year), decreasing = TRUE))
        ),
        
        conditionalPanel(condition = "input.selView == 'Commonwealth Games'",
          selectInput(inputId = "selCommTyp"
                      , label = "Choose Individual or Pairs results:"
                      , choices = c("Both", sort(df_comm_games$Type)))
        ),
        
        conditionalPanel(condition = "input.selView == 'Kings/Queens Prizes' || 
                                      input.selView == 'Commonwealth Games' ||
                         input.selHnrBrd != 'Year'",
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
        
        p("Data is current as at 17.09.2017"),
        
        a(href="https://trentham3269.shinyapps.io/shinyrangemap/", "NRAA Ranges")
      ),
      
      # Data Table
      mainPanel( 
        dataTableOutput(outputId = "results")   
      )
    ) 
  )
)
