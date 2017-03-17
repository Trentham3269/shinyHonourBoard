# nraaHonourBoard UI
shinyUI( 
  
  fluidPage(
    
    # include Google Analytics
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
          selectInput(inputId = "selHnrBrdNm"
                      , label = "Choose name to list win(s):"
                      , choices = c("Select name from list", df.sort$Winner))
        ),
        
        conditionalPanel(condition = "input.selView == 'Commonwealth Games'",
          selectInput(inputId = "selCommTyp"
                      , label = "Choose Individual or Pairs results:"
                      , choices = c("Both", sort(df.comm$Type)))
        ),
        
        br(),
        
        downloadButton(outputId = "download"
                       , label = "Download Data"
                       , class = "btn-primary"
        ),
        
        br(),
        br(),
        
        conditionalPanel(condition = "input.selView == 'Kings/Queens Prizes' || 
                                      input.selView == 'Commonwealth Games'",
                         br(),
                         plotlyOutput("plot", width = "100%", height = "300px"),
                         br()
        ),
        
        p("Note: additional Channel Islands, Guernsey and Papua New Guinea results are being 
          confirmed prior to inclusion.")
      ),
      
      # Data Table
      mainPanel( 
        dataTableOutput(outputId = "results")   
      )
    ) 
  )
)


