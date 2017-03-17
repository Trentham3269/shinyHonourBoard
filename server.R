shinyServer(function(input, output) {
  
  # DATA -------------------------------------------------------------------------------------------
  
  results.output <- reactive({
    
    if (input$selView == "Kings/Queens Prizes"){
    
      if (input$selHnrBrdNm == "Select name from list"){
      
        # group historical results by winner
        df.hist %>%
          select(everything()) %>% 
          filter(Winner != 'NA') %>%
          group_by(Winner) %>%
          summarise(`Kings/Queens Prize Count` = n()) %>%
          arrange(desc(`Kings/Queens Prize Count`))
        
      } else {
        
        # subset data frame with user's selections
        df.hist %>% 
          select(Year
                 , Winner
                 , `Kings/Queens Prize` = State) %>% 
          filter(Winner != 'NA' &
                 Winner == input$selHnrBrdNm) %>%
          arrange(desc(Year))
        
      }
      
    } else if (input$selView == "Commonwealth Games"){
      
      if (input$selCommTyp == "Both"){
        
        df.comm %>% 
          select(Year
                 , Medal
                 , Winner
                 , Match = Type
                 , `Host City` = Host_City
                 , `Host Country` = Host_Nation) %>% 
          arrange(desc(Year))
        
      } else {
        
        df.comm %>% 
          select(Year
                 , Medal
                 , Winner
                 , Match = Type
                 , `Host City` = Host_City
                 , `Host Country` = Host_Nation) %>% 
          filter(Match == input$selCommTyp) %>% 
          arrange(desc(Year))
        
      }
    
    } else if (input$selView == "World Long Range Championships"){ 
      
      df.wlrc %>% 
        arrange(desc(Year))
      
    } else if (input$selView == "Overseas Championships"){ 
      
      df.nonQ %>% 
        arrange(desc(Year))
      
    } 
               
  })
  
  # TABLE ------------------------------------------------------------------------------------------
  
  output$results <- renderDataTable({
  
    results.output()
  
  }, options = list(paging = FALSE
                    , columnDefs = list(list(className = 'dt-center', targets = c("_all")))))
  
  # PLOT -------------------------------------------------------------------------------------------
  
  output$plot <- renderPlotly({
    
    if (input$selView == "Kings/Queens Prizes"){
        
      if (input$selHnrBrdNm == "Select name from list"){
      
        # top 10 by Kings/Queens Prize Count
        results.output() %>% 
          top_n(n = 10) ->
        results.top
        
        # get order for x axis
        x.order <- list(results.top$Winner)
        
        # plot
        plot_ly(data    = results.top
                , x     = ~Winner
                , y     = ~`Kings/Queens Prize Count`
                , type  = "bar") %>%
        layout(title    = paste0("Top 10 Kings/Queens Winners")
               , xaxis  = list(title = "", categoryorder = "array", categoryarray = x.order)
               , yaxis  = list(title = "")
               , margin = list(l = 20, t = 40, r = 40, b = 55)) %>%
        config(displayModeBar = FALSE)
      
      } else if (input$selHnrBrdNm != "Select name from list"){
      
        # plot
        plot_ly(data    = results.output()
                , x     = ~`Kings/Queens Prize`
                , type  = "histogram") %>%
        layout(title    = paste0(input$selHnrBrdNm, " - Wins by Location")
               , xaxis  = list(title = "")
               , yaxis  = list(dtick = 1)
               , margin = list(l = 20, t = 40, r = 20, b = 50)) %>%
        config(displayModeBar = FALSE)
        
      } 
      
    } else if (input$selView == "Commonwealth Games"){
      
      # order x axis
      x.order <- c("Gold", "Silver", "Bronze")
    
      # plot
      plot_ly(data    = results.output()
              , x     = ~Medal
              , type  = "histogram") %>%
      layout(title    = "Commonwealth Games Medal Tally"
             , xaxis  = list(title = "", categoryorder = "array", categoryarray = x.order)
             , yaxis  = list(dtick = 1)
             , margin = list(l = 20, t = 40, r = 20, b = 50)) %>%
      config(displayModeBar = FALSE)
      
    } else if (input$selView == "World Long Range Championships"){
      
      plotly_empty()
      
    } else if (input$selView == "Overseas Championships"){
      
      plotly_empty()
      
    } 
  
  })
  
  # DOWNLOAD ---------------------------------------------------------------------------------------
  
  # Switch for download's filename
  dwnld.nm <- reactive({
    
    switch(input$selView
           , "Kings/Queens Prizes"            = "Kings/Queens_Prizes"
           , "Commonwealth Games"             = "Commonwealth_Games"
           , "World Long Range Championships" = "WLRC"
           , "Overseas Championships"         = "Overseas_Championships")

  })
  
  # Download handler
  output$download <- downloadHandler(
    
    filename = function() {
      paste0(dwnld.nm(),".csv")
    }, 
    content = function(file) {
      write_csv(results.output(), file)
    }
  
  )

}) 