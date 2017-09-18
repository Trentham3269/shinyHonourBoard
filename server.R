shinyServer(function(input, output) {
  
  # DATA -------------------------------------------------------------------------------------------
  
  results_output <- reactive({
    
    if (input$selView == "Kings/Queens Prizes" & input$selHnrBrd == "Name"){
      
      if (input$selHnrBrdNm == "Select name from list"){
      
        # group historical results by winner
        df_kings_queens %>%
          group_by(Name) %>%
          summarise(`Kings/Queens Prize Count` = n()) %>%
          arrange(desc(`Kings/Queens Prize Count`))
        
      } else {
        
        # subset data frame with user's selections
        df_kings_queens %>% 
          select(Year
                 , Name
                 , `Kings/Queens Prize` = Championship) %>% 
          filter(Name == input$selHnrBrdNm) %>%
          arrange(desc(Year))
        
      }
      
    } else if (input$selView == "Kings/Queens Prizes" & input$selHnrBrd == "Year"){
    
      # subset data frame with user's selections
      df_kings_queens %>% 
        select(Year
               , Name
               , `Kings/Queens Prize` = Championship) %>% 
        filter(Year == input$selHnrBrdYr)
      
      #TODO order shoots within yr
      #TODO group winners for yr for plotly output

    } else if (input$selView == "Commonwealth Games"){
      
      if (input$selCommTyp == "Both"){
        
        df_comm_games %>% 
          select(Year
                 , Name
                 , Place
                 , Match = Type
                 , `Host City` = Location
                 , `Host Country` = Country) %>% 
          arrange(desc(Year))
        
      } else {
        
        df_comm_games %>% 
          select(Year
                 , Name
                 , Place
                 , Match = Type
                 , `Host City` = Location
                 , `Host Country` = Country) %>% 
          filter(Match == input$selCommTyp) %>% 
          arrange(desc(Year))
        
      }
    
    } else if (input$selView == "World Long Range Championships"){ 
      
      df_world_champs %>% 
        select(Year
               , Name
               , Place
               , Location
               , Country) %>% 
        arrange(desc(Year))
      
    } else if (input$selView == "Overseas Championships"){ 
      
      df_overseas_champs %>% 
        select(Year
               , Championship
               , Name
               , Place
               , Location
               , Country) %>% 
        arrange(desc(Year))
      
    } 
               
  })
  
  # TABLE ------------------------------------------------------------------------------------------
  
  output$results <- renderDataTable({
  
    results_output()
  
  }, options = list(paging = FALSE
                    , columnDefs = list(list(className = 'dt-center', targets = c("_all")))))
  
  # PLOT -------------------------------------------------------------------------------------------
  
  output$plot <- renderPlotly({
    
    if (input$selView     == "Kings/Queens Prizes" &
        input$selHnrBrd   == "Name" &
        input$selHnrBrdNm == "Select name from list"){
      
        # top 10 by Kings/Queens Prize Count
        results_output() %>% 
          top_n(n = 10) ->
        results_top
        
        # get order for x axis
        x_order <- list(results_top$Name)
        
        # plot
        plot_ly(data    = results_top
                , x     = ~Name
                , y     = ~`Kings/Queens Prize Count`
                , type  = "bar") %>%
        layout(title    = paste0("Top 10 Kings/Queens Winners")
               , xaxis  = list(title = "", categoryorder = "array", categoryarray = x_order)
               , yaxis  = list(title = "")
               , margin = list(l = 20, t = 40, r = 40, b = 90)) %>%
        config(displayModeBar = FALSE)
      
      } else if (input$selView     == "Kings/Queens Prizes" &
                 input$selHnrBrd   == "Name" &
                 input$selHnrBrdNm != "Select name from list"){
      
        # plot
        plot_ly(data    = results_output()
                , x     = ~`Kings/Queens Prize`
                , type  = "histogram") %>%
        layout(title    = paste0(input$selHnrBrdNm, " - Wins by Location")
               , xaxis  = list(title = "")
               , yaxis  = list(dtick = 1)
               , margin = list(l = 20, t = 40, r = 20, b = 50)) %>%
        config(displayModeBar = FALSE)
        
      } else if (input$selView   == "Kings/Queens Prizes" &
                 input$selHnrBrd == "Year"){
        
        # plot
        plot_ly(data    = results_output()
                , x     = ~Name
                , type  = "histogram") %>%
        layout(title    = paste0(input$selHnrBrdYr, " Kings/Queens Winners")
               , xaxis  = list(title = "")
               , yaxis  = list(dtick = 1)
               , margin = list(l = 20, t = 40, r = 20, b = 90)) %>%
        config(displayModeBar = FALSE)
      
      } else if (input$selView == "Commonwealth Games"){
        
        # order x axis
        x_order <- c("Gold", "Silver", "Bronze")
      
        # plot
        plot_ly(data    = results_output()
                , x     = ~Place
                , type  = "histogram") %>%
        layout(title    = "Commonwealth Games Medal Tally"
               , xaxis  = list(title = "", categoryorder = "array", categoryarray = x_order)
               , yaxis  = list(dtick = 1)
               , margin = list(l = 20, t = 40, r = 20, b = 50)) %>%
        config(displayModeBar = FALSE)
        
      } 
      
      else if (input$selView == "World Long Range Championships"){
        
        plotly_empty()
        
      } else if (input$selView == "Overseas Championships"){
        
        plotly_empty()
        
      } 
  
  })
  
  # DOWNLOAD ---------------------------------------------------------------------------------------
  
  # Switch for download's filename
  dwnld_nm <- reactive({
    
    switch(input$selView
           , "Kings/Queens Prizes"            = "Kings/Queens_Prizes"
           , "Commonwealth Games"             = "Commonwealth_Games"
           , "World Long Range Championships" = "WLRC"
           , "Overseas Championships"         = "Overseas_Championships")

  })
  
  # Download handler
  output$download <- downloadHandler(
    
    filename = function() {
      paste0(dwnld_nm(),".csv")
    }, 
    content = function(file) {
      write_csv(results_output(), file)
    }
  
  )

}) 