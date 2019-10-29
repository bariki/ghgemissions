
shinyServer(function(input, output){
    
    output$country_map = renderGvis(country_map(input))
    
    output$country_word_map = renderPlot( country_word_map(input) )
    
    output$regional_overall_bar = renderGvis(regional_overall_bar(input))
    
    output$regional_trend_line = renderGvis(regional_trend_line(input))
    
    output$country_comparison = renderGvis(country_comparison(input))
    
    output$min_emission <- renderInfoBox( min_emission(input) )
    
    output$max_emission <- renderInfoBox( max_emission(input) )
    
    output$status_emission <- renderInfoBox( status_emission(input) )
    
    output$table <- DT::renderDataTable(DT::datatable({
        data <- dataSource %>% filter(year >= input$table_year_filter[1] & year <= input$table_year_filter[2])
        if (input$table_region_filter != "ALL") {
            data <- data[data$region == input$table_region_filter,]
        }
        names(data) = toupper(names(data))
        data
    }))
    
    
})