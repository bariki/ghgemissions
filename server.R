library(DT)
library(shiny)
library(googleVis)
library(tidyr)

shinyServer(function(input, output){
    
    output$regional_overall_bar = renderGvis(regional_overall_bar(input))
    
    output$regional_trend_line = renderGvis(regional_trend_line(input))
    
    output$country_comparison = renderGvis(country_comparison(input))
    
    output$topEmisioner1 <- renderInfoBox(
        infoBox("Country with Lowest Emission ",
                number_compress(emmison_min_current_year), paste("Tones in ",year_to),
                icon = icon("calculator"), fill = TRUE))
    
    output$topEmisioner2 <- renderInfoBox(
        infoBox("Country with Highest Emission",
                number_compress(emmison_max_current_year), paste("Tones in ",year_to), 
                icon = icon("calculator"), fill = TRUE))
    
    
})