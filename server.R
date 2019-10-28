library(shiny)
library(googleVis)
library(tidyr)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output){
    
    output$country_map = renderGvis(country_map(input))
    
    output$regional_overall_bar = renderGvis(regional_overall_bar(input))
    
    output$regional_trend_line = renderGvis(regional_trend_line(input))
    
    output$country_comparison = renderGvis(country_comparison(input))
    
    output$topEmisioner1 <- renderInfoBox(
        infoBox("Country with Lowest Emission ",
                number_compress(emmison_min_current_year), paste("Tones in ",year_to),
                icon = icon("chevron-down"), fill = TRUE))
    
    output$topEmisioner2 <- renderInfoBox(
        infoBox("Country with Highest Emission",
                number_compress(emmison_max_current_year), paste("Tones in ",year_to), 
                icon = icon("chevron-up"), fill = TRUE))
    
    
})