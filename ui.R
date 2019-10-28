library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title = "GHG Emissions"),
    dashboardSidebar(# sidebarUserPanel("OPEN DATA",
        #                  image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"
        # ),
        # hr(),
        
        sidebarMenu(
            menuItem(
                "Regional Overall",
                tabName = "regional_overall",
                icon = icon("map")
            ),
            menuItem(
                "Reginal Trends",
                tabName = "regional_trend",
                icon = icon("database")
            ),
            menuItem(
                "Country Comparisons",
                tabName = "country_comparison",
                icon = icon("database")
            )
            # hr(),
            # menuItem("Country Profile", tabName = "country_profile", icon = icon("database")),
            # menuItem("Country Comparisons", tabName = "map", icon = icon("database")),
            # menuItem("Country Profile", tabName = "data", icon = icon("database"))
            
        )),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            tabItem(
                tabName = "regional_overall",
                fluidRow(
                    infoBoxOutput("topEmisioner1", width = 6),
                    infoBoxOutput("topEmisioner2", width = 6)
                ),
                
                fluidRow(column(
                    12,
                    sliderInput(
                        "overall_year_filter",
                        "Year between:",
                        min = year_from,
                        max = year_to,
                        value = c(year_from, year_to),
                        width = "90%"
                    )
                )),
                
                fluidRow(column(
                    12, box(
                        htmlOutput("regional_overall_bar"),
                        width = "100%",
                        height = 600
                    )
                ))
            ),
            
            tabItem(tabName = "regional_trend",
                    
                    fluidRow(column(
                        12,
                        sliderInput(
                            "trend_year_filter",
                            "Year between:",
                            min = year_from,
                            max = year_to,
                            value = c(year_from, year_to),
                            width = "90%"
                        )
                    )),
                    
                    fluidRow(column(
                        12, box(
                            htmlOutput("regional_trend_line"),
                            width = "100%",
                            height = 600
                        )
                    ))),
            
            tabItem(
                tabName = "country_comparison",
                
                fluidRow(
                    column(
                        4,
                        selectizeInput(
                            inputId = "com_country1",
                            label = "Country",
                            choices = unique(coutry_names)
                        )
                    ),
                    column(
                        4,
                        selectizeInput(
                            inputId = "com_country2",
                            label = "Country",
                            choices = unique(coutry_names)
                        )
                    ),
                    column(
                        4,
                        selectizeInput(
                            inputId = "com_country3",
                            label = "Country",
                            choices = unique(coutry_names)
                        )
                    )
                    
                ),
                
                fluidRow(column(
                    12,
                    sliderInput(
                        "com_year_filter",
                        "Year between:",
                        min = year_from,
                        max = year_to,
                        value = c(year_from, year_to),
                        width = "90%"
                    )
                )),
                
                fluidRow(box(
                    htmlOutput("country_comparison"),
                    width = 12,
                    height = 600
                ))
            )
 
        )
    )
))