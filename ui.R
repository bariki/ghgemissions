


shinyUI(dashboardPage(
    dashboardHeader(title = "G gases Emissions", titleWidth = 260),
    dashboardSidebar(
        # sidebarUserPanel("OPEN DATA",
        #                  image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"
        # ),
        # hr(),
        
        sidebarMenu(
            menuItem("Dashboard",
                     tabName = "dashboard",
                     icon = icon("home")),
            menuItem(
                "Reginal Overview",
                tabName = "regional_overall",
                icon = icon("angle-double-right")
            ),
            menuItem(
                "Reginal Trends",
                tabName = "regional_trend",
                icon = icon("angle-double-right")
            ),
            menuItem(
                "Country Trends",
                tabName = "country_comparison",
                icon = icon("angle-double-right")
            ),
            menuItem(
                "Emission Decrease Rate",
                tabName = "decrease_emission_rate",
                icon = icon("angle-double-right")
            ),
            menuItem(
                "Data Source",
                tabName = "data_table",
                icon = icon("angle-double-right")
            )
            # hr(),
            # menuItem("Country Profile", tabName = "country_profile", icon = icon("database")),
            # menuItem("Country Comparisons", tabName = "map", icon = icon("database")),
            # menuItem("Country Profile", tabName = "data", icon = icon("database"))
            
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            tabItem(
                tabName = "dashboard",
                fluidRow(
                    infoBoxOutput("min_emission", width = 4),
                    infoBoxOutput("max_emission", width = 4),
                    infoBoxOutput("status_emission", width = 4)
                ),
                
                fluidRow(column(
                    12,
                    sliderInput(
                        "dashboard_year_filter",
                        "Year between:",
                        min = year_from,
                        max = year_to,
                        value = c(year_from, year_to),
                        step = 1,
                        width = "99%"
                    )
                )),
                
                fluidRow(column(
                    12, box(
                        HTML("<h2 class='title'>Country Green House Gas Emissions</h2>"),
                        column(8, htmlOutput("country_map")),
                        column(4, plotOutput("country_word_map")),
                        width = "100%",
                        height = 600
                    )
                ))
                
            ),
            
            tabItem(tabName = "regional_overall",
                    
                    fluidRow(
                        column(
                            4,
                            selectInput(
                                "regional_aggregate_filter",
                                "Aggregate Type",
                                choices = aggregate_types,
                                selected = 1
                            )
                        ),
                        column(
                            8,
                            sliderInput(
                                "regional_year_filter",
                                "Year between:",
                                min = year_from,
                                max = year_to,
                                value = c(year_from, year_to),
                                step = 1,
                                width = "99%"
                            )
                        )
                    ),
                    
                    fluidRow(column(
                        12, box(
                            HTML("<h2 class='title'>Reginal House Green Gas Emissions</h2>"),
                            htmlOutput("regional_overall_bar"),
                            width = "100%",
                            height = 600
                        )
                    ))),
            
            
            tabItem(tabName = "regional_trend",
                    
                    fluidRow(column(
                        12,
                        sliderInput(
                            "trend_year_filter",
                            "Year between:",
                            min = year_from,
                            max = year_to,
                            value = c(year_from, year_to),
                            step = 1,
                            width = "99%"
                        )
                    )),
                    
                    fluidRow(column(
                        12, box(
                            HTML(
                                "<h2 class='title'> Reginal Trends of Green House Gas Emissions per year</h2>"
                            ),
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
                        step = 1,
                        width = "99%"
                    )
                )),
                
                fluidRow(box(
                    HTML(
                        "<h2 class='title'>Country Trends of Green House Gas Emissions per year</h2>"
                    ),
                    htmlOutput("country_comparison"),
                    width = 12,
                    height = 600
                ))
            ),
            
            tabItem(tabName = "decrease_emission_rate",
                    
                    fluidRow(
                        column(2,
                               selectInput(
                                   "decrease_emission_region_filter", "Region:", c("ALL", unique(as.character(
                                       region_names$region
                                   )))
                               )),
                        column(2,
                               selectInput(
                                   "decrease_emission_type", "Decrease Rate:", c("Best","Worst")
                               )),
                        column(2,
                               numericInput("decrease_emission_num", "Number of Country", 10, min = 1, max = 30)
                               ),
                        column(
                            6,
                            sliderInput(
                                "decrease_emission_year_filter",
                                "Year between:",
                                min = year_from,
                                max = year_to,
                                value = c(year_from, year_to),
                                step = 1,
                                width = "99%"
                            )
                        )
                    ),
                    
                    fluidRow(column(
                        12, box(
                            HTML("<h2 class='title'> Green House Gas Emissions Decrease Rate</h2>"),
                            htmlOutput("emission_rate_chart"),
                            width = "100%",
                            height = 800
                        )
                    ))),
            
            tabItem(tabName = "data_table",
                    
                    fluidRow(
                        column(4,
                               selectInput(
                                   "table_region_filter", "Region:", c("ALL", unique(as.character(
                                       region_names$region
                                   )))
                               )),
                        column(
                            8,
                            sliderInput(
                                "table_year_filter",
                                "Year between:",
                                min = year_from,
                                max = year_to,
                                value = c(year_from, year_to),
                                step = 1,
                                width = "99%"
                            )
                        )
                    ),
                    
                    fluidRow(column(
                        12, box(
                            HTML("<h2 class='title'> Green House Gas Emissions Data</h2>"),
                            DT::dataTableOutput("table"),
                            width = "100%",
                            height = 600
                        )
                    )))
            
        ),
        
        
        fluidRow(column(
            12,
            HTML(
                "<p class='footer'>Bariki Elilaki : bariki.elilaki@gmail.com</p>"
            )
            
        ))
        
    )
))