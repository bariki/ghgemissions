library(tidyr)
library(dplyr)
library(ggplot2)
library(googleVis)
library(shiny)
library(shinydashboard)

#library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#data source
dataSource = tbl_df(read.csv("df.csv"))

#country list
coutry_names = dataSource %>% select(country) %>% distinct()
region_names = dataSource %>% select(region) %>% distinct()
year_from = dataSource %>% select(year) %>% min()
year_to = dataSource %>% select(year) %>% max()


AG_SUM = 1
AG_AV = 2
AG_MAX = 3
AG_MIN = 4
AG_PER = 5

aggregate_types = list(
  "Summation" = AG_SUM,
  "Average" = AG_AV,
  "Maximum" = AG_MAX,
  "Minimum" = AG_MIN,
  "Percentage" = AG_PER
)

number_compress <- function(tx) {
  div <- findInterval(as.numeric(gsub("\\,", "", tx)),
                      c(0, 1e3, 1e6, 1e9, 1e12))
  paste(round(as.numeric(gsub("\\,", "", tx)) / 10 ^ (3 * (div - 1)), 2),
        c("", "K", "M", "B", "T")[div])
}

max_emission = function(input) {
  temp1 = dataSource %>% filter(year >= input$dashboard_year_filter[1] &
                                  year <= input$dashboard_year_filter[2])
  
  max_amount = temp1 %>% select(amount) %>% max()
  temp_year =  temp1 %>% filter(amount == max_amount) %>% select(year) %>% max()
  country =  temp1 %>% filter(amount == max_amount &
                                year == temp_year) %>% select(country) %>% head(1)
  
  infoBox(
    "High Emission",
    paste(number_compress(max_amount), "Tones"),
    paste(country$country, " in ", temp_year),
    icon = icon("chevron-up"),
    color = "red",
    fill = TRUE
  )
  
}

min_emission = function(input) {
  temp1 = dataSource %>% filter(
    year >= input$dashboard_year_filter[1] &
      year <= input$dashboard_year_filter[2] &
      amount > 0
  )
  
  min_amount = temp1 %>% select(amount) %>% min()
  temp_year =  temp1 %>% filter(amount == min_amount) %>% select(year) %>% max()
  country =  temp1 %>% filter(amount == min_amount &
                                year == temp_year) %>% select(country) %>% head(1)
  
  infoBox(
    "Low Emission",
    paste(number_compress(min_amount), "Tones"),
    paste(country$country, " in ", temp_year),
    icon = icon("chevron-down"),
    color = "yellow",
    fill = TRUE
  )
  
}

status_emission = function(input) {
  
  avg_emission = dataSource %>% filter(year >= input$dashboard_year_filter[1] &
                                         year <= input$dashboard_year_filter[2]) %>%
    mutate(avg = mean(amount)) %>% head(1) %>% select(avg)
  
  above = dataSource %>% filter(year >= input$dashboard_year_filter[1] &
                                  year <= input$dashboard_year_filter[2]) %>%
    group_by(country) %>%
    summarise(avg = mean(amount)) %>% filter(avg >= avg_emission$avg)
  
  below = dataSource %>% filter(year >= input$dashboard_year_filter[1] &
                                  year <= input$dashboard_year_filter[2]) %>%
    group_by(country) %>%
    summarise(avg = mean(amount)) %>% filter(avg < avg_emission$avg)
  
  infoBox(
    "Above average",
    paste(nrow(above), "/", (nrow(below)+nrow(above)) ),
    paste(
      number_compress(avg_emission$avg),
      " Tons",
      "Average Emission"
    ),
    icon = icon("balance-scale"),
    fill = TRUE
  )
  
}

### region overall bar graph
regional_overall_bar = function(input) {
  
  yvar = c('Tones')
  hAxis = "{title:'Tones'}"
  
  if( AG_SUM == input$regional_aggregate_filter ) {
    
    temp1 = dataSource %>% filter(year >= input$regional_year_filter[1] &
                                    year <= input$regional_year_filter[2]) %>%
      group_by(region) %>%
      summarise(Tones = sum(amount)) %>%
      arrange((Tones))
    
  }
  else if( AG_AV == input$regional_aggregate_filter ) {
    
    temp1 = dataSource %>% filter(year >= input$regional_year_filter[1] &
                                    year <= input$regional_year_filter[2]) %>%
      group_by(region) %>%
      summarise(Tones = mean(amount)) %>%
      arrange((Tones))
    
  }
  else if( AG_MAX == input$regional_aggregate_filter ) {
    
    temp1 = dataSource %>% filter(year >= input$regional_year_filter[1] &
                                    year <= input$regional_year_filter[2]) %>%
      group_by(region) %>%
      summarise(Tones = max(amount)) %>%
      arrange((Tones))
    
  }
  else if( AG_MIN == input$regional_aggregate_filter ) {
    
    temp1 = dataSource %>% filter(year >= input$regional_year_filter[1] &
                                    year <= input$regional_year_filter[2] & amount > 0 ) %>%
      group_by(region) %>%
      summarise(Tones = min(amount)) %>%
      arrange((Tones))
    
  }
  else if( AG_PER == input$regional_aggregate_filter ) {
    
    total = dataSource %>% filter(year >= input$regional_year_filter[1] &
                                    year <= input$regional_year_filter[2] ) %>%
      summarise(total = sum(amount))
    
    temp1 = dataSource %>% filter(year >= input$regional_year_filter[1] &
                                    year <= input$regional_year_filter[2] ) %>%
      group_by(region) %>%
      summarise(Percentage = sum(amount)/total$total * 100 ) %>%
      arrange((Percentage))
    
    yvar = c('Percentage')
    hAxis = "{title:'Percentage'}"
    
  }
  
  chart <-
    gvisBarChart(
      temp1,
      xvar = "region",
      yvar = yvar,
      options = list(
        vAxis = "{title:'Regions'}",
        hAxis = hAxis,
        bar = "{groupWidth:'90%'}",
        resolution = "provinces",
        width = "100%",
        height = "500px",
        legend = "{ position: 'none' }"
      )
    )
  
  return (chart)
  
}

### country map graph
country_map = function(input) {
  temp1 = dataSource %>% filter(year >= input$dashboard_year_filter[1] &
                                  year <= input$dashboard_year_filter[2]) %>%
    group_by(country) %>%
    summarise(Tones = sum(amount))
  
  chart = gvisGeoChart(
    temp1,
    locationvar = "country",
    colorvar = "Tones",
    options = list(width = '100%',
                   height = 500)
  )
  
  return (chart)
  
}


### country worldMap
country_word_map = function(input) {
  temp1 = dataSource %>% filter(year >= input$dashboard_year_filter[1] &
                                  year <= input$dashboard_year_filter[2]) %>%
    group_by(country) %>%
    summarise(Tones = sum(amount))
  
  map = wordcloud(
    words = temp1$country,
    freq = temp1$Tones,
    min.freq = 1,
    max.words = 10,
    random.order = FALSE,
    rot.per = 0,
    scale = c(2, 1),
    colors = brewer.pal(8, "Dark2")
  )
  
  return (map)
  
}

### region overall plotbox graph
temp2 = dataSource %>% group_by(region, year) %>% summarise(amount = sum(amount))
regional_overall_boxplot = ggplot(temp2, aes(x = region, y = amount)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal()
#plot(regional_overall_boxplot)


### region trend graph
regional_trend_line = function(input) {
  temp3 = dataSource %>% filter(year >= input$trend_year_filter[1] &
                                  year <= input$trend_year_filter[2]) %>%
    group_by(region, year) %>%
    summarise(Tones = sum(amount)) %>%
    spread(region, Tones)
  
  chart = gvisLineChart(
    temp3,
    xvar = "year",
    yvar = as.vector(names(temp3)[-1]),
    options = list(
      vAxis = "{title:'Tones'}",
      hAxis = "{title:'Year'}",
      resolution = "provinces",
      width = "100%",
      height = "500px",
      legend = "{ position: 'right' }"
    )
  )
  return (chart)
}


### country comparison trend graph
country_comparison = function(input) {
  temp4 = dataSource %>% filter(
    country %in% c(input$com_country1, input$com_country2, input$com_country3) &
      year >= input$com_year_filter[1] &
      year <= input$com_year_filter[2]
  ) %>%
    group_by(country, year) %>%
    summarise(Tones = sum(amount)) %>%
    spread(country, Tones)
  
  chart = gvisLineChart(
    temp4,
    xvar = "year",
    yvar = as.vector(names(temp4)[-1]),
    options = list(
      vAxis = "{title:'Tones'}",
      hAxis = "{title:'Year'}",
      resolution = "provinces",
      width = "100%",
      height = "500px",
      legend = "{ position: 'top' }"
    )
  )
  
  return (chart)
  
}


### emission rate chart
emission_rate_chart = function(input) {
  
  temp1 = dataSource
  if (input$decrease_emission_region_filter != "ALL") {
    temp1 = dataSource %>% filter( region == input$decrease_emission_region_filter)
  }
  
  temp2 = temp1 %>% filter(year >= input$decrease_emission_year_filter[1] &
                                  year <= input$decrease_emission_year_filter[2]) %>%
    group_by(country) %>%
    mutate(diff = amount - lag(amount)) %>% drop_na()
    
  temp3 = temp2 %>% group_by(country) %>% summarise(Tones = -1*mean(diff))
  
  if (input$decrease_emission_type == "Best") {
    temp3 = temp3 %>% arrange( desc(Tones) ) %>% head( input$decrease_emission_num )
  }
  else {
    temp3 = temp3 %>% arrange( Tones ) %>% head( input$decrease_emission_num )
  }
  
  chart <-
    gvisBarChart(
      temp3,
      xvar = "country",
      yvar = c('Tones'),
      options = list(
        vAxis = "{title:'Country'}",
        hAxis = "{title:'Average rate of decrease in Tones per year'}",
        bar = "{groupWidth:'80%'}",
        resolution = "provinces",
        width = "100%",
        height = "600px",
        legend = "{ position: 'none' }"
      )
    )
  
  return (chart)
  
}
