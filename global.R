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
df1 = tbl_df(read.csv("df.csv"))

#country list
coutry_names = df1 %>% select(country) %>% distinct()
year_from = df1 %>% select(year) %>% min()
year_to = df1 %>% select(year) %>% max()
emmison_max_overall = df1 %>% select(amount) %>% max()
emmison_max_current_year = df1 %>% filter(year == year_to)  %>% select(amount) %>% max()
emmison_min_current_year = df1 %>% filter(year == year_to &
                                            amount > 0)  %>% select(amount) %>% min()

number_compress <- function(tx) {
  div <- findInterval(as.numeric(gsub("\\,", "", tx)),
                      c(0, 1e3, 1e6, 1e9, 1e12))
  paste(round(as.numeric(gsub("\\,", "", tx)) / 10 ^ (3 * (div - 1)), 2),
        c("", "K", "M", "B", "T")[div])
}

### region overall bar graph
regional_overall_bar = function(input) {
  temp1 = df1 %>% filter(year >= input$regional_year_filter[1] &
                           year <= input$regional_year_filter[2]) %>%
    group_by(region) %>%
    summarise(Tones = sum(amount)) %>%
    arrange((Tones))
  chart <-
    gvisBarChart(
      temp1,
      xvar = "region",
      yvar = c('Tones'),
      options = list(
        vAxis = "{title:'Regions'}",
        hAxis = "{title:'Tones'}",
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
  temp1 = df1 %>% filter(year >= input$dashboard_year_filter[1] &
                           year <= input$dashboard_year_filter[2]) %>%
    group_by(country) %>%
    summarise(Tones = sum(amount))
  
  chart = gvisGeoChart(
    temp1,
    locationvar = "country",
    colorvar = "Tones",
    options = list(
      width = '100%',
      height = 500
    )
  )
  
  return (chart)
  
}


### country worldMap
country_word_map = function(input) {
  
  temp1 = df1 %>% filter(year >= input$dashboard_year_filter[1] &
                           year <= input$dashboard_year_filter[2]) %>%
    group_by(country) %>%
    summarise(Tones = sum(amount))
  
  map = wordcloud(words = temp1$country, freq = temp1$Tones, min.freq = 1,
            max.words=10, random.order=FALSE, rot.per=0, scale=c(2,1),
            colors=brewer.pal(8, "Dark2"))
  
  return (map)
  
}

### region overall plotbox graph
temp2 = df1 %>% group_by(region, year) %>% summarise(amount = sum(amount))
regional_overall_boxplot = ggplot(temp2, aes(x = region, y = amount)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal()
#plot(regional_overall_boxplot)


### region trend graph
regional_trend_line = function(input) {
  temp3 = df1 %>% filter(year >= input$trend_year_filter[1] &
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
  temp4 = df1 %>% filter(
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
