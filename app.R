library(RCurl)
library(tidyverse)
library(glue)
library(DT)
library(plyr)
library(plotly)
library(RColorBrewer)

simpl           <- read_csv(getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
simpl_deaths    <- read_csv(getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"))
simpl_recovered <- read_csv(getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"))

simplr <- simpl %>% 
  dplyr::rename(
    sub_state = `Province/State`,
    country   = `Country/Region`
  ) %>%
  select(-c(Lat, Long, sub_state)) %>%
  group_by(country) %>%
  summarize_each(list(~sum(., na.rm = T))) %>%
  ungroup()

simplr_deaths <- simpl_deaths %>% 
  dplyr::rename(
    sub_state = `Province/State`,
    country   = `Country/Region`
  ) %>%
  select(-c(Lat, Long, sub_state)) %>%
  group_by(country) %>%
  summarize_each(list(~sum(., na.rm = T))) %>%
  ungroup()

simplr_recovered <- simpl_recovered %>% 
  dplyr::rename(
    sub_state = `Province/State`,
    country   = `Country/Region`
  ) %>%
  select(-c(Lat, Long, sub_state)) %>%
  group_by(country) %>%
  summarize_each(list(~sum(., na.rm = T))) %>%
  ungroup()

simplst <- simplr %>%
  gather(
    date,
    confirmed,
    -country
  ) %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%y")
  )

simplst_deaths <- simplr_deaths %>%
  gather(
    date,
    deaths,
    -country
  ) %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%y")
  )

simplst_recovered <- simplr_recovered %>%
  gather(
    date,
    recovered,
    -country
  ) %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%y")
  )

simplst <- left_join(simplst, simplst_deaths, by = c("country", "date")) %>%
  left_join(simplst_recovered, by = c("country", "date")) %>%
  filter(country %in% c("US", "China", "Korea, South", "Italy", "Iran",
                        "India", "Spain", "Germany", "France", "Switzerland",
                        "United Kingdom")) %>%
  mutate(
    country = as.factor(country),
    pop    = case_when(
      country == "US" ~ 330000000,
      country == "China" ~ 1386000000,
      country == "Korea, South" ~ 51470000,
      country == "Italy" ~ 60480000,
      country == "Iran" ~ 81160000,
      country == "India" ~ 1339000000,
      country == "Spain" ~ 46660000,
      country == "Germany" ~ 82790000,
      country == "France" ~ 66990000,
      country == "Switzerland" ~ 8570000,
      country == "United Kingdom" ~ 66440000
    )
  )

ui <- fluidPage(
  tags$style("* { font-family: Arial; }"),
  
  titlePanel("COVID-19 Outbreak Plot"),
  
  sidebarLayout(
    sidebarPanel(
      p("This plot represents the cumulative number of confirmed COVID-19 cases by selected countries."),
      a("\n JHU CSSE Source Data (Github)", target="_blank", href="https://github.com/CSSEGISandData/COVID-19"),
      br(),
      a("\n JHU Interactive COVID-19 Map", target="_blank", href="https://coronavirus.jhu.edu/map.html"),
      
      dateRangeInput("dates",  h4("Date Range"), start = min(simplst$date), end = max(simplst$date)),
      checkboxGroupInput(inputId = "line",
                         label   = h4("Which countries would you like to plot?"),
                         choices = unique(simplst$country))
    ),
    
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Confirmed", plotOutput("plot"),
                           downloadButton(outputId = "downloadPlot", label = "Download Plot")),
                  tabPanel("Recovered", plotOutput("plot_1"),
                           downloadButton(outputId = "downloadPlot_1", label = "Download Plot")),
                  tabPanel("Deaths", plotOutput("plot_2"),
                           downloadButton(outputId = "downloadPlot_2", label = "Download Plot")),
                  tabPanel("Table", DT::dataTableOutput("table"),
                           downloadButton("downloadTable", "Download Table")),
                  tabPanel("Rate Plot", plotOutput("rate_plot"),
                           downloadButton("downloadRate_plot", "Download Plot")),
                  tabPanel("Rate-Trend Plot", plotOutput("rate_trend_plot"),
                           downloadButton("downloadRate_trend_plot", "Download Plot")))
      # 
      # plotOutput("plot_1")
    )
  )
)

server <- function(input, output) {
  
  plot_input <- reactive({
    validate(need(!is.null(input$line), "Please tick a box to show a plot."))
    
    data <- simplst %>% 
      filter(country %in% input$line) %>%
      filter(date >= input$dates[1] & date <= input$dates[2])
    
    ggplot(data = data, aes(x = date, y = confirmed, group = country)) +
      geom_line(aes(color = country), size = 1) +
      labs(
        title   = "Confirmed Covid-19 cases by country",
        x       = "Date",
        y       = "Count",
        color   = "Country",
        caption = "Johns Hopkins CSSE: https://github.com/CSSEGISandData/COVID-19"
      ) +
      scale_color_brewer(palette="Dark2") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.text=element_text(size=10),
            axis.title=element_text(size=12))
  })
  
  plot_1_input <- reactive({
    validate(need(!is.null(input$line), "Please tick a box to show a plot."))
    
    data <- simplst %>% 
      filter(country %in% input$line) %>%
      filter(date >= input$dates[1] & date <= input$dates[2])
    
    ggplot(data = data, aes(x = date, y = recovered, group = country)) +
      geom_line(aes(color = country), size = 1) +
      labs(
        title   = "Recovered Covid-19 cases by country",
        x       = "Date",
        y       = "Count",
        color   = "Country",
        caption = "Johns Hopkins CSSE: https://github.com/CSSEGISandData/COVID-19"
      ) +
      scale_color_brewer(palette="Dark2") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.text=element_text(size=10),
            axis.title=element_text(size=12))
  })
  
  plot_2_input <- reactive({
    validate(need(!is.null(input$line), "Please tick a box to show a plot."))
    
    data <- simplst %>% 
      filter(country %in% input$line) %>%
      filter(date >= input$dates[1] & date <= input$dates[2])
    
    ggplot(data = data, aes(x = date, y = deaths, group = country)) +
      geom_line(aes(color = country), size = 1) +
      labs(
        title   = "Covid-19 deaths by country",
        x       = "Date",
        y       = "Count",
        color   = "Country",
        caption = "Johns Hopkins CSSE: https://github.com/CSSEGISandData/COVID-19"
      ) +
      scale_color_brewer(palette="Dark2") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.text=element_text(size=10),
            axis.title=element_text(size=12))
  })
  
  rate_plot_input <- reactive({
    validate(need(!is.null(input$line), "Please tick a box to show a plot."))
    
    data <- simplst %>% 
      filter(country %in% input$line) %>%
      filter(date == input$dates[2]) %>%
      mutate(
        confirmed = round((confirmed / pop) * 100000, 1),
        deaths    = round((deaths / pop) * 100000, 1),
        recovered = round((recovered / pop) * 100000, 1)
      ) %>%
      select(-pop) %>%
      gather(stat, count, confirmed:recovered) %>%
      mutate(
        stat = as.factor(stat)
      )
    
    ggplot(data = data, aes(x = stat, y = count, group = country)) +
      geom_bar(aes(fill = country), stat = "identity", position = "dodge") +
      geom_text(aes(label = count), vjust = -.5, position = position_dodge(width = 0.9)) +
      labs(
        title   = glue("Comparison of metrics by country ({input$dates[2]})"),
        x       = "Metric",
        y       = "Count (per 100,000)",
        color   = "Country",
        caption = "Johns Hopkins CSSE: https://github.com/CSSEGISandData/COVID-19"
      ) +
      scale_fill_brewer(palette="Dark2") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.text=element_text(size=10),
            axis.title=element_text(size=12))
  })
  
  rate_trend_plot_input <- reactive({
    validate(need(!is.null(input$line), "Please tick a box to show a plot."))
    
    data <- simplst %>% 
      filter(country %in% input$line) %>%
      filter(date >= input$dates[1] & date <= input$dates[2]) %>%
      mutate(
        confirmed = round((confirmed / pop) * 100000, 1),
        deaths    = round((deaths / pop) * 100000, 1),
        recovered = round((recovered / pop) * 100000, 1)
      )
    
    ggplot(data = data, aes(x = date, y = confirmed, group = country)) +
      geom_line(aes(color = country), size = 1) +
      labs(
        title   = "Confirmed Covid-19 cases (per 100,000) by country",
        x       = "Date",
        y       = "Count (per 100,000)",
        color   = "Country",
        caption = "Johns Hopkins CSSE: https://github.com/CSSEGISandData/COVID-19"
      ) +
      scale_color_brewer(palette="Dark2") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.text=element_text(size=10),
            axis.title=element_text(size=12))
  })
  
  output$plot <- renderPlot({ plot_input() })
  output$plot_1 <- renderPlot({ plot_1_input() })
  output$plot_2 <- renderPlot({ plot_2_input() })
  output$rate_plot <- renderPlot({ rate_plot_input() })
  output$rate_trend_plot <- renderPlot({ rate_trend_plot_input() })
  
  d <- reactive({
    simplst %>% 
      filter(country %in% input$line) %>%
      filter(date >= input$dates[1] & date <= input$dates[2]) %>%
      select(-pop)
  })
  
  output$table <- DT::renderDataTable({
    d()
  })
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      glue("covid19_data_{input$dates[1]}_to_{input$dates[2]}.txt")
    },
    content = function(file) {
      write_tsv(d(), file)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = glue("covid19_caseplot_{input$dates[1]}_to_{input$dates[2]}.pdf"),
    content  = function(file) {
      pdf(file, width = 7, height = 5)
      print(plot_input())
      dev.off()
    }
  )
  
  output$downloadPlot_1 <- downloadHandler(
    filename = glue("covid19_recoveredplot_{input$dates[1]}_to_{input$dates[2]}.pdf"),
    content  = function(file) {
      pdf(file, width = 7, height = 5)
      print(plot_1_input())
      dev.off()
    }
  )
  
  output$downloadPlot_2 <- downloadHandler(
    filename = glue("covid19_deathplot_{input$dates[1]}_to_{input$dates[2]}.pdf"),
    content  = function(file) {
      pdf(file, width = 7, height = 5)
      print(plot_2_input())
      dev.off()
    }
  )
  
  output$downloadRate_plot <- downloadHandler(
    filename = glue("covid19_rateplot_{input$dates[2]}.pdf"),
    content  = function(file) {
      pdf(file, width = 7, height = 5)
      print(rate_plot_input())
      dev.off()
    }
  )
  
  output$downloadRate_trend_plot <- downloadHandler(
    filename = glue("covid19_ratetrendplot_{input$dates[1]}_to_{input$dates[2]}.pdf"),
    content  = function(file) {
      pdf(file, width = 7, height = 5)
      print(rate_trend_plot_input())
      dev.off()
    }
  )


}

shinyApp(ui, server)

