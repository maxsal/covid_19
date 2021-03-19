library(tidyverse)
library(janitor)
library(scales)
library(zoo)
library(glue)
library(showtext)
font_add_google("Lato", "Lato")
font_add_google("Noto Serif", "Noto Serif")
showtext_auto()

quick_plot <- function(state, avg_date = "2020-07-01", accent = "#e00000") {
  d <- suppressMessages(vroom::vroom("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")) %>%
    dplyr::select(-c(UID, iso2, iso3, FIPS, code3, Admin2, Lat, Long_, Combined_Key, Country_Region)) %>%
    filter(Province_State == state)
  
  tmp <- colSums(d[, -1]) %>% as.data.frame()
  tmp$date <- rownames(tmp)
  d <- as_tibble(tmp) %>% dplyr::select(date, count = ".") %>%
    mutate(date = as.Date(date, "%m/%d/%y")) %>% 
    arrange() %>%
    mutate(daily = count - dplyr::lag(count)) %>%
    mutate(day7  = zoo::rollmean(daily, k = 7, fill = NA, align = "right"))
  
  high_lab_date <- d %>% filter(daily == max(daily, na.rm = T)) %>% pull(date)
  high_lab_val <-  d %>% filter(daily == max(daily, na.rm = T)) %>% pull(daily) * 0.8
  
  end_tib <- tibble(
    end_date = d %>% drop_na(day7) %>% filter(date == max(date, na.rm = T)) %>% pull(date),
    end_val  = d %>% drop_na(day7) %>% filter(date == max(date, na.rm = T)) %>% pull(day7)
  )
  
  d %>%
    drop_na() %>%
    filter(daily >= 0 & day7 >= 0) %>%
    ggplot() +
    geom_bar(aes(x = date, y = daily), stat = "identity", fill = accent, alpha = 0.4) +
    geom_line(aes(x = date, y = day7), color = accent, size = 1, inherit.aes = FALSE) +
    geom_point(data = end_tib, aes(x = end_date, y = end_val), color = accent, size = 2) +
    
    annotate(geom = "segment", x = as.Date(avg_date), y = d %>% filter(date == avg_date) %>% pull(day7) + 1000,
             xend = as.Date(avg_date), yend = d %>% filter(date == avg_date) %>% pull(day7)) +
    geom_text(aes(x = as.Date(avg_date),
                  y = d %>% filter(date == avg_date) %>% pull(day7) + 1250),
              family = "Lato",
              label = "7-day\naverage",
              vjust = 0, size = 3) +
    
    annotate(geom = "segment", x = high_lab_date - 20, y = high_lab_val,
             xend = high_lab_date, yend = high_lab_val) +
    geom_text(aes(x = high_lab_date - 25,
                  y = high_lab_val),
              family = "Lato",
              label = "New\ncases",
              hjust = 1, size = 3) +
    
    scale_x_date() +
    scale_y_continuous(labels = comma) +
    labs(
      title    = glue("{state} Coronavirus\nCase Count"),
      subtitle = glue("Updated {format(max(d$date), '%B %e, %Y')}"),
      y        = "new cases"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "Lato"),
      plot.title = element_text(face = "bold", hjust = 0.5, family = "Noto Serif", size = 24),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = accent),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(hjust = 1),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
}

quick_plot(state = "Michigan")