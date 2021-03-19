library(data.table)
library(tidyverse)
library(zoo)
library(scales)
library(glue)
library(showtext)
font_add_google("Noto Serif", "Noto Serif")
font_add_google("Lato", "Lato")
showtext_auto()

quick_plot_dt <- function(state, avg_date = "2020-07-01", accent = "#e00000") {
  d <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
  
  drop_cols <- c("UID", "iso2", "iso3", "FIPS", "code3", "Admin2", "Lat", "Long_", "Combined_Key", "Country_Region")
  d         <- d[, !drop_cols, with = FALSE][Province_State == state]
  
  dates <- names(colSums(d[, -1]))
  d     <- as.data.table(colSums(d[, -1]))
  setnames(d, "V1", "count")
  d     <- d[, date := as.Date(dates, "%m/%e/%y")][order(date)]
  d[, daily := count - lag(count)]
  d[, day7 := rollmean(daily, 7, fill = NA, align = "right")]
  
  end_dt <- data.table(
    end_date = d[!is.na(day7)][date == max(date, na.rm = TRUE)]$date,
    end_val  = d[!is.na(day7)][date == max(date, na.rm = TRUE)]$day7
  )
  
  na.omit(d)[daily >= 0 & day7 >= 0] %>%
    ggplot() +
    geom_bar(aes(x = date, y = daily), stat = "identity", fill = accent, alpha = 0.4) +
    geom_line(aes(x = date, y = day7), color = accent, size = 1, inherit.aes = FALSE) +
    geom_point(data = end_dt, aes(x = end_date, y = end_val), color = accent, size = 2) +
    
    annotate(geom = "segment", x = as.Date(avg_date), y = d[date == avg_date]$day7 + 1000,
             xend = as.Date(avg_date), yend = d[date == avg_date]$day7) +
    geom_text(aes(x = as.Date(avg_date),
                  y = d[date == avg_date]$day7 + 1250),
              family = "Lato",
              label = "7-day\naverage",
              vjust = 0, size = 3) +
    
    annotate(geom = "segment", x = d[daily == max(daily, na.rm = T)]$date - 20, y = d[daily == max(daily, na.rm = T)]$daily * 0.8,
             xend = d[daily == max(daily, na.rm = T)]$date, yend = d[daily == max(daily, na.rm = T)]$daily * 0.8) +
    geom_text(aes(x = d[daily == max(daily, na.rm = T)]$date - 25,
                  y = d[daily == max(daily, na.rm = T)]$daily * 0.8),
              family = "Lato",
              label = "New\ncases",
              hjust = 1, size = 3) +
    
    scale_x_date() +
    scale_y_continuous(labels = comma) +
    labs(
      title    = glue("{state} Coronavirus Map\nand Case Count"),
      subtitle = glue("Updated {format(max(d$date), '%B %e, %Y')}"),
      y        = "new cases"
    ) +
    theme_minimal() +
    theme(
      text               = element_text(family = "Lato"),
      plot.title         = element_text(face = "bold", hjust = 0.5, family = "Noto Serif", size = 24),
      plot.subtitle      = element_text(hjust = 0.5, size = 10, color = accent),
      axis.title.x       = element_blank(),
      axis.title.y       = element_blank(),
      axis.text.x        = element_text(hjust = 1),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

quick_plot_dt(state = "Michigan")