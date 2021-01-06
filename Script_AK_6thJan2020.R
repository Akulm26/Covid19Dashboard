setwd('D:\\Ayesha Khan\\COVID19')
# ---- Loading libraries ----
library("shiny")
library("shinydashboard")
library("tidyverse")
library("leaflet")
library("plotly")
library("DT")
library("fs")
library("wbstats")

source("utils.R", local = T)

downloadGithubData <- function() {
  download.file(
    url      = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip",
    destfile = "data/covid19_data.zip"
  )
  
  data_path <- "COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"
  unzip(
    zipfile   = "data/covid19_data.zip",
    files     = paste0(data_path, c("confirmed_global.csv", "deaths_global.csv", "recovered_global.csv", "confirmed_US.csv", "deaths_US.csv")),
    exdir     = "data",
    junkpaths = T
  )
}

updateData <- function() {
  # Download data from Johns Hopkins (https://github.com/CSSEGISandData/COVID-19) if the data is older than 0.5h
  if (!dir_exists("data")) {
    dir.create('data')
    downloadGithubData()
  } else if ((!file.exists("data/covid19_data.zip")) || (as.double(Sys.time() - file_info("data/covid19_data.zip")$change_time, units = "hours") > 0.5)) {
    downloadGithubData()
  }
}

# Update with start of app
updateData()


# TODO: Still throws a warning but works for now
data_confirmed    <- read_csv("data/time_series_covid19_confirmed_global.csv")
data_deceased     <- read_csv("data/time_series_covid19_deaths_global.csv")
data_recovered    <- read_csv("data/time_series_covid19_recovered_global.csv")
data_confirmed_us <- read_csv("data/time_series_covid19_confirmed_US.csv")
data_deceased_us  <- read_csv("data/time_series_covid19_deaths_US.csv")

# Get latest data
current_date <- as.Date(aq9, format = "%m/%d/%y")
changed_date <- file_info("data/covid19_data.zip")$change_time


# Get evolution data by country
data_confirmed_sub <- data_confirmed %>%
  pivot_longer(names_to = "date", cols = 5:ncol(data_confirmed)) %>%
  group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
  summarise("confirmed" = sum(value, na.rm = T))

data_recovered_sub <- data_recovered %>%
  pivot_longer(names_to = "date", cols = 5:ncol(data_recovered)) %>%
  group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
  summarise("recovered" = sum(value, na.rm = T))

data_deceased_sub <- data_deceased %>%
  pivot_longer(names_to = "date", cols = 5:ncol(data_deceased)) %>%
  group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
  summarise("deceased" = sum(value, na.rm = T))
