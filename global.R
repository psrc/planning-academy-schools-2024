# Packages ----------------------------------------------------------------

# Packages for Interactive Web application
library(shiny)
library(shinyBS)
library(shinydashboard)
library(bs4Dash)
library(shinycssloaders)
library(bslib)

# Packages for Data Cleaning/Processing
library(tidyverse)

# Packages for Chart Creation
library(psrcplot)
library(echarts4r)

# Packages for Map Creation
library(sf)
library(leaflet)

# Packages for Table Creation
library(DT)
library(scales)

# Package for Excel Data Creation
library(openxlsx)

# Package for generating HTML
library(htmltools)

# Run Modules Files ---------------------------------------------------------------------------

module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)
source("functions.R")

# Page Information --------------------------------------------------------

page_text <- read_csv("data/page_text.csv", show_col_types = FALSE)
place_information <- read_csv("data/place_information.csv", show_col_types = FALSE)

# Inputs ---------------------------------------------------------------

wgs84 <- 4326
load_clr <- "#91268F"
latest_yr <- "2023"

rgc_title <- "Regional Growth Center (4/23/2024)"
school_title <- "Planning Academy School 2024"

year_ord <- c("2022", "2021", "2017", "2012")
summary_ord <- c("Land Area (acres)", "Year", "Category",
                 "Population", "Housing Units", "Total Employment",
                 "Activity Units per Acre", "Jobs per Resident")

# Data via RDS files ------------------------------------------------------

pop_hh_hu_data <- readRDS("data/population-housing.rds") |> mutate(year = factor(year, levels=year_ord))
age_data <- readRDS("data/population_by_age.rds") |> mutate(year = factor(year, levels=year_ord))
race_data <- readRDS("data/population_by_race.rds") |> mutate(year = factor(year, levels=year_ord))
income_data <- readRDS("data/households_by_income.rds") |> mutate(year = factor(year, levels=year_ord))
education_data <- readRDS("data/educational_attainment.rds") |> mutate(year = factor(year, levels=year_ord))
tenure_data <- readRDS("data/households_by_tenure.rds") |> mutate(year = factor(year, levels=year_ord))
type_data <- readRDS("data/housing_units_by_type.rds") |> mutate(year = factor(year, levels=year_ord))
burden_data <- readRDS("data/cost_burden.rds") |> mutate(year = factor(year, levels=year_ord))
mode_data <- readRDS("data/mode_to_work.rds") |> mutate(year = factor(year, levels=year_ord))
vehicles_data <- readRDS("data/households_by_vehicles.rds") |> mutate(year = factor(year, levels=year_ord))
jobs_data <- readRDS("data/jobs_data.rds") |> mutate(year = factor(year, levels=year_ord)) |> drop_na()
stops_data <- readRDS("data/transit_stop_data.rds")

rgc_shape <- readRDS("data/rgc_shape.rds") |> st_transform(wgs84) |> rename(geometry="Shape") |> mutate(geography_type = rgc_title)
school_shape <- readRDS("data/school_shape.rds") |> st_transform(wgs84) |> mutate(geography_type = school_title)
place_shape <- bind_rows(rgc_shape, school_shape)
stop_shape <- readRDS("data/transit_stop_lyr.rds")

# Place Summary Data ------------------------------------------------------
max_year <- max(as.integer(as.character(jobs_data$year)))

summary_data <- bind_rows(pop_hh_hu_data, jobs_data) |>
  mutate(year = as.integer(as.character(year))) |>
  filter(grouping %in% c("Population", "Housing Units", "Total") & year >= max_year & !(geography_type %in% c("County", "Region"))) |>
  mutate(grouping = str_replace_all(grouping, "Total", "Total Employment")) |>
  select("geography", "geography_type", "grouping", "estimate") |>
  pivot_wider(names_from = c("grouping"), values_from = c("estimate")) |>
  as_tibble()

place_summary <- place_shape |> 
  st_drop_geometry() |> 
  group_by(name, category, geography_type) |>
  summarise(acres = sum(acres)) |>
  as_tibble() |>
  mutate(acres = round(acres, 0)) |> 
  rename(geography = "name")

place_year<- place_information |> 
  filter(type_of_place != "Manufacturing Industrial Center") |> 
  select(geography = "name", year = "designation_year")

summary_data <- left_join(summary_data, place_summary, by=c("geography", "geography_type")) |>
  drop_na() |>
  mutate(`Activity Units per Acre` = round((`Population` + `Total Employment`)/acres, 1)) |>
  mutate(`Jobs per Resident` = round((`Total Employment`)/(`Population`), 1))

summary_data <- left_join(summary_data, place_year, by=c("geography")) |>
  rename(`Land Area (acres)` = "acres", Year = "year", Category = "category") |>
  mutate(`Land Area (acres)` = format(`Land Area (acres)`, big.mark = ",")) |>
  mutate(`Year` = as.character(Year)) |>
  mutate(`Population` = format(`Population`, big.mark = ",")) |>
  mutate(`Housing Units` = format(`Housing Units`, big.mark = ",")) |>
  mutate(`Total Employment` = format(`Total Employment`, big.mark = ",")) |>
  mutate(`Activity Units per Acre` = as.character(`Activity Units per Acre`)) |>
  mutate(`Jobs per Resident` = as.character(`Jobs per Resident`))

summary_data <- summary_data |> 
  pivot_longer(!c(geography, geography_type), values_to = "estimate", names_to = "grouping") |> 
  as_tibble() |>
  mutate(grouping = factor(grouping, levels = summary_ord)) |>
  arrange(geography_type, geography, grouping)

rm(place_summary, place_year)


