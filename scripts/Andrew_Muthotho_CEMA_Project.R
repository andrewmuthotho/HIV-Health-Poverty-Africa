---
  
  title: "2025 Data Science Task"
  author: "Andrew Muthotho"

---

# Install packages  
install.packages("stringi")
install.packages("janitor")
install.packages("sf")

# Check to see if packages are installed 
"stringi" %in% installed.packages()

   
# Load necessary libraries
library(tidyverse)  # For data wrangling and visualization
library(readxl)     # For reading Excel files
library(sf)         # For spatial data and shapefiles
library(lme4)       # For mixed-effects models
library(ggplot2)    # For plotting
library(dplyr)      # Data manipulation
library(readr)      # Read csvs properly
library(janitor)    # For cleaning messy headers
library(scales)
library(RColorBrewer)

# # TASK 1A - Plot for Top 75 Countries Burdend Worldwide

# HIV dataset
hiv_data <- read_csv("../data/HIV data 2000-2023.csv")
  
# Multidimensional Poverty dataset (Excel)
multidimensional_poverty <- read_excel("../data/multidimensional_poverty.xlsx")

# Mortality Dataset
mortality_data <- read.csv("../data/dataset_datascience.csv")

# Sum HIV cases per country
hiv_data_clean <- hiv_data %>%
  select(Location, Period, Value) %>%
  mutate(value_clean = parse_number(Value, locale = locale(grouping_mark = " ")))%>%
  filter(!is.na(value_clean))

hiv_country_total <- hiv_data_clean %>%
  group_by(Location) %>%
  summarise(total_cases = sum(value_clean, na.rm = TRUE)) %>%
  arrange(desc(total_cases))

# Calculate cumulative percentage contribution
hiv_country_total <- hiv_country_total %>%
  mutate(cum_percent = cumsum(total_cases) / sum(total_cases) * 100)

# Find countries that together make up 75% of total HIV burden
top_75_countries <- hiv_country_total %>%
  filter(cum_percent <= 75) %>%
  pull(Location)

# Filter original data to only these countries
hiv_top75 <- hiv_data_clean %>%
  filter(Location %in% top_75_countries)

# # Creating a distinct color difference for SA and Thailand

# Subset for South Africa and Thailand
sa_thai <- hiv_top75 %>% 
  filter(Location %in% c("South Africa", "Thailand"))

# Get default ggplot color palette for remaining countries
default_colors <- hue_pal()(length(unique(hiv_top75$Location)))

# Create a named color vector that overrides only SA and Thailand
custom_colors <- setNames(default_colors, sort(unique(hiv_top75$Location)))
custom_colors["South Africa"] <- "#E41A1C"  # red
custom_colors["Thailand"] <- "#377EB8"      # blue

# Plot
ggplot(hiv_top75, aes(x = Period, y = value_clean, group = Location)) +
  geom_line(aes(color = Location), linewidth = 1) +
  
  # Emphasize South Africa and Thailand with slightly thicker lines
  geom_line(
    data = sa_thai, 
    aes(x = Period, y = value_clean, color = Location), 
    linewidth = 1.5
  ) +
  
  # Assign colors (only override SA and Thailand)
  scale_color_manual(values = custom_colors) +
  
  # Axis formatting
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    breaks = seq(0, 8e6, by = 2e6)
  ) +
  scale_x_continuous(breaks = seq(2000, 2023, by = 5)) +
  
  labs(
    title = "HIV Cases in Top 75% Burden Countries (2000–2023)",
    x = "Year",
    y = "People Living with HIV (Millions)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 5)
  )


# # TASK 1B - Plot for Top Countries Per Region

# Create numerical data 
hiv_data <- hiv_data %>%
  mutate(value_clean = parse_number(Value, locale = locale(grouping_mark = " ")))%>%
  filter(!is.na(value_clean))

# Group by WHO region and country, then sum HIV cases
hiv_region_country_total <- hiv_data %>%
  group_by(ParentLocationCode, Location) %>%
  summarise(total_cases = sum(value_clean, na.rm = TRUE)) %>%
  arrange(ParentLocationCode, desc(total_cases))

# For each WHO region, find countries contributing to 75%
top_countries_per_region <- hiv_region_country_total %>%
  group_by(ParentLocationCode) %>%
  mutate(cum_percent = cumsum(total_cases) / sum(total_cases) * 100) %>%
  filter(cum_percent <= 75)

# Extract the list of countries
top_region_countries <- top_countries_per_region %>%
  select(ParentLocationCode, Location)

# Filter original data
hiv_region_top75 <- hiv_data %>%
  semi_join(top_region_countries, by = c("ParentLocationCode", "Location"))

hiv_region_grouped <- hiv_region_top75 %>%
  group_by(ParentLocationCode, Period) %>%   # Group by region and year
  summarise(total_HIV = sum(value_clean, na.rm = TRUE)) %>%
  ungroup()

# Creating the Plot

ggplot(hiv_region_grouped, aes(x = Period, y = total_HIV, color = ParentLocationCode)) +
  geom_line(linewidth = 1) +  # Keeps separate lines per country
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    breaks = seq(0, 20e6, by = 2e6)
  ) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "HIV Cases Grouped by WHO Region (2000–2023)",
    x = "Year",
    y = "People Living with HIV (Millions)",
    color = "WHO Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  )

# # TASK 2 - Correlation Analysis

# Skip the first row and use the second row as column names
multidimensional_poverty <- read_excel(
  "../data/multidimensional_poverty.xlsx",
  skip = 1
)

# # Column Cleaning for the Multidimensional Poverty Data

# 1. Save subheaders (these are in row 1 of ...11 to ...15)
poverty_subheaders <- multidimensional_poverty[1, 10:15] %>% unlist(use.names = FALSE)

# 2. Drop that first row from the main dataset
multidimensional_poverty_clean <- multidimensional_poverty[-1, ]

# 3. Rename ...11 to ...15 with meaningful names from the subheaders
colnames(multidimensional_poverty_clean)[10:15] <- poverty_subheaders

# 4. Convert necessary columns to numeric
multidimensional_poverty_clean <- multidimensional_poverty_clean %>%
  mutate(across(c(`Monetary (%)`,
                  `Educational attainment (%)`,
                  `Educational enrollment (%)`,
                  `Electricity (%)`,
                  `Sanitation (%)`,
                  `Drinking water (%)`,
                  `Multidimensional poverty headcount ratio (%)`),
                ~ as.numeric(as.character(.))))

# Rename Columns for Merge
multidimensional_poverty_clean <- multidimensional_poverty_clean %>%
  select_all() %>%
  rename(Country = Economy, Year = 'Reporting year')

# Merge HIV and Multidimensional Poverty Data

# Select relevant columns for merge
hiv_for_merge <- hiv_data %>%
  select(Location, Period, value_clean) %>%
  rename(Country = Location, Year = Period, HIV_Cases = value_clean)

# Join on Country and Year
merged_data <- hiv_for_merge %>%
  left_join(multidimensional_poverty_clean, by = c("Country", "Year"))

# Outcome: People Living with HIV (Value)
# Fixed Effects: Poverty Headcount Ratio, Electricity Access, Sanitation, etc
# Random Effects: Country, Year

merged_data$Country <- as.factor(merged_data$Country)
merged_data$Year <- as.factor(merged_data$Year)

# Fit model
Poverty_HIV_model <- lmer(HIV_Cases ~ `Multidimensional poverty headcount ratio (%)` +
                `Educational attainment (%)` +
                `Sanitation (%)`  +
                  
                `Drinking water (%)` +
                (1 | Year),
              data = merged_data)
summary(Poverty_HIV_model)


# Checking Colinearity of Variables
install.packages("car")
library(car)
vif(Poverty_HIV_model)


# # TASK 3 - Filter for East Africa and Plot


eac_countries <- c("Kenya", "Uganda", "Tanzania", "Rwanda", "Burundi", "South Sudan", "Democratic Republic of the Congo", "Somalia")

# Filter Mortality Data for EAC countries
eac_mortality <- mortality_data %>%
  filter(Geographic.area%in% eac_countries)

# Filter Under-5 Mortality
under5_eac <- eac_mortality %>%
  filter(Indicator == "Under-five mortality rate")

# Filter Neonatal Mortality
neonatal_eac <- eac_mortality %>%
  filter(Indicator == "Neonatal mortality rate")

# Latest Year for each indicator
latest_year_under5 <- max(under5_eac$Series.Year, na.rm = TRUE)
latest_year_neonatal <- max(neonatal_eac$Series.Year, na.rm = TRUE)

# Filter latest year for each
under5_latest <- under5_eac %>%
  filter(Series.Year == latest_year_under5)

neonatal_latest <- neonatal_eac %>%
  filter(Series.Year == latest_year_neonatal)

# # TASK 4 - Create Maps

# Load libraries
library(sf)  # for shapefiles

# Read shapefile
africa_map <- st_read("../data/shapefiles/Africa")  # replace with your downloaded file

# Simplify the Shapefile
install.packages('rmapshaper')
library(rmapshaper)
africa_map_simplified <- ms_simplify(africa_map, keep = 0.05, keep_shapes = TRUE)


# Merge map data with under5 mortality
under5_eac_map_data <- africa_map_simplified %>%
  left_join(under5_latest, by = c("ADM0_NAME" = "Geographic.area"))

# Merge map data with neonatal mortality
neonatal_map_data <- africa_map_simplified %>%
  left_join(neonatal_latest, by = c("ADM0_NAME" = "Geographic.area"))

# Plot under5 mortality
ggplot(data = under5_eac_map_data) +
  geom_sf(aes(fill = Observation.Value)) +
  scale_fill_viridis_c() +
  labs(title = "Under-5 Mortality Rate in East African Community (Latest Year)",
       fill = "Deaths per 1000 live births") +
  theme_minimal()

# Plot neonatal mortality
ggplot(data = neonatal_map_data) +
  geom_sf(aes(fill = Observation.Value)) +
  scale_fill_viridis_c() +
  labs(title = "Neonatal Mortality Rate in East African Community (Latest Year)",
       fill = "Deaths per 1000 live births") +
  theme_minimal()


# # TASK 5 - Create Trendlines 

under5_eac$Series.Year <- as.numeric(str_extract(under5_eac$Series.Year, "\\d{4}"))
neonatal_eac$Series.Year <- as.numeric(str_extract(neonatal_eac$Series.Year, "\\d{4}"))

# Plot Under-5 trend
ggplot(under5_eac, aes(x = Series.Year, y = Observation.Value, group = Geographic.area, color = Geographic.area)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_line(alpha = 0.5 ) +
  geom_smooth(aes(group = 1), method = "loess", color = "black", linewidth = 1) +
  scale_x_continuous(breaks = seq(1960, 2030, by = 10))+
  labs(title = "Under-5 Mortality Trends - EAC Countries",
       x = "Year", y = "Deaths per 1000 live births",
       color = "Geographic.area") +
  theme_minimal(base_size = 8)+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    legend.position = "bottom",
    legend.text = element_text(size = 7)
  )

# Plot Neonatal trend
ggplot(neonatal_eac, aes(x = Series.Year, y = Observation.Value, group = Geographic.area, color = Geographic.area)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_line(alpha = 0.5 ) +
  geom_smooth(aes(group = 1), method = "loess", color = "black", linewidth = 1) +
  scale_x_continuous(breaks = seq(1960, 2030, by = 10))+
  labs(title = "Neonatal Mortality Trends - EAC Countries",
       x = "Year", y = "Deaths per 1000 live births",
       color = "Geographic.area") +
  theme_minimal(base_size = 8)+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    legend.position = "bottom",
    legend.text = element_text(size = 7)
)



