# HIV-Health-Poverty-Africa
 -- HIV, Poverty, and Mortality Trends in Africa (2000–2023)

This project explores the intersection of HIV prevalence, poverty, and child mortality trends across Africa — with a special focus on Sub-Saharan and East African countries — using open datasets from the **World Health Organization (WHO)**, **World Bank**, and the **UN Inter-agency Group for Child Mortality Estimation (UN IGME)**.

---

## Project Context

HIV remains a significant public health issue in Sub-Saharan Africa. Beyond the health implications, HIV intersects with socio-economic challenges such as poverty, education, and access to clean water and sanitation. By leveraging global datasets, this project seeks to uncover patterns, inform policy, and contribute to data-driven healthcare and development efforts.

---

### Part 1: HIV and Poverty Analysis

- Visualize HIV case trends in the countries contributing to **75% of the global HIV burden** (2000–2023)
- Create regional trend visualizations by **WHO regions**
- Merge HIV data with **World Bank Multidimensional Poverty data** to:
  - Explore the relationship between HIV prevalence and poverty
  - Identify key contributing factors (e.g. income, sanitation, education)
  - Account for **random effects** like country and year using mixed models
- Provide a written summary of insights and correlations

### Part 2: Child Mortality Trends in East Africa

- Filter and visualize data for **8 East African Community (EAC)** countries:
  i.e.: Burundi, DRC, Kenya, Rwanda, Somalia, South Sudan, Tanzania, Uganda
    
- Map the **latest under-five and neonatal mortality rates** using GADM shapefiles
- Plot historical **average trends** in both indicators across EAC countries
- Identify countries with the **highest current mortality rates**

---

## Data Sources

- **HIV Data:** WHO Global Health Observatory (2000–2023)
- **Poverty Indicators:** World Bank Multidimensional Poverty Index
- **Child Mortality:** UN IGME (via UNICEF Data)
- **Shapefiles:** [GADM.org](https://www.gadm.org/)

---

## Tools & Libraries

- **R**: `tidyverse`, `ggplot2`, `dplyr`, `sf`, `lme4`, `ggthemes`
- **Data Viz**: Loess smoothing, facet plots, thematic maps
- **Statistical Modeling**: Mixed-effects regression to explore random effects

---

## Sample Outputs

- Trend plots showing regional and global HIV burden
- Choropleth maps of child mortality across East Africa
- Scatter plots & models analyzing HIV–poverty correlations

---

## Key Findings (Summary)

> While HIV prevalence has declined in some countries, it remains high in areas with severe multidimensional poverty. Regions with limited access to clean water, low educational attainment, and poor health infrastructure tend to exhibit both high HIV prevalence and child mortality. Mixed-effect models highlight that sanitation and education are key correlates. Countries like **South Sudan** and **Somalia** show consistently high under-five and neonatal mortality rates, while **Rwanda** and **Uganda** show marked improvements over time.

---

