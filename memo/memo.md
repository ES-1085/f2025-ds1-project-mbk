Project memo
================
Team MBK

This document should contain a detailed account of the data clean up for
your data and the design choices you are making for your plots. For
instance you will want to document choices you’ve made that were
intentional for your graphic, e.g. color you’ve chosen for the plot.
Think of this document as a code script someone can follow to reproduce
the data cleaning steps and graphics in your handout.

``` r
library(tidyverse)
library(broom)
library(readr)
library(scales)
```

## Data Clean Up Steps for Overall Data

### Step 1: Load and Pivot Individual Datasets to Long Format

We load five separate CSV files from Gapminder and pivot each from wide
format (years as columns) to long format (one row per country-year).
This structure is necessary for merging datasets and performing
analysis.

``` r
homicide <- read_csv("../data/murder_total_deaths.csv")
```

    ## Rows: 193 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): geo, name
    ## dbl (32): 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
life <- read_csv("../data/lex.csv")
```

    ## Rows: 194 Columns: 303
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (2): geo, name
    ## dbl (301): 1800, 1801, 1802, 1803, 1804, 1805, 1806, 1807, 1808, 1809, 1810,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
gdp_per_capita <- read_csv("../data/gdp_pcap.csv")
```

    ## Rows: 193 Columns: 303
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (2): geo, name
    ## dbl (301): 1800, 1801, 1802, 1803, 1804, 1805, 1806, 1807, 1808, 1809, 1810,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
babies_per_woman <- read_csv("../data/children_per_woman_total_fertility.csv")
```

    ## Rows: 195 Columns: 303
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (2): geo, name
    ## dbl (301): 1800, 1801, 1802, 1803, 1804, 1805, 1806, 1807, 1808, 1809, 1810,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
population <- read_csv("../data/pop.csv")
```

    ## Rows: 195 Columns: 303
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (2): geo, name
    ## dbl (301): 1800, 1801, 1802, 1803, 1804, 1805, 1806, 1807, 1808, 1809, 1810,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
homicide_long <- homicide %>%
  pivot_longer(
    cols = -c(geo, name),
    names_to  = "year",
    values_to = "homicide_rate"
  )

life_long <- life %>%
  pivot_longer(
    cols = -c(geo, name),
    names_to  = "year",
    values_to = "lifeExp"
  ) 

gdp_long <- gdp_per_capita %>%
  pivot_longer(
    cols = -c(geo, name),
    names_to  = "year",
    values_to = "gdpPercap"
  ) 

babies_per_woman_long <- babies_per_woman %>%
  pivot_longer(
    cols = -c(geo, name),
    names_to  = "year",
    values_to = "babies_per_woman"
  ) 

pop_long <- population %>%
  pivot_longer(
    cols = -c(geo, name),
    names_to  = "year",
    values_to = "pop"
  )
```

### Step 2: Merge Datasets and Filter Years

We merge all five datasets using full joins to preserve all country-year
observations, then filter to focus on 1990-2021. We also calculate
growth rates for GDP and population.

``` r
combined_long <- homicide_long %>%
  full_join(life_long, by = c("geo", "name", "year")) %>%
  full_join(gdp_long, by = c("geo", "name", "year")) %>%
  full_join(babies_per_woman_long, by = c("geo", "name", "year")) %>%
  full_join(pop_long, by = c("geo", "name", "year"))

combined_long <- combined_long %>%
  filter(year >= 1990 & year <= 2021)

combined_long <- combined_long %>%
  mutate(year = as.numeric(year)) %>%
  group_by(geo) %>% 
  mutate(
    gdp_growth_percent = (gdpPercap - lag(gdpPercap)) / lag(gdpPercap) * 100,
    pop_growth_percent = (pop - lag(pop)) / lag(pop) * 100
  ) %>%
  ungroup()
```

## Plots

### ggsave example for saving plots

``` r
p1 <- starwars |>
  filter(mass < 1000, 
         species %in% c("Human", "Cerean", "Pau'an", "Droid", "Gungan")) |>
  ggplot() +
  geom_point(aes(x = mass, 
                 y = height, 
                 color = species)) +
  labs(x = "Weight (kg)", 
       y = "Height (m)",
       color = "Species",
       title = "Weight and Height of Select Starwars Species",
       caption = paste("This data comes from the starwars api: https://swapi.py43.com"))


ggsave("example-starwars.png", width = 4, height = 4)

ggsave("example-starwars-wide.png", width = 6, height = 4)
```

### Plot 1: GDP per Capita vs. Life Expectancy

This plot examines the relationship between economic prosperity (GDP per
capita) and health outcomes (life expectancy).

Design choices: - Log scale on x-axis to handle the wide range of GDP
values and reveal patterns across all income levels - Steelblue color
with alpha = 0.4 transparency to show density where points overlap -
LOESS smoothing curve in dark red to highlight the overall positive
relationship - Dollar formatting on x-axis for readability - Minimal
theme for clean, professional appearance

#### Data cleanup steps specific to plot 1

``` r
plot1_data <- combined_long %>%
  filter(!is.na(gdpPercap), !is.na(lifeExp))
```

#### Final Plot 1

``` r
p1 <- plot1_data %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 1.5) +
  geom_smooth(method = "loess", color = "darkred", se = TRUE, linewidth = 1.2) +
  scale_x_log10(labels = dollar_format()) +
  labs(
    title = "GDP per Capita and Life Expectancy",
    subtitle = "All countries, 1990-2021",
    x = "GDP per Capita (log scale, USD)",
    y = "Life Expectancy (years)",
    caption = "Data source: Gapminder (World Bank)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )
p1
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](memo_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggsave("plot1_gdp_life_expectancy.png", plot = p1, width = 7, height = 5)
```

    ## `geom_smooth()` using formula = 'y ~ x'

### Plot 2: \_\_\_\_\_\_\_\_\_

### Plot 3: \_\_\_\_\_\_\_\_\_\_\_

Add more plot sections as needed. Each project should have at least 3
plots, but talk to me if you have fewer than 3.

### Plot 4: \_\_\_\_\_\_\_\_\_\_\_
