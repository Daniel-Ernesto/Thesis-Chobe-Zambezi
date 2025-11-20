############## NDVI time series visualization (lineplot and boxplot) ###############
### A. Install packages and download libraries
#install.packages(c("tidyverse", "ggplot2", "corrplot", "car",
#                    "GGally", "sf","dunn.test", "FSA", "read_csv", "reshape2",
#                    "cluster", "ggpubr"))

library(tidyverse)
library(lubridate)
library(readr)
library(writexl)
library(ggtext)
library(viridisLite)
library(withr)


### B. Load the MODIS NDVI raw timeseries extracted from GEE
ndvi <- read_csv("C:/Users/danie/OneDrive/Desktop/MASTER/Temporal/Sites/Datasets/NDVI_8d.csv") %>%
  mutate(date = ymd(date))  # Mutate the date column in a modifiable format

site_names <- c("VSB301", "VSB302", "VSB303", "VSB304", "VSB305", "VSB306", # Create the site column
                "VSN301", "VSN302", "VSN303", "VSN304", "VSN305", "VSN306",
                "VSN307", "VSN309", "VSN310")

ndvi <- ndvi %>%                         # Assign correctly the site and date properties to each row
  group_by(date) %>%
  mutate(site_id = row_number()) %>%
  ungroup() %>%
  mutate(site_name = site_names[site_id],
         year = year(date)) %>%
  filter(year %in% 2020:2024, !is.na(mean), !is.na(site_name))

mopane_sites   <- c("VSB302","VSB303","VSB304","VSN301","VSN302",    # Define the species forests site
                    "VSN306","VSN307","VSN309","VSN310")
baikiaea_sites <- c("VSB301","VSB305","VSB306","VSN303","VSN304","VSN305")

species_map <- c(setNames(rep("C. mopane",   length(mopane_sites)),   mopane_sites),     # Change the names 
                 setNames(rep("B. plurijuga", length(baikiaea_sites)), baikiaea_sites))

ndvi <- ndvi %>%
  mutate(
    Species = factor(species_map[site_name], levels = c("C. mopane","B. plurijuga")),  # Apply the new names
    Country = if_else(startsWith(site_name, "VSB"), "Botswana", "Namibia")
  ) %>%
  filter(!is.na(Species))

### C. Create the Box plot (2020-2024)
label_colors <- c(       # Define the site colors for the graphic
  "VSB302" = "#439e1b","VSB303" = "#439e1b","VSB304" = "#439e1b",
  "VSN301" = "#439e1b","VSN302" = "#439e1b","VSN306" = "#439e1b",
  "VSN307" = "#439e1b","VSN309" = "#439e1b","VSN310" = "#439e1b",
  "VSB301" = "#e3e218","VSB305" = "#e3e218","VSB306" = "#e3e218",
  "VSN303" = "#e3e218","VSN304" = "#e3e218","VSN305" = "#e3e218"
)

label_fun <- function(x) {  # Helper function to colorate the x labels
  sapply(x, function(s) {
    col <- label_colors[[s]]
    paste0("<span style='color:", col, "'>", s, "</span>")
  })
}

fill_vals <- c("C. mopane" = "#439e1b", "B. plurijuga" = "#e3e218")  # To colorate the Fill legends

ndvi_2020_24 <- ndvi %>%      # Filter only the 2020-2024 dates
  filter(year %in% 2020:2024, !is.na(mean), !is.na(site_name), !is.na(Species))

ndvi_season <- ndvi_2020_24 %>%  # Filter to separate the datas for the 2 seasons
  mutate(
    month_day = format(date, "%m-%d"),
    Season = case_when(
      month_day >= "12-29" | month_day <= "03-29" ~ "Rainy", # The dates for the rainy season
      month_day >= "08-03" & month_day <= "11-02" ~ "Dry",   # Dates for the Dry season
      TRUE ~ NA_character_
    ),
    Season = factor(Season, levels = c("Rainy", "Dry"))   # Save season as a factor
  ) %>%
  filter(!is.na(Season))     # All the other NDVI values of other dates are excluded

season_fill <- c(     # Colors for the seasons
  "Rainy" = "#56B4E9",  
  "Dry"   = "#E69F00"   
)

species_color <- c(   # Colors for the borders of the box
  "C. mopane"    = "#439e1b",  
  "B. plurijuga" = "#e3e218"   
)

p <- ggplot(     # Final boxplot
  ndvi_season,
  aes(x = site_name, y = mean, fill = Season, color = Species)
) +
  geom_boxplot(
    position = position_dodge(width = 0.8),   # To make the pair of box at each X site
    outlier.alpha = 0.5
  ) +
  scale_fill_manual(
    values = season_fill,
    name = "Season"
  ) +
  scale_color_manual(
    values = species_color,
    name = "Species",
    labels = c(
      expression(italic("C. mopane")),
      expression(italic("B. plurijuga"))
    )
  ) +
  scale_x_discrete(labels = label_fun) +   # The labels must be colored based on the species
  labs(
    title = "NDVI distribution per site separated by season (2020-2024)",
    x = "Sites", y = "NDVI"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1),
    legend.position = "right"
  )

print(p)

stats_per_site_season <- ndvi_season %>%                # To get the specifics values of each box
  group_by(site_name, Species, Country, Season) %>%
  summarise(
    min    = boxplot.stats(mean)$stats[1],
    q1     = boxplot.stats(mean)$stats[2],
    median = boxplot.stats(mean)$stats[3],
    q3     = boxplot.stats(mean)$stats[4],
    max    = boxplot.stats(mean)$stats[5],
    n      = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(IQR = q3 - q1) %>%
  arrange(site_name, Season)

out_dir <- "C:/Users/danie/OneDrive/Desktop/MASTER/Temporal/Sites/Plots/NDVI_Box_byYear"  # To save the statistics in a final excel file
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
xlsx_path <- file.path(out_dir, "NDVI_stats_per_site_2020_2024_season.xlsx")
write_xlsx(list("stats_per_site" = stats_per_site_season), path = xlsx_path)

### D. Line plot (2020-2024)
ndvi_5y <- ndvi %>%                      # Extract from the table only the data on the exact range
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2020-01-01"),
         date <= as.Date("2024-12-31"),
         !is.na(mean),
         !is.na(site_name))

sites <- sort(unique(ndvi_5y$site_name))    # Define a color palette "Turbo" composed as many colors as the sites
pal   <- viridisLite::turbo(length(sites))
names(pal) <- sites 

withr::with_locale(c("LC_TIME" = "C"), {   # Create the line plot forcing the month to be in englis (e.g. Jan 2020)
  p <- ggplot(ndvi_5y, aes(x = date, y = mean, color = site_name, group = site_name)) +
    geom_line(size = 0.6, alpha = 0.9) +
    scale_color_manual(values = pal, name = "Site") +
    scale_x_date(limits = as.Date(c("2020-01-01","2024-12-31")),
                 date_breaks = "3 months",
                 date_labels = "%b %Y") +  
    labs(title = "NDVI time series for each site (2020–2024)", x = "Date", y = "NDVI") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
})
