############## Area distribution and NDVI test and visualization ###############
### A. Install packages and download libraries
install.packages(c("tidyverse", "ggplot2", "corrplot", "car",
                   "GGally", "sf","dunn.test", "FSA", "read_csv", "reshape2",
                   "cluster", "ggpubr"))
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(janitor)
library(stringr)
library(forcats)
library(tidyr)
library(purrr)
library(scales)
library(ggtext)

### B. Class composition over the region
df <- read_excel("C:/Users/danie/OneDrive/Desktop/MASTER/Redazione/New_Temporal2/Step1/Region/LULC_areas_by_region_combined.xlsx")  # Load the data extracted from GEE
class_names <- c(          # Encode the number of the class with the precise name
  "1" = "Water",
  "2" = "Soil",
  "3" = "C. mopane",
  "5" = "Other Vegetation",
  "6" = "B. plurijuga",
  "7" = "Grass")
class_colors <- c(                    # Define the colors for each class
  "Water"   = "#366d9e",
  "Soil"             = "#e79046",
  "C. mopane"        = "#439e1b",
  "Other Vegetation" = "#ca5fc4",
  "B. plurijuga"     = "#e3e218",
  "Grass"            = "#daeaeb")

class_labels <- c(                  # Create specifici labels to make Mopane and Baikiaea in italic
  "Water"   = "Water",
  "Soil"             = "Soil",
  "C. mopane"        = expression(italic("C. mopane")),
  "Other Vegetation" = "Other Vegetation",
  "B. plurijuga"     = expression(italic("B. plurijuga")),
  "Grass"            = "Grass"
)

df <- df %>%           # Mutate the table to aggregate the variables of season and year
  mutate(
    season = ifelse(grepl("Rainy", image), "Rainy", "Dry"),
    year   = gsub("[^0-9]", "", image),
    combo  = paste(year, season, sep = " - ")
  )

df$class <- class_names[as.character(df$code)] # Apply the correct name to the classes based on the codes defined previously

df_perc <- df %>%               # Define the percentages for each class over the total surface for each season-year combo
  group_by(combo) %>%
  mutate(
    total_area = sum(area_km2, na.rm = TRUE),
    perc = area_km2 / total_area * 100
  ) %>%
  ungroup()

df_perc <- df_perc %>%      # Define the order on the X axe, before rainy then dry
  mutate(season = factor(season, levels = c("Rainy", "Dry")))

combo_levels <- df_perc %>%
  distinct(year, season, combo) %>%
  arrange(as.integer(year), season) %>%
  pull(combo)

df_perc$combo <- factor(df_perc$combo, levels = combo_levels)

ggplot(df_perc, aes(x = combo, y = area_km2, fill = class)) +  # Create the stacked barplot with the percentages 
  geom_col(width = 0.7) +
  geom_text(aes(label = ifelse(perc > 3, paste0(round(perc), "%"), "")),        # Only the ones superior to 3% will appear
            position = position_stack(vjust = 0.5), size = 2.5, color = "black") +
  scale_fill_manual(
    values = class_colors,
    labels = class_labels
  ) +
  scale_y_continuous(labels = scales::label_number(big.mark = "'")) +
  labs(
    title = "Class composition over the region",
    x = NULL, y = "Area (km²)", fill = "Class"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.minor = element_blank()
  )

### C. Class composition per site (Example for Rainy2021, same code for the rest of the seven remaining combos)
data <- read_excel("C:/Users/danie/OneDrive/Desktop/MASTER/Redazione/New_Temporal2/Step1/Masked/LULC_areas_by_site_combined.xlsx") # Load the data extracted from GEE

plot_data <- data %>%               # Filter only the Rainy2021 combo and calculate the percentage of each class over the total
  filter(image == "Rainy2021") %>%
  group_by(siteID) %>%
  mutate(
    total_area = sum(area_km2, na.rm = TRUE),
    perc = area_km2 / total_area * 100
  ) %>%
  ungroup()

class_order <- c("Baikiaea", "Mopane", "Grass", "OtherVegetation", "Soil", "Water") # Define an order of appearance of the class on the graphic

class_colors <- c(   
  "Water"            = "#366d9e",
  "Soil"             = "#e79046",
  "Mopane"           = "#439e1b",
  "OtherVegetation"  = "#ca5fc4",
  "Baikiaea"         = "#e3e218",
  "Grass"            = "#daeaeb"
)
class_labels <- c(
  "Water"            = "Water",
  "Soil"             = "Soil",
  "Mopane"           = expression(italic("C. mopane")),
  "OtherVegetation"  = "Other Vegetation",
  "Baikiaea"         = expression(italic("B. plurijuga")),
  "Grass"            = "Grass"
)

plot_data$class_name <- factor(plot_data$class_name, levels = class_order)   # Set Class Names as factor with the desired order

label_colors <- c(      # Define the colors for each forests species site
  "VSB302" = "#439e1b",
  "VSB303" = "#439e1b",
  "VSB304" = "#439e1b",
  "VSN301" = "#439e1b",
  "VSN302" = "#439e1b",
  "VSN306" = "#439e1b",
  "VSN307" = "#439e1b",
  "VSN309" = "#439e1b",
  "VSN310" = "#439e1b",
  "VSB301" = "#e3e218",
  "VSB305" = "#e3e218",
  "VSB306" = "#e3e218",
  "VSN303" = "#e3e218",
  "VSN304" = "#e3e218",
  "VSN305" = "#e3e218"
)
color_label <- function(site) { # Function to colorate the labels with the right color with Markdown
  col <- label_colors[[site]]
  glue::glue("<span style='color:{col}'>{site}</span>")
}

x_labels <- setNames(lapply(names(label_colors), color_label), names(label_colors)) # Recreate the colored labels

ggplot(plot_data, aes(x = siteID, y = area_km2, fill = class_name)) +   # Create the plot
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(perc > 3.5, paste0(round(perc), "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 2.5, color = "black") +
  scale_fill_manual(
    values = class_colors[class_order],
    labels = class_labels[class_order]
  ) +
  scale_y_continuous(labels = label_number(big.mark = "'")) +
  labs(
    title = "Class composition per site – Rainy2021",
    x = "Sites",
    y = "Area (km²)",
    fill = "Class"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = x_labels) +
  theme(
    axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1)
  )

### D. Changes in species area (Rainy2021-Rainy2024)
site_order <- c("VSB301","VSB302","VSB303","VSB304","VSB305","VSB306",  #Define the order of appearance of the sites on the x axis
                "VSN301","VSN302","VSN303","VSN304","VSN305","VSN306","VSN307","VSN309","VSN310")

transition_colors <- c(
  "0" = "grey",   # Not species-related
  "1" = "red",    # Species lost area
  "2" = "blue",   # Species stable area
  "3" = "green"   # Species gained area
)
type_labels <- c(
  "0" = "Not species-related",
  "1" = "Species lost area",
  "2" = "Species stable area",
  "3" = "Species gained area"
)

mop <- read_excel("C:/Users/danie/OneDrive/Desktop/MASTER/Redazione/New_Temporal2/Step3/Nuovo/mopane/Filtered_transitions_Mopane.xlsx")     # Load the data for mopane
bai <- read_excel("C:/Users/danie/OneDrive/Desktop/MASTER/Redazione/New_Temporal2/Step3/Nuovo/baikiaea/Filtered_transitions_Baikiaea.xlsx") # Load the data for Baikiaea

needed <- c("area_km2","siteID","species","transition_label","transition_type") # Create a vector of the columsn needed
stopifnot(all(needed %in% names(mop)), all(needed %in% names(bai)))             #filter the tables with the needed columns

df_all <- bind_rows(mop[, needed], bai[, needed]) %>%   # Unify the 2 datasets with the type labels and the correct species assigned to each site
  mutate(
    transition_type = factor(as.character(transition_type), levels = c("0","1","2","3")),
    siteID = factor(siteID, levels = site_order)
  )

grid_all <- df_all %>%           # Create the grid of data for separated based on each transition type 
  distinct(transition_label) %>%
  crossing(
    siteID = factor(site_order, levels = site_order),
    transition_type = factor(c("0","1","2","3"), levels = c("0","1","2","3"))
  )
df_completed <- grid_all %>%
  left_join(df_all, by = c("transition_label","siteID","transition_type")) %>%
  mutate(
    area_km2 = replace_na(area_km2, 0),
    species  = replace_na(species, "NA")
  )

df_plot <- df_completed %>%  # Aggregate for each transition X sites X types and calculate the percentages over the total surface for each type
  group_by(transition_label, siteID, transition_type) %>%
  summarise(area_km2 = sum(area_km2, na.rm = TRUE), .groups = "drop") %>%
  group_by(transition_label, siteID) %>%
  mutate(percent = 100 * area_km2 / sum(area_km2, na.rm = TRUE)) %>%
  ungroup()

transitions_order <- df_plot %>%   # Order the transition
  distinct(transition_label) %>%
  tidyr::extract(
    col = transition_label,
    into = c("s1_txt", "y1", "s2_txt", "y2"),
    regex = "^(Rainy|Dry)(\\d{4})_to_(Rainy|Dry)(\\d{4})$",
    remove = FALSE
  ) %>%
  mutate(
    y1 = as.integer(y1),
    y2 = as.integer(y2),
    s1 = if_else(s1_txt == "Rainy", 0L, 1L),  # To force the order as Rainy before and Dry then
    s2 = if_else(s2_txt == "Rainy", 0L, 1L)
  ) %>%
  arrange(y1, s1, y2, s2) %>%
  pull(transition_label)

label_colors <- c(
  "VSB302" = "#439e1b",
  "VSB303" = "#439e1b",
  "VSB304" = "#439e1b",
  "VSN301" = "#439e1b",
  "VSN302" = "#439e1b",
  "VSN306" = "#439e1b",
  "VSN307" = "#439e1b",
  "VSN309" = "#439e1b",
  "VSN310" = "#439e1b",
  "VSB301" = "#e3e218",
  "VSB305" = "#e3e218",
  "VSB306" = "#e3e218",
  "VSN303" = "#e3e218",
  "VSN304" = "#e3e218",
  "VSN305" = "#e3e218"
)
color_label <- function(site) {
  col <- label_colors[[site]]
  glue::glue("<span style='color:{col}'>{site}</span>")
}
x_labels <- setNames(lapply(names(label_colors), color_label), names(label_colors))

plot_list <- df_plot %>%   # Create the graphics for each transition by dividing the dataset in subgroup and applying a function for each one
  mutate(
    transition_label = factor(transition_label, levels = transitions_order),
    siteID = factor(siteID, levels = site_order)  
  ) %>%
  group_split(transition_label) %>%
  map(~{
    tlab <- as.character(unique(.x$transition_label))
    ggplot(.x, aes(x = siteID, y = area_km2, fill = transition_type)) +
      geom_col() +
      geom_text(
        aes(label = ifelse(percent > 3, paste0(round(percent), "%"), "")),
        position = position_stack(vjust = 0.5), size = 2.5, color = "black"
      ) +
      scale_fill_manual(
        values = transition_colors,
        breaks = names(type_labels),
        labels = type_labels,
        name   = "Transition type"
      ) +
      scale_y_continuous(labels = label_number(big.mark = "'")) +
      labs(
        title = paste("Changes in species areas -", tlab),
        x = "Sites", y = "Area (km²)"
      ) +
      
      theme_minimal() +
      scale_x_discrete(labels = x_labels) +
      theme(
        axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1)
      )
  })

purrr::walk(plot_list, print)   # Print all the graphics
