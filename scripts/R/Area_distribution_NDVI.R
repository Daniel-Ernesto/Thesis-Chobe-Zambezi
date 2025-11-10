############## Area distribution and NDVI test and visualization ###############
### A. Install packages and download libraries
install.packages(c("tidyverse", "ggplot2", "corrplot", "car",
"GGally", "sf","dunn.test", "FSA", "read_csv", "reshape2",
"cluster", "ggpubr"))

library(tidyverse)
library(ggplot2)
library(corrplot)
library(car)
library(GGally)
library(sf)
library(dunn.test)
library(FSA)
library(readr)
library(reshape2)
library(cluster)
library(readxl)
library(ggpubr)
library(dplyr)
library(factoextra)
library(scales)
library(dunn.test)

### B. Download all the table to analyse
ds <- read_excel("C:/Users/danie/OneDrive/Desktop/MASTER/PyC/Statistiche/Statistics.xlsx") # Contains all the surfaces information from each classified site
df = read_excel("C:/Users/danie/OneDrive/Desktop/MASTER/PyC/Statistiche/df.xlsx")          # Contains all the training points sampled on the 9 features

### C. Class distribution in each site
ds_filtered <- ds %>% filter(Class != 0) # Clean the dataset from the class = 0 (Total)

ds_filtered <- ds_filtered %>%
  mutate(Country = ifelse(str_detect(Sites, "VSB"), "Botswana", "Namibia")) # Add the columns "Country"

class_names <- c(          # Encode the number of the class with the precise name
  "1" = "Stagnant Water",
  "2" = "Soil",
  "3" = "Large C. mopane",
  "4" = "Small C. mopane",
  "5" = "Other Vegetation",
  "6" = "B. plurijuga",
  "7" = "Grass")

ds_filtered$Class <- factor(ds_filtered$Class,  # Label the class
                            levels = 1:7,
                            labels = class_names[as.character(1:7)])

class_colors <- c(                    # Define the colors for each class
  "Stagnant Water"   = "#366d9e",
  "Soil"             = "#e79046",
  "Large C. mopane"  = "#439e1b",
  "Small C. mopane"  = "#80eb69",
  "Other Vegetation" = "#ca5fc4",
  "B. plurijuga"     = "#e3e218",
  "Grass"            = "#daeaeb")

ds_perc <- ds_filtered %>%        # Define the percentages for each class over the site total surface
  group_by(Sites) %>%
  mutate(
    total_area = sum(Area_km2, na.rm = TRUE),
    perc = Area_km2 / total_area * 100
  ) %>%
  ungroup()

ggplot(ds_perc, aes(x = Sites, y = Area_km2, fill = Class)) +      # Create the stacked bar plot 
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = ifelse(perc > 3, paste0(round(perc), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 2.5, color = "black"
  ) +
  scale_fill_manual(
    values = class_colors,
    labels = c(
      "Stagnant Water",
      "Soil",
      expression("Large "*italic("C. mopane")),
      expression("Small "*italic("C. mopane")),
      "Other Vegetation",
      expression(italic("B. plurijuga")),
      "Grass"
    )
  ) +
  scale_y_continuous(labels = scales::label_number(big.mark = "'")) +
  labs(title = "Class area for each site",
       x = "Sites", y = "Area (km²)", fill = "Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### D. Area and NVI
# 1. Large Mopane
mopane <- ds %>%                       # Filter only the Class 3 separated by country on the ds table
  filter(Class == 3) %>%
  mutate(
    Country = ifelse(str_detect(Sites, "VSB"), "Botswana", "Namibia"),
    Area_m2 = Area_km2 * 1e6
  )

bw <- mopane %>% filter(Country == "Botswana") %>% pull(Area_m2) # Extract the area value for each country
nm <- mopane %>% filter(Country == "Namibia") %>% pull(Area_m2)

test <- wilcox.test(Area_m2 ~ Country, data = mopane)   # Apply the Wilcoxon test for the countries to test the area
print(test) # to print the results of the test

cat("Average Botswana:", format(round(mean(bw, na.rm = TRUE), 2), big.mark = ","), "m²\n") # To print the average values of area in m2 for the 2 countries
cat("Average Namibia:", format(round(mean(nm, na.rm = TRUE), 2), big.mark = ","), "m²\n")

adult_mopane <- df %>%        # Filter only the Class 3 separated by country on the df table
  filter(class == 3) %>%
  mutate(
    Site = source_file,
    Country = ifelse(str_detect(source_file, "VSB"), "Botswana", "Namibia"))

ggplot(adult_mopane, aes(x = Site, y = NDVI, fill = Site)) +      # Box plot for the NDVI values on each site
  geom_boxplot() +
  labs(
    title = expression("Large " * italic("C. mopane") * " – NDVI by Site"),  # To write C. mopane on italic
    x = "Sites", y = "NDVI"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

test_ndvi <- wilcox.test(NDVI ~ Country, data = adult_mopane)  # Apply the wilcoxon test to the NDVI
print(test_ndvi)

bw_ndvi <- mean(adult_mopane$NDVI[adult_mopane$Country == "Botswana"], na.rm = TRUE) # Compute the average value of NDVI
nm_ndvi <- mean(adult_mopane$NDVI[adult_mopane$Country == "Namibia"], na.rm = TRUE)

cat("Average NDVI - Botswana:", round(bw_ndvi, 3), "\n")     # Print the average value 
cat("Average NDVI - Namibia:",  round(nm_ndvi, 3), "\n")


# 2. Small Mopane
Smopane <- ds %>%                       # Filter only the Class 4 separated by country on the ds table
  filter(Class == 4) %>%
  mutate(
    Country = ifelse(str_detect(Sites, "VSB"), "Botswana", "Namibia"),
    Area_m2 = Area_km2 * 1e6
  )

Sbw <- Smopane %>% filter(Country == "Botswana") %>% pull(Area_m2) # Extract the area value for each country
Snm <- Smopane %>% filter(Country == "Namibia") %>% pull(Area_m2)

Stest <- wilcox.test(Area_m2 ~ Country, data = Smopane)   # Apply the Wilcoxon test for the countries to test the area
print(Stest) # to print the results of the test

cat("Average Botswana:", format(round(mean(Sbw, na.rm = TRUE), 2), big.mark = ","), "m²\n") # To print the average values of area in m2 for the 2 countries
cat("Average Namibia:", format(round(mean(Snm, na.rm = TRUE), 2), big.mark = ","), "m²\n")

small_mopane <- df %>%        # Filter only the Class 4 separated by country on the df table
  filter(class == 4) %>%
  mutate(
    Site = source_file,
    Country = ifelse(str_detect(source_file, "VSB"), "Botswana", "Namibia"))

ggplot(small_mopane, aes(x = Site, y = NDVI, fill = Site)) +      # Box plot for the NDVI values on each site
  geom_boxplot() +
  labs(
    title = expression("Small " * italic("C. mopane") * " – NDVI by Site"),  # To write C. mopane on italic
    x = "Sites", y = "NDVI"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Stest_ndvi <- wilcox.test(NDVI ~ Country, data = small_mopane)  # Apply the wilcoxon test to the NDVI
print(Stest_ndvi)

Sbw_ndvi <- mean(small_mopane$NDVI[small_mopane$Country == "Botswana"], na.rm = TRUE) # Compute the average value of NDVI
Snm_ndvi <- mean(small_mopane$NDVI[small_mopane$Country == "Namibia"], na.rm = TRUE)

cat("Average NDVI - Botswana:", round(Sbw_ndvi, 3), "\n")     # Print the average value 
cat("Average NDVI - Namibia:",  round(Snm_ndvi, 3), "\n")


# 3. Baikiaea
baik <- ds %>%                       # Filter only the Class 6 separated by country on the ds table
  filter(Class == 6) %>%
  mutate(
    Country = ifelse(str_detect(Sites, "VSB"), "Botswana", "Namibia"),
    Area_m2 = Area_km2 * 1e6
  )

Bbw <- baik %>% filter(Country == "Botswana") %>% pull(Area_m2) # Extract the area value for each country
Bnm <- baik %>% filter(Country == "Namibia") %>% pull(Area_m2)

Btest <- wilcox.test(Area_m2 ~ Country, data = baik)   # Apply the Wilcoxon test for the countries to test the area
print(Btest) # to print the results of the test

cat("Average Botswana:", format(round(mean(Bbw, na.rm = TRUE), 2), big.mark = ","), "m²\n") # To print the average values of area in m2 for the 2 countries
cat("Average Namibia:", format(round(mean(Bnm, na.rm = TRUE), 2), big.mark = ","), "m²\n")

baikiaea <- df %>%        # Filter only the Class 6 separated by country on the df table
  filter(class == 6) %>%
  mutate(
    Site = source_file,
    Country = ifelse(str_detect(source_file, "VSB"), "Botswana", "Namibia"))

ggplot(baikiaea, aes(x = Site, y = NDVI, fill = Site)) +      # Box plot for the NDVI values on each site
  geom_boxplot() +
  labs(
    title = expression(italic("B. plurijuga") * " – NDVI by Site"),  # To write B. plurijuga on italic
    x = "Sites", y = "NDVI"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Btest_ndvi <- wilcox.test(NDVI ~ Country, data = baikiaea)  # Apply the wilcoxon test to the NDVI
print(Btest_ndvi)

Bbw_ndvi <- mean(baikiaea$NDVI[baikiaea$Country == "Botswana"], na.rm = TRUE) # Compute the average value of NDVI
Bnm_ndvi <- mean(baikiaea$NDVI[baikiaea$Country == "Namibia"], na.rm = TRUE)

cat("Average NDVI - Botswana:", round(Bbw_ndvi, 3), "\n")     # Print the average value 
cat("Average NDVI - Namibia:",  round(Bnm_ndvi, 3), "\n")
