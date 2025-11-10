################ Area distribution and NDVI test and visualization ###############
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
library(tidyr)
library(rstatix)
library(stringr)
library(openxlsx)

### B. Download all the table to analyse
data <- read_excel("Geodata.xlsx")       # Contains the information about the sampling during the fieldwork with all the physical parameters

### C. Overview of the trees on the Geodatabase (Table 2)
sampling_cols <- c("Leaves", "Bark", "Slice", "Cambium", "Flower",   # Define the limitant columns for the sampled and measured trees
                   "sprout", "Core", "Fruit_seeds", "Stem", "roots")
measure_cols <- c("CBH")                                             # Define the "measured" trees (any tree that has a value on CBH measure as example)

data <- data %>%
  mutate(Sampled = if_any(all_of(sampling_cols), ~!is.na(.)),         # Define the 3 category (Sampled, Measured, Located) for each tree
         Measured = if_any(all_of(measure_cols), ~!is.na(.)),
         Located = if_all(c("Latitude_N", "Longitude_E"), ~!is.na(.)))

species_extended <- data %>%                    # Table counting the 3 categories for each species
  group_by(species) %>%
  summarise(
    n = n(),
    Sampled = sum(Sampled, na.rm = TRUE),
    Measured = sum(Measured, na.rm = TRUE),
    Located = sum(Located, na.rm = TRUE))
print(species_extended)

site_extended <- data %>%                       # Create the table by assigning the Mopane and Baikiaea individuals to the 3 category
  mutate(
    isMopane = species == "Colophospermum mopane",
    isBaikiae = species == "Baikiaea plurijuga",
    isUnknown = species == "?"
  ) %>%
  group_by(Site_name) %>%
  summarise(
    n = n(),
    Sampled_Mopane = sum(Sampled & isMopane, na.rm = TRUE),    # Define the columns
    Measured_Mopane = sum(Measured & isMopane, na.rm = TRUE),
    Located_Mopane = sum(Located & isMopane, na.rm = TRUE),
    Sampled_Baikiae = sum(Sampled & isBaikiae, na.rm = TRUE),
    Measured_Baikiae = sum(Measured & isBaikiae, na.rm = TRUE),
    Located_Baikiae = sum(Located & isBaikiae, na.rm = TRUE),
    `?` = sum(isUnknown, na.rm = TRUE))
print(site_extended)                                           # Print the table


### D. Box plots and tests on the 13 variables
species_data <- data %>%                                       # Filter the input data by species and country
  filter(species %in% c("Baikiaea plurijuga", "Colophospermum mopane")) %>%
  mutate(
    species = factor(species,
                     levels = c("Baikiaea plurijuga", "Colophospermum mopane"),
                     labels = c("Baikiaea plurijuga", "Colophospermum mopane")),
    Country = ifelse(str_detect(survey_abreviation, "VSB"), "Botswana", "Namibia"),
    group = paste(species, Country, sep = "_")
  )

vars <- c("NDVI", "NDRE", "BSI", "Distance_short","Distance_long", # Define the 13 variables
                  "Height", "DBH","DBA", "Crown_height", "Radius_canopy",
                  "Canopy_cover", "Number_trunks", "altitude_ge")

for (var in vars) {  # create a loop on each of the variables to create the plot separated by country and species
  
  df_var <- species_data %>%
    select(species, Country, all_of(var)) %>%
    filter(!is.na(.data[[var]]))
  
  p <- ggplot(df_var, aes(x = species, y = .data[[var]], fill = Country)) +
    geom_boxplot(position = position_dodge(0.8)) +
    scale_fill_manual(values = c("Botswana" = "#2A9D8F", "Namibia" = "#F4A261")) +
    scale_x_discrete(
      labels = c(
        "Baikiaea plurijuga" = expression(italic("B. plurijuga")),
        "Colophospermum mopane" = expression(italic("C. mopane"))
      )
    ) +
    labs(title = paste("Comparison between species and countries –", var),
         x = "Species", y = var, fill = "Country") +
    theme_minimal(base_size = 12)
  
  print(p)                 # Print all the 13 plots
}

results_list <- list()    # Create a list to store the tests results

for (var in vars) {              # Create a loop on all the variables to apply the 3 tests
  df_var <- species_data %>%
    select(species, Country, group, all_of(var)) %>%
    filter(!is.na(.data[[var]]))
  
  means <- df_var %>%               # Calculate the mean for the 2 species in the 2 countries
    group_by(group) %>%
    summarise(mean_value = mean(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = group, values_from = mean_value)
  
  ## inter-species test
  test_species <- wilcox_test(df_var, formula = as.formula(paste(var, "~ species")))
  p_species <- test_species$p
  signif_species <- ifelse(p_species < 0.05, "Significativo", "NS")
  
  # Mopane intra-species test
  mopane_df <- df_var %>% filter(species == "Colophospermum mopane")
  test_mopane <- wilcox_test(mopane_df, formula = as.formula(paste(var, "~ Country")))
  p_mopane <- test_mopane$p
  signif_mopane <- ifelse(p_mopane < 0.05, "Significativo", "NS")
  
  # Baikiaea intra-species test
  baikiaea_df <- df_var %>% filter(species == "Baikiaea plurijuga")
  test_baikiaea <- wilcox_test(baikiaea_df, formula = as.formula(paste(var, "~ Country")))
  p_baikiaea <- test_baikiaea$p
  signif_baikiaea <- ifelse(p_baikiaea < 0.05, "Significativo", "NS")
  
  summary_row <- data.frame(                                      # Define the name of the columns
    Variabile = var,
    Mopane_Botswana = means$`Colophospermum mopane_Botswana`,
    Mopane_Namibia = means$`Colophospermum mopane_Namibia`,
    Baikiaea_Botswana = means$`Baikiaea plurijuga_Botswana`,
    Baikiaea_Namibia = means$`Baikiaea plurijuga_Namibia`,
    p_inter_specie = p_species,
    Significativo_inter = signif_species,
    p_intra_mopane = p_mopane,
    Significativo_mopane = signif_mopane,
    p_intra_baikiaea = p_baikiaea,
    Significativo_baikiaea = signif_baikiaea
  )
  
  results_list[[var]] <- summary_row
}

results_table <- bind_rows(results_list)  # unify the results of the 3 tests in one table

write.xlsx(results_table, "Risultati_Wilcoxon.xlsx")  # Save the tables as excel file

### E. Corrplots
data_tot <- read_excel("Geodata.xlsx")              # Load the geodatabase
num_vars <- c("altitude_ge", "NDVI", "NDRE", "BSI", # Selecting the variables
              "Distance_short", "Distance_long", 
              "Height", "DBH", "DBA", 
              "Crown_height", "Radius_canopy", 
              "Canopy_cover", "Number_trunks")

measure_cols <- c("CBH")      # Selectin only the trees "measured" (any tree that has a value on CBH measure as example)
data <- data_tot %>%
  mutate(Measured = if_any(all_of(measure_cols), ~!is.na(.))) %>%
  filter(Measured)

new_data <- data %>%          # Creating a new dataset with only the columns of the selected variables
  select(all_of(num_vars), species, survey_abreviation, Site_name) 
print(new_data)

mopane_data <- new_data %>% filter(species == "Colophospermum mopane")  # Split the dataframe for the 2 species
baikiaea_data <- new_data %>% filter(species == "Baikiaea plurijuga")

correlation_test_matrix <- function(df, vars) {   # Function for calculate the p value matrix of the correlation with spearman method
  cor_p <- matrix(NA, ncol = length(vars), nrow = length(vars),
                  dimnames = list(vars, vars))
  for (i in seq_along(vars)) {
    for (j in seq_along(vars)) {
      test <- cor.test(df[[vars[i]]], df[[vars[j]]], method = "spearman", use = "complete.obs")
      cor_p[i, j] <- test$p.value
    }
  }
  return(cor_p)}

mopane_num <- mopane_data %>% select(all_of(num_vars)) %>% drop_na()  # Calculate the correlation matrix and p value for the mopane
mopane_cor <- cor(mopane_num, use = "complete.obs")
mopane_p <- correlation_test_matrix(mopane_num, num_vars)

baikiaea_num <- baikiaea_data %>% select(all_of(num_vars)) %>% drop_na() # Calculate the correlation matrix and p value for the baikiaea
baikiaea_cor <- cor(baikiaea_num, use = "complete.obs")
baikiaea_p <- correlation_test_matrix(baikiaea_num, num_vars)

# Mopane
corrplot(mopane_cor, method = "color", type = "upper", order = "hclust",  # Plot only the significative correlation
         p.mat = mopane_p, sig.level = 0.05, insig = "blank",
         tl.cex = 0.8,
         title = bquote("Correlations – " * italic("C. mopane")),
         mar = c(0, 0, 1, 0))

# Baikiaea
corrplot(baikiaea_cor, method = "color", type = "upper", order = "hclust", # Plot only the significative correlation
         p.mat = baikiaea_p, sig.level = 0.05, insig = "blank",
         tl.cex = 0.8,
         title = bquote("Correlations – " * italic("B. plurijuga")),
         mar = c(0, 0, 1, 0))

# Reduction of the variables for PCA
find_high_cor_pairs <- function(cor_mat, threshold = 0.8) { # Function to identify the correlations above 0.8 or -0.8
  var_names <- colnames(cor_mat)
  high_cor_pairs <- data.frame(Var1 = character(), Var2 = character(), Correlation = numeric())
  
  for (i in 1:(ncol(cor_mat)-1)) {
    for (j in (i+1):ncol(cor_mat)) {
      corr_value <- cor_mat[i, j]
      if (abs(corr_value) > threshold) {
        high_cor_pairs <- rbind(high_cor_pairs, 
                                data.frame(Var1 = var_names[i],
                                           Var2 = var_names[j],
                                           Correlation = round(corr_value, 3)))
      }
    }
  }
  return(high_cor_pairs)}

print(find_high_cor_pairs(mopane_cor, threshold = 0.8)) # High correlations for mopane

print(find_high_cor_pairs(baikiaea_cor, threshold = 0.8)) # High correlations for baikiaea

mopane_var <- apply(mopane_num, 2, var, na.rm = TRUE) # Show variances of the variables for mopane
print(round(mopane_var, 3))

baikiaea_var <- apply(baikiaea_num, 2, var, na.rm = TRUE) # Show variances of the variables for baikiaea
print(round(baikiaea_var, 3))

### F. PCA - Mopane
data_tot <- read_excel("Geodata.xlsx")  # Loading the data

bio_vars <- c("altitude_ge","NDVI","BSI","Distance_short","Distance_long",
              "Height", "Crown_height", "Radius_canopy", "Canopy_cover","DBH", 
              "Number_trunks") # Selecting only the desired variables (excluding DBA and NDRE based on the results of the previous reduction analysis)

measure_cols <- c("CBH")       # Filtering only the "measured" trees for mopane
data <- data_tot %>%
  mutate(Measured = if_any(all_of(measure_cols), ~!is.na(.))) %>%
  filter(Measured, species == "Colophospermum mopane")

pca_input <- data %>%         # Creating a new dataset with only the desired columns and without NA
  select(all_of(bio_vars), species, survey_abreviation, Site_name) %>%
  drop_na()

pca_scaled <- scale(pca_input %>% select(all_of(bio_vars)))  # Scale the dataset

fviz_nbclust(pca_scaled, kmeans, method = "silhouette") # Silhouette method to identify the optimal number of clusters

set.seed(123)                                           # Calculate the clusters with kmeans
k_opt <- 2
km_result <- kmeans(pca_scaled, centers = k_opt, nstart = 25)

pca_input$Cluster <- as.factor(km_result$cluster)       # Add the cluster factor to the original dataset

pca_res <- prcomp(pca_scaled, center = TRUE, scale. = TRUE) # Perform the PCA

fviz_pca_biplot(pca_res,                                # Graphic of the pca, with clusters and loadings
                geom.ind = "point",
                habillage = pca_input$Cluster,
                addEllipses = TRUE,
                label = "var",
                repel = TRUE,
                palette = c("#F8766D", "#00BFC4")) +
  labs(title = expression("PCA with the identified clusters – " * italic("C. mopane")))

summary(pca_res)                   # Summary of the PCA

print(round(pca_res$rotation, 3))  # Contribution of the variables to each Principal component

cluster_summary <- pca_input %>%   # Summary table of the assigned clusters
  select(species, survey_abreviation, Site_name, Cluster)
print(cluster_summary)
sum(cluster_summary$Cluster == 2)  # How many rows are cluster 2
sum(cluster_summary$Cluster == 1)  # How many rows are cluster 1


fviz_contrib(pca_res, choice = "var", axes = 1, top = 10) # Visualize the contribution (%) of the variablse to the 3 firts principal components
fviz_contrib(pca_res, choice = "var", axes = 2, top = 10)
fviz_contrib(pca_res, choice = "var", axes = 3, top = 10)

ggplot(pca_input, aes(x = Site_name, fill = Cluster)) +   # Distribution of clusters per site
  geom_bar(position = "fill") +
  labs(y = "Proportion", x="Sites", title = expression("Cluster distribution by site – " * italic("C. mopane"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

for (var in bio_vars) {                                  # Comparison between the 2 cluster on the variables
  
  df_var <- pca_input %>%                                # Filter the data on the variables
    select(Cluster, all_of(var)) %>%
    filter(!is.na(.data[[var]]))
  
  shapiro_res <- df_var %>%                              # Check the normality
    group_by(Cluster) %>%
    summarise(p_value = tryCatch(shapiro.test(.data[[var]])$p.value, error = function(e) NA))
  print(shapiro_res)
  all_normal <- all(shapiro_res$p_value > 0.05, na.rm = TRUE)
  
  formula <- as.formula(paste(var, "~ Cluster"))         # Check the homoscedasticity
  levene_res <- tryCatch(leveneTest(formula, data = df_var), error = function(e) NA)
  
  if (!is.na(levene_res[1, "Pr(>F)"])) {
    homoscedastic <- levene_res[1, "Pr(>F)"] > 0.05
  } else {
    homoscedastic <- FALSE
  }
  if (all_normal & homoscedastic) {                      # Choice between parametric o non parametric 
    test_res <- t.test(formula, data = df_var, var.equal = TRUE)
  } else {
    test_res <- wilcox.test(formula, data = df_var)
  }
  
  print(test_res)                                        # Print the results of the test
  
  p_val <- test_res$p.value                              # Extract the p value
  p_lab <- if (p_val < 0.001) {
    "p < 0.001"
  } else {
    paste("p =", format(round(p_val, 3), scientific = TRUE))
  }
  
  p <- ggplot(df_var, aes(x = Cluster, y = .data[[var]], fill = Cluster)) +  # Clusters plot
    geom_boxplot() +
    labs(title = paste("Distribution of", var, "by cluster")) +
    theme_minimal() +
    theme(legend.position = "none")
  print(p)
}

### G. PCA - Baikiaea
data_tot <- read_excel("Geodata.xlsx")

bio_vars <- c("altitude_ge","NDVI","BSI","Distance_short","Distance_long",
              "Height", "Crown_height", "Radius_canopy", "Canopy_cover","DBH", 
              "Number_trunks") 

measure_cols <- c("CBH")
data <- data_tot %>%
  mutate(Measured = if_any(all_of(measure_cols), ~!is.na(.))) %>%
  filter(Measured, species == "Baikiaea plurijuga")

pca_input <- data %>%
  select(all_of(bio_vars), species, survey_abreviation, Site_name) %>%
  drop_na()

pca_scaled <- scale(pca_input %>% select(all_of(bio_vars)))

fviz_nbclust(pca_scaled, kmeans, method = "silhouette") 

set.seed(123)
k_opt <- 2
km_result <- kmeans(pca_scaled, centers = k_opt, nstart = 25)

pca_input$Cluster <- as.factor(km_result$cluster)

pca_res <- prcomp(pca_scaled, center = TRUE, scale. = TRUE)

fviz_pca_biplot(pca_res,
                geom.ind = "point",
                habillage = pca_input$Cluster,
                addEllipses = TRUE,
                label = "var",
                repel = TRUE,
                palette = c("#F8766D", "#00BFC4")) +
  labs(title = expression("PCA with the identified clusters – " * italic("B. plurijuga")))

summary(pca_res)

print(round(pca_res$rotation, 3))

cluster_summary <- pca_input %>%
  select(species, survey_abreviation, Site_name, Cluster)
print(cluster_summary)
sum(cluster_summary$Cluster == 2)
sum(cluster_summary$Cluster == 1)

fviz_contrib(pca_res, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_res, choice = "var", axes = 2, top = 10)
fviz_contrib(pca_res, choice = "var", axes = 3, top = 10)

ggplot(pca_input, aes(x = Site_name, fill = Cluster)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", x= "Sites", title = expression("Cluster distribution by site – " * italic("B. plurijuga"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

for (var in bio_vars) {
  
  df_var <- pca_input %>%
    select(Cluster, all_of(var)) %>%
    filter(!is.na(.data[[var]]))
  
  shapiro_res <- df_var %>%
    group_by(Cluster) %>%
    summarise(p_value = tryCatch(shapiro.test(.data[[var]])$p.value, error = function(e) NA))
  
  print(shapiro_res)
  all_normal <- all(shapiro_res$p_value > 0.05, na.rm = TRUE)
  
  formula <- as.formula(paste(var, "~ Cluster"))
  levene_res <- tryCatch(leveneTest(formula, data = df_var), error = function(e) NA)
  
  if (!is.na(levene_res[1, "Pr(>F)"])) {
    homoscedastic <- levene_res[1, "Pr(>F)"] > 0.05
  } else {
    homoscedastic <- FALSE
  }
  
  if (all_normal & homoscedastic) {
    test_res <- t.test(formula, data = df_var, var.equal = TRUE)
  } else {
    test_res <- wilcox.test(formula, data = df_var)
  }
  
  print(test_res)
  
  p_val <- test_res$p.value
  p_lab <- if (p_val < 0.001) {
    "p < 0.001"
  } else {
    paste("p =", format(round(p_val, 3), scientific = TRUE))
  }
  
  p <- ggplot(df_var, aes(x = Cluster, y = .data[[var]], fill = Cluster)) +  
    geom_boxplot() +
    labs(title = paste("Distribution of", var, "by cluster")) +
    theme_minimal() +
    theme(legend.position = "none")
  print(p)
  
  print(p)
}
