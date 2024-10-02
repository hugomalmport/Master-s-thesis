# Load necessary libraries
library(googlesheets4)
library(ggplot2)
library(tidyr)
library(dplyr)
library(nls2)
library(lme4)
library(lmerTest)

# Clean the environment
rm(list = ls())

# Load the data measurements from Google Sheets
kelp_data <- read_sheet("https://docs.google.com/spreadsheets/d/1FHVlBrssyievdxxegrkq6g7MS1HEL3AAirMl7akIjMo",
                        sheet = "pilotexperiment1")

## Weight calculations

# Remove rows with NA values in the relevant columns for weight calculations
kelp_data_weight <- na.omit(kelp_data[, c("stipe_distance_mm", "species",
                                          "weight1_g", "weight2_g", 
                                          "weight3_g")])

## Plot 1 - Weight reduction by time and divided into groups based on stipe distances

# Calculate weight reduction at each time point
kelp_data_weight <- kelp_data_weight %>%
  mutate(
    weight_red_second = (weight2_g - weight1_g) / weight1_g * 100,    # Weight reduction between first and second measurement
    weight_red_final = (weight3_g - weight1_g) / weight1_g * 100      # Weight reduction between first and final measurement
  )

# Reshape the data to long format
kelp_long_weight <- kelp_data_weight %>%
  pivot_longer(cols = c(weight_red_second, weight_red_final), 
               names_to = "time", 
               values_to = "weight_reduction") %>%
  mutate(time = recode(time, 
                       "weight_red_second" = "Day 4",
                       "weight_red_final" = "Day 7"))

# Define colors for the species
species_colors <- c("L. digitata" = "brown1", 
                    "L. hyperborea" = "forestgreen", 
                    "S. latissima" = "cornflowerblue")

# Create the faceted growth plot for Weight reduction
p1 <- ggplot(kelp_long_weight, aes(x = time, y = weight_reduction, group = species, color = species)) +
  geom_point(size = 3, alpha = 0.7) +  # Points for each sample
  geom_smooth(method = "lm", se = FALSE, linetype = "solid") +  # Trendline without confidence interval
  facet_wrap(~ stipe_distance_mm, scales = "free", labeller = as_labeller(function(x) paste0(x, " mm"))) +  # Group plots by stipe distance
  labs(title = "Weight reduction by stipe distance",
       x = "Time (days)", 
       y = "Weight reduction (%)",
       color = "Species") +
  scale_color_manual(values = species_colors) +  # Set specific colors for species
  scale_y_continuous(limits = c(-100, 0)) +  # Set same y-axis limits
  theme_minimal(base_size = 14) +  
  theme(legend.position = "top", 
        strip.background = element_blank())

print(p1)

## Plot 2 - Area reduction by time and divided into groups based on stipe distances

kelp_data_area <- na.omit(kelp_data[, c("stipe_distance_mm", "species",
                                        "area1_mm2", "area2_mm2", 
                                        "area3_mm2")])

# Calculate area reduction at each time point
kelp_data_area <- kelp_data_area %>%
  mutate(
    area_red_second = (area2_mm2 - area1_mm2) / area1_mm2 * 100,    # Area reduction between first and second measurement
    area_red_final = (area3_mm2 - area1_mm2) / area1_mm2 * 100      # Area reduction between first and final measurement
  )

# Reshape the data to long format
kelp_long_area <- kelp_data_area %>%
  pivot_longer(cols = c(area_red_second, area_red_final), 
               names_to = "time", 
               values_to = "area_reduction") %>%
  mutate(time = recode(time, 
                       "area_red_second" = "Day 4",
                       "area_red_final" = "Day 7"))

# Create the faceted growth plot for Area reduction
p2 <- ggplot(kelp_long_area, aes(x = time, y = area_reduction, group = species, color = species)) +
  geom_point(size = 3, alpha = 0.7) +  # Points for each sample
  geom_smooth(method = "lm", se = FALSE, linetype = "solid") +  # Trendline without confidence interval
  facet_wrap(~ stipe_distance_mm, scales = "free", labeller = as_labeller(function(x) paste0(x, " mm"))) +  # Group plots by stipe distance
  labs(title = "Area reduction by stipe distance",
       x = "Time (days)", 
       y = "Area reduction (%)",
       color = "Species") +
  scale_color_manual(values = species_colors) +  # Set specific colors for species
  scale_y_continuous(limits = c(-100, 0)) +  # Set same y-axis limits
  theme_minimal(base_size = 14) +  
  theme(legend.position = "top", 
        strip.background = element_blank())

print(p2)

## Plot 3 - Exponential model for Weight change

# Calculate absolute weight change (final weight - initial weight)
kelp_data <- kelp_data %>%
  mutate(weight_change = weight3_g - weight1_g)

# Fit an exponential model: weight_change ~ a * exp(b * stipe_distance_mm)
exp_model <- tryCatch({
  nls2(weight_change ~ a * exp(b * stipe_distance_mm),
       data = kelp_data, 
       start = list(a = min(kelp_data$weight_change, na.rm = TRUE), b = 0.01))
}, error = function(e) {
  message("Error in model fitting: ", e)
  return(NULL)
})

# Generate predicted values from the model if fitting was successful
if (!is.null(exp_model)) {
  kelp_data$predicted <- NA  # Initialize the predicted column with NA
  # Ensure to only assign predictions to the rows used in the model fitting
  kelp_data$predicted[!is.na(kelp_data$weight_change)] <- predict(exp_model)
  
  # Create the plot with the exponential trendline
  p3 <- ggplot(kelp_data, aes(x = stipe_distance_mm, y = weight_change)) +
    geom_point(aes(color = species), size = 3, alpha = 0.7) +  # Points for each sample
    geom_line(aes(y = predicted), color = "black", linetype = "solid", linewidth = 0.6) +  # Exponential trendline
    labs(title = "Exponential weight reduction",
         x = "Distance from stipe (mm)", 
         y = "Weight change (g)",
         color = "Species") +
    scale_color_manual(values = species_colors) +  # Set specific colors for species
    theme_minimal(base_size = 14) +  
    theme(legend.position = "top")
  
  print(p3)
}

## Plot 4 - Exponential model for Area change

# Calculate absolute area change (final area - initial area)
kelp_data_area <- kelp_data_area %>%
  mutate(area_change = area3_mm2 - area1_mm2)

# Fit an exponential model for area change
exp_model_area <- tryCatch({
  nls2(area_change ~ a * exp(b * stipe_distance_mm),
       data = kelp_data_area, 
       start = list(a = min(kelp_data_area$area_change, na.rm = TRUE), b = 0.01))
}, error = function(e) {
  message("Error in model fitting for area: ", e)
  return(NULL)
})

# Generate predicted values from the model if fitting was successful
if (!is.null(exp_model_area)) {
  kelp_data_area$predicted <- NA  # Initialize predicted column with NA
  kelp_data_area$predicted[!is.na(kelp_data_area$area_change)] <- predict(exp_model_area)  # Assign predictions correctly
  
  # Create the plot with the exponential trendline
  p4 <- ggplot(kelp_data_area, aes(x = stipe_distance_mm, y = area_change)) +
    geom_point(aes(color = species), size = 3, alpha = 0.7) +  # Points for each sample
    geom_line(aes(y = predicted), color = "black", linetype = "solid", linewidth = 0.6) +  # Exponential trendline
    labs(title = "Exponential Area Reduction",
         x = "Distance from stipe (mm)", 
         y = "Area change (mm^2)",
         color = "Species") +
    scale_color_manual(values = species_colors) +  # Set specific colors for species
    theme_minimal(base_size = 14) +  
    theme(legend.position = "top")
  
  print(p4)
}
