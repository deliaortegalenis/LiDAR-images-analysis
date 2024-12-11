
#####################
#Intensity
####################

# Load necessary libraries
library(lidR)
library(sf)
library(dplyr)
library(data.table)

# Load the shapefile containing polygons
polygons <- st_read("L:/new_shape/c_exper_2.shp")

shapefile_sf=st_as_sf(polygons)
st_crs(shapefile_sf)=4326  
polygons=st_transform(shapefile_sf, crs = "EPSG:32618")


las <- readLAS("L:/Test_LIDAR2/cloud1aa9e0a3556da081.las")

if (is.empty(las)) {
  stop("The LAS file is empty. Please provide a valid LAS file.")
}

id_column <- "parcela"

# Initialize a results list to store statistics for each polygon
results_list <- list()

# Loop through each polygon in the shapefile
for (i in 1:nrow(polygons)) {
  polygon <- polygons[i, ]
  
  # Clip the LAS data to the polygon
  las_clip <- clip_roi(las, polygon)
  
  # Check if the clipped LAS data contains points
  if (!is.empty(las_clip)) {
    intensity_values <- las_clip@data$Intensity
    
    # Check if intensity values exist and compute statistics
    if (length(intensity_values) > 0 && !all(is.na(intensity_values))) {
      polygon_stats <- data.frame(
        Polygon_ID = polygon[[id_column]],
        Mean = mean(intensity_values, na.rm = TRUE),
        Median = median(intensity_values, na.rm = TRUE),
        Std_Dev = sd(intensity_values, na.rm = TRUE),
        Min = min(intensity_values, na.rm = TRUE),
        Max = max(intensity_values, na.rm = TRUE),
        Q5 = quantile(intensity_values, 0.05, na.rm = TRUE),
        Q25 = quantile(intensity_values, 0.25, na.rm = TRUE),
        Q75 = quantile(intensity_values, 0.75, na.rm = TRUE),
        Q90 = quantile(intensity_values, 0.90, na.rm = TRUE),
        Q95 = quantile(intensity_values, 0.95, na.rm = TRUE)
      )
    } else {
      # Handle case when intensity values are missing or empty
      polygon_stats <- data.frame(
        Polygon_ID = polygon[[id_column]],  # Use the correct ID column
        Mean = NA,
        Median = NA,
        Std_Dev = NA,
        Min = NA,
        Max = NA,
        Q5 = NA,
        Q25 = NA,
        Q75 = NA,
        Q90 = NA,
        Q95 = NA
      )
    }
  } 
  
  # Add the statistics to the results list
  results_list[[i]] <- polygon_stats
}

# Combine all results into a single data frame
results <- do.call(rbind, results_list)



