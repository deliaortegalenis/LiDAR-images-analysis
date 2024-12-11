
library(terra)

################################
# List directories for each LiDAR file's CHM rasters
#################################

lidar_dirs <- list.dirs("L:/OUTPUT_LIDAR_all" , full.names = TRUE, recursive = FALSE)

# Function to load rasters for a single polygon across multiple LiDAR files
load_rasters_by_polygon <- function(polygon_index, lidar_dirs) {
  raster_list <- list()
  
  # Loop through each LiDAR file's directory
  for (dir in lidar_dirs) {
    # Construct the raster file name for this polygon in the current directory
    raster_file <- file.path(dir, paste0("chm_polygon_", polygon_index, ".tif"))
    
    if (file.exists(raster_file)) {
      raster_list[[basename(dir)]] <- rast(raster_file)  # Load raster
    }
  }
  
  return(raster_list)  # List of rasters for this polygon across LiDAR files
}


#################################
###Crown cover percentage
################################

calculate_crown_cover <- function(chm_raster, canopy_threshold = 1) {
  # Create a binary raster where values above the threshold are canopy (1), and below are non-canopy (0)
  canopy_raster <- chm_raster > canopy_threshold
  
  # Get the frequency of all pixel values (0s and 1s) without NA removal directly
  pixel_freq <- freq(canopy_raster, digits = 0)
  
  # Remove NA rows from the frequency table
  pixel_freq <- na.omit(pixel_freq)
  
  # Extract total number of valid (non-NA) pixels
  total_pixels <- sum(pixel_freq[, "count"], na.rm = TRUE)
  
  # Extract number of canopy pixels (i.e., value == 1)
  canopy_pixels <- pixel_freq[pixel_freq[, "value"] == 1, "count"]
  
  # If no canopy pixels exist, set to 0
  if (length(canopy_pixels) == 0) {
    canopy_pixels <- 0
  }
  
  # Calculate canopy cover percentage
  canopy_cover_percentage <- (canopy_pixels / total_pixels) * 100
  
  return(canopy_cover_percentage)
}



####################
##summary statistics for each polygon
####################
 

summarize_chm_to_dataframe <- function(raster_list, lidar_file_name, polygon_index,  canopy_threshold = 1) {
  raster_stack <- rast(raster_list)  # Stack rasters
  
  # Calculate summary statistics
  mean_chm <- global(raster_stack, fun = "mean", na.rm = TRUE)[1]
  sd_chm <- global(raster_stack, fun = "sd", na.rm = TRUE)[1]
  min_chm <- global(raster_stack, fun = "min", na.rm = TRUE)[1]
  max_chm <- global(raster_stack, fun = "max", na.rm = TRUE)[1]
  
  # Coefficient of Variation (CV = SD / mean)
  cv_chm <- sd_chm / mean_chm
  
  # Calculate percentiles (25th, 50th (median), 75th)
  p5 <- global(raster_stack, fun = function(x) quantile(x, 0.05, na.rm = TRUE))[1]
  p25 <- global(raster_stack, fun = function(x) quantile(x, 0.25, na.rm = TRUE))[1]
  p50 <- global(raster_stack, fun = function(x) quantile(x, 0.50, na.rm = TRUE))[1]
  p75 <- global(raster_stack, fun = function(x) quantile(x, 0.75, na.rm = TRUE))[1]
  p90 <- global(raster_stack, fun = function(x) quantile(x, 0.90, na.rm = TRUE))[1]
  p95 <- global(raster_stack, fun = function(x) quantile(x, 0.95, na.rm = TRUE))[1]
  
  # Calculate crown cover percentage
  crown_cover_percentage <- calculate_crown_cover(raster_stack, canopy_threshold)
  
  
  # Return a named data frame, including LiDAR file name and polygon ID
  return(data.frame(
    lidar_file = lidar_file_name,
    polygon_id = polygon_index,
    mean = mean_chm,
    sd = sd_chm,
    min = min_chm,
    max = max_chm,
    cv = cv_chm,
    p5 = p5,
    p25 = p25,
    p50 = p50,
    p75 = p75,
    p90 = p90,
    p95 = p95,
    crown_cover_percentage = crown_cover_percentage
  ))
}


#########################
##Loop over each polygon and LIDAR file
#########################

# List of directories for each LiDAR file
lidar_dirs <- list.dirs("L:/OUTPUT_LIDAR_all", full.names = TRUE, recursive = FALSE)

# Initialize a data frame to store the statistics for all polygons across all LiDAR files
all_polygon_stats <- data.frame()

# Loop through each LiDAR file (folder)
for (lidar_dir in lidar_dirs) {
  # Extract the folder name (LiDAR file name)
  lidar_file_name <- basename(lidar_dir)
  
  # Loop through each polygon (assuming 24 polygons)
  for (polygon_index in 1:24) {
    # Load rasters for this polygon across the current LiDAR file
    rasters_for_polygon <- load_rasters_by_polygon(polygon_index, lidar_dir)
    
    # Summarize the CHM statistics for this polygon and LiDAR file
    polygon_stats <- summarize_chm_to_dataframe(rasters_for_polygon, lidar_file_name, polygon_index)
    
    # Append the results to the main data frame
    all_polygon_stats <- rbind(all_polygon_stats, polygon_stats)
  }
}

# View the final data frame (contains both lidar_file and polygon_id columns)
head(all_polygon_stats)


export(all_polygon_stats, "C:/Users/dortega/Desktop/data_complete2.xlsx") 
