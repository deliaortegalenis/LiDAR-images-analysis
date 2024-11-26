


library(lidR)
library(sf)
library(raster)
library(terra)

#libraries LAScatalog
library(TreeLS)
library(spanner)
library(landecoutils)
library(rlas)

##########################
#analysis to each .las file
##########################

# Read the LAS files as a LAScatalog
catalog <- readLAScatalog("L:/Test_LIDAR2/")
dir = "L:/Test_LIDAR2/"

# Read the shapefile containing the polygons
shapefile <- shapefile("L:/new_shape/c_exper_2.shp")

shapefile_sf=st_as_sf(shapefile)
st_crs(shapefile_sf)=4326  
shapefile=st_transform(shapefile_sf, crs = "EPSG:32618")

# Ensure the CRS of the shapefile matches the LAS data
las_crs <- st_crs(catalog)

if (st_crs(shapefile) != las_crs) {
  shapefile <- st_transform(shapefile, las_crs)
}


# Define buffer distance (in meters)
buffer_distance <- 8

# Calculate the centroids of each polygon
centroids <- st_centroid(shapefile)

# Apply the buffer to each polygon in the shapefile
shapefile_buffered <- st_buffer(centroids, dist = buffer_distance)


# Set chunk size and options for processing the catalog
# Set the number of cores for parallel processing
#opt_chunk_size(catalog) <- 500  # Adjust chunk size as necessary
check_for_lax(dir, n_cores = 30) 

max_height_threshold <-1.25
process_chm_per_polygon <- function(cluster, shape, output_dir) {
  # Extract the LAS object from the LAScluster
  las <- readLAS(cluster)
  
  if (lidR::is.empty(las)) {
    return(NULL)
  }
  
  # Initialize a list to store CHM results
  chm_list <- list()
  
  # Loop through each polygon in the shapefile
  for (i in 1:nrow(shape)) {
    polygon <- shape[i, ]
    las_clipped <- clip_roi(las, polygon)
    
    if (!is.empty(las_clipped)) {
      las_clipped <- classify_ground(las_clipped, algorithm = csf()) 
      dem <- rasterize_terrain(las_clipped, res = 1, algorithm = knnidw(k = 10L, p = 2))
      csm <- rasterize_canopy(las_clipped, res = 1, algorithm = pitfree())
      
      if (!is.null(dem) & !is.null(csm)) {
        chm <- csm - dem
        
        chm[chm > max_height_threshold] <- NA
        
        # Save the result as a raster file
        output_file <- file.path(output_dir, paste0("chm_polygon_", i, ".tif"))
        writeRaster(chm, output_file, overwrite = TRUE)
        
        
      }
    }
  }
  
  return(chm_list)
} 

result <- catalog_apply(catalog, process_chm_per_polygon, shape = shapefile, output_dir = "L:/OUTPUT_LIDAR_all/LIDAR_7")
