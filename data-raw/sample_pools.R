## code to prepare `sample_pools` dataset
library(lubridate)
library(dplyr)
set.seed(42)
pools = getPools(getToken(),2018,2024,'mosquito')
sample_pools = pools[c("agency_code","id","surv_year","collection_date","site_code" ,"species_display_name" ,"sex_type","trap_acronym" ,"num_count", "test_target_acronym","test_method_name" ,"test_status_name")]
sample_pools$site_code = abs(as.numeric(sample_pools$site_code)-sample.int(50, 1))
sample_pools$id = abs(sample_pools$id-sample.int(100000, 1))
sample_pools$surv_year= sample_pools$surv_year-3
sample_pools$collection_date = as.Date(ymd_hms(sample_pools$collection_date)-years(3))
sample_pools$agency_code = "AGENCY"
sample_pools$agency_id = 01
sample_pools=sample_pools[!is.na(sample_pools$species_display_name),]
# Read and transform shapefile
shape_data <- st_read("C:/Users/Christina/Desktop/Zone_shape/5126zbRtT0j5Hpe.shp")
shape_data <- st_transform(shape_data, crs = 4326)

# Split shape_data into zones
zones <- split(shape_data, shape_data$name)  # Adjust column name if needed

num_samples <- nrow(sample_pools)  # Ensure correct count
samples_per_zone <- ceiling(num_samples / length(zones))  # Ensure we generate enough points

# Function to generate reproducible points inside a polygon
generate_points_in_zone <- function(zone, num_points) {
  bbox <- st_bbox(zone)
  points <- data.frame()

  while(nrow(points) < num_points) {
    # Generate more points than needed (buffer)
    extra_points <- num_points * 1.1

    random_lons <- runif(extra_points, bbox["xmin"], bbox["xmax"])
    random_lats <- runif(extra_points, bbox["ymin"], bbox["ymax"])

    random_points <- st_as_sf(data.frame(lon = random_lons, lat = random_lats),
                              coords = c("lon", "lat"), crs = 4326)

    # Keep only valid points inside the zone
    valid_points <- random_points[st_within(random_points, zone, sparse = FALSE), ]

    # Append only the needed number of points
    points <- rbind(points, valid_points[1:min(num_points - nrow(points), nrow(valid_points)), ])
  }

  return(points[1:num_points, ])  # Return exactly the requested number of points
}

# Generate points per zone and check counts
zone_points <- lapply(names(zones), function(zone_name) {
  set.seed(42)  # Keep consistent per zone
  points <- generate_points_in_zone(zones[[zone_name]], samples_per_zone)
  points$zone <- zone_name
  return(points)
})

# Combine all points and ensure we match sample_pools row count
all_sample_points <- do.call(rbind, zone_points)[1:num_samples, ]

# Final check: Ensure row count matches
if(nrow(all_sample_points) != nrow(sample_pools)) {
  stop("Error: Mismatch in row counts! Check spatial sampling.")
}

# Extract coordinates from sf object
all_sample_points <- all_sample_points %>%
  mutate(longitude = st_coordinates(.)[, 1],
         latitude = st_coordinates(.)[, 2])

# Assign to sample_collections
sample_pools$pool_longitude = all_sample_points$longitude
sample_pools$pool_latitude = all_sample_points$latitude
# Filter and sample data
set.seed(42)  # Reproducibility for sampling



sample_pools %>%
  group_by(surv_year) %>% sample_n(500)->sample_pools

write.csv(sample_pools,"data-raw/sample_pools.csv")

usethis::use_data(sample_pools, overwrite = TRUE)

