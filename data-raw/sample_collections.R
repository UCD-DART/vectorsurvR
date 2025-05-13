library(lubridate)
library(dplyr)
library(sf)
library(sp)

set.seed(40)  # Global seed for reproducibility

# Fetch collections
collection = getArthroCollections(getToken(), 2018, 2024, 'mosquito')
sample_collections = collection[c("agency_code", "collection_id", "collection_date",
                                  "surv_year", "species_display_name", "sex_type",
                                  "trap_acronym", "trap_problem_bit", "num_trap",
                                  "trap_nights", "num_count", "site_code")]

sample_collections$collection_id = abs(sample_collections$collection_id - sample.int(1000, 1))
sample_collections$surv_year = sample_collections$surv_year - 3
sample_collections$collection_date = as.Date(ymd_hms(sample_collections$collection_date) - years(3))
sample_collections$site_code = abs(as.numeric(sample_collections$site_code) - sample.int(1000, 1))
sample_collections$agency_code =  "AGENCY"
sample_collections$agency_id =  01
sample_collections$county = "County 1"

sample_collections = sample_collections[!is.na(sample_collections$species_display_name),]

# Read and transform shapefile
shape_data <- st_read("C:/Users/Christina/Desktop/Zone_shape/5126zbRtT0j5Hpe.shp")
shape_data <- st_transform(shape_data, crs = 4326)

# Split shape_data into zones
zones <- split(shape_data, shape_data$name)  # Adjust column name if needed

num_samples <- nrow(sample_collections)  # Ensure correct count
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
  set.seed(40)  # Keep consistent per zone
  points <- generate_points_in_zone(zones[[zone_name]], samples_per_zone)
  points$zone <- zone_name
  return(points)
})

# Combine all points and ensure we match sample_collections row count
all_sample_points <- do.call(rbind, zone_points)[1:num_samples, ]

# Final check: Ensure row count matches
if(nrow(all_sample_points) != nrow(sample_collections)) {
  stop("Error: Mismatch in row counts! Check spatial sampling.")
}

# Extract coordinates from sf object
all_sample_points <- all_sample_points %>%
  mutate(longitude = st_coordinates(.)[, 1],
         latitude = st_coordinates(.)[, 2])

# Assign to sample_collections
sample_collections$collection_longitude = all_sample_points$longitude
sample_collections$collection_latitude = all_sample_points$latitude
# Filter and sample data
set.seed(40)  # Reproducibility for sampling
sample_collections <- sample_collections %>%
  group_by(surv_year, month(collection_date),.drop = TRUE) %>%
  filter(!species_display_name %in% c("V pensylvanica", "D variabilis", "D occidentalis", "I pacificus", "Dermacentor", "V germanica")) %>%
  sample_n(70,replace = T)
sample_collections = sample_collections[,-c(grep("month",colnames(sample_collections)))]
# Save data
write.csv(sample_collections, "data-raw/sample_collections.csv")
usethis::use_data(sample_collections, overwrite = TRUE)
