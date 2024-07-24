## code to prepare `sample_collections` dataset goes here
library(lubridate)
library(dplyr)

collection = getArthroCollections(getToken(),2019,2023, 'mosquito', 55)
sample_collections = collection[c(1,3,7,8,9,10,11,17,19,23,28)]
sample_collections$collection_id = abs(sample_collections$collection_id-sample.int(1000, 1))
sample_collections$surv_year = sample_collections$surv_year-3
sample_collections$collection_date = as.Date(ymd_hms(sample_collections$collection_date)-years(3))
sample_collections$site = abs(sample_collections$site-sample.int(1000, 1))


sample_collections=sample_collections[!is.na(sample_collections$species_display_name),]
sample_collections %>% group_by(surv_year) %>% sample_n(500)->sample_collections

write.csv(sample_collections,"data-raw/sample_collections.csv")

sample_collections = read.csv("data-raw/sample_collections.csv", header = TRUE)
sample_collections=sample_collections[-c(1)]

usethis::use_data(sample_collections, overwrite = TRUE)



