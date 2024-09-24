## code to prepare `sample_collections` dataset goes here
library(lubridate)
library(dplyr)

collection = getArthroCollections(getToken(),2019,2023, 'mosquito', 55)
sample_collections = collection[c("collection_id","collection_date"  ,"surv_year","species_display_name","sex_type","trap_acronym" ,"trap_problem_bit","num_trap","trap_nights" ,"num_count","site_code")]
sample_collections$collection_id = abs(sample_collections$collection_id-sample.int(1000, 1))
sample_collections$surv_year = sample_collections$surv_year-3
sample_collections$collection_date = as.Date(ymd_hms(sample_collections$collection_date)-years(3))
sample_collections$site_code = abs(as.numeric(sample_collections$site_code)-sample.int(1000, 1))




sample_collections=sample_collections[!is.na(sample_collections$species_display_name),]

sample_collections %>%  group_by(surv_year) %>% filter(!species_display_name%in%c("V pensylvanica","D variabilis" ,"D occidentalis","I pacificus"))%>% sample_n(500)->sample_collections

write.csv(sample_collections,"data-raw/sample_collections.csv")

sample_collections = read.csv("data-raw/sample_collections.csv", header = TRUE)
sample_collections=sample_collections[-c(1)]

usethis::use_data(sample_collections, overwrite = TRUE)



