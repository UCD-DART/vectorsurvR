## code to prepare `sample_pools` dataset
library(lubridate)
library(dplyr)
pools = getPools(getToken(),2019,2023,'mosquito')
sample_pools = pools[c("agency_code","pool_id","surv_year","collection_date","site_code" ,"species_display_name" ,"sex_type","trap_acronym" ,"num_count", "target_acronym","method_name" ,"status_name")]
sample_pools$site_code = abs(as.numeric(sample_pools$site_code)-sample.int(50, 1))
sample_pools$pool_id = abs(sample_pools$pool_id-sample.int(100000, 1))
sample_pools$surv_year= sample_pools$surv_year-3
sample_pools$collection_date = as.Date(ymd_hms(sample_pools$collection_date)-years(3))
sample_pools$agency_code = ifelse(sample_pools$agency_code==unique(sample_pools$agency_code)[1], "Agency_1", "Agency_2")
sample_pools=sample_pools[!is.na(sample_pools$species_display_name),]
sample_pools %>%
  group_by(surv_year) %>% sample_n(500)->sample_pools

write.csv(sample_pools,"data-raw/sample_pools.csv")

usethis::use_data(sample_pools, overwrite = TRUE)

