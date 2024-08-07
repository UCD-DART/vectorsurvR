## code to prepare `sample_pools` dataset
library(lubridate)
library(dplyr)
pools = getPools(getToken(),2019,2023,'mosquito', 55)
sample_pools = pools[c(1,6,7,8,9,23,27,37,42,44)]
sample_pools$site = abs(sample_pools$site-sample.int(50, 1))

sample_pools$pool_id = abs(sample_pools$pool_id-sample.int(100000, 1))
sample_pools$surv_year= sample_pools$surv_year-3
sample_pools$collection_date = as.Date(ymd_hms(sample_pools$collection_date)-years(3))
sample_pools=sample_pools[!is.na(sample_pools$species_display_name),]
sample_pools %>% group_by(surv_year) %>% sample_n(500)->sample_pools
write.csv(sample_pools,"data-raw/sample_pools.csv")

sample_pools = read.csv("data-raw/sample_pools.csv")
sample_pools=sample_pools[-c(1)]
usethis::use_data(sample_pools, overwrite = TRUE)
