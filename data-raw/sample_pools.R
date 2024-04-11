## code to prepare `sample_pools` dataset

pools = getPools(getToken(),2020,2023)
sample_pools = pools[c(1,6,8,9,10,24,28,38,43)]
sample_pools$pool_id = sample_pools$pool_id-30050
sample_pools$surv_year= sample_pools$surv_year-3

write.csv(sample_pools,"data-raw/sample_pools.csv")

sample_pools = read.csv("data-raw/sample_pools.csv")
sample_pools=sample_pools[-c(1)]
usethis::use_data(sample_pools, overwrite = TRUE)
