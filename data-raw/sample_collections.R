## code to prepare `sample_collections` dataset goes here
collection = getArthroCollections(getToken(),2019,2023, 'mosquito', 55)
sample_collections = collection[c(1,3,7,8,9,10,11,17,19,23,28)]
sample_collections$collection_id = sample_collections$collection_id-30050
sample_collections$surv_year = sample_collections$surv_year-3
sample_collections$collection_date = as.Date(ymd_hms(sample_collections$collection_date)-years(3))
sample_collections = na.omit(sample_collections)
#freq_t = data.frame(table(sample_collections$species_display_name)/dim(sample_collections)[1])
#sample_collections$species_display_name = sample(freq_t$Var1,size=dim(sample_collections)[1],replace = T, prob=freq_t$Freq)

write.csv(sample_collections,"data-raw/sample_collections.csv",append = F)

sample_collections = read.csv("data-raw/sample_collections.csv", header = TRUE)
sample_collections=sample_collections[-c(1)]
usethis::use_data(sample_collections, overwrite = TRUE)



