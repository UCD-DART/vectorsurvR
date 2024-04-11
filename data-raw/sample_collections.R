## code to prepare `sample_collections` dataset goes here

collection = getArthroCollections(getToken(),2020,2023)
sample_collections = collection[c(1,3,7,9,10,11,17,19,23,28)]
sample_collections$collection_id = sample_collections$collection_id-30050
sample_collections$surv_year = sample_collections$surv_year-3
write.csv(sample_collections,"data-raw/sample_collections.csv")

sample_collections = read.csv("data-raw/sample_collections.csv", header = TRUE)
sample_collections=sample_collections[-c(1)]
usethis::use_data(sample_collections, overwrite = TRUE)



