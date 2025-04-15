#' Get Pools data
#'
#' Retrieves VectorSurv pools data for desired year range
#' @param token access token retrived from `getToken()`
#' @param start_year  Beginning of year range
#' @param end_year End of year range
#' @param arthropod Specify arthropod type from: 'mosquito', 'tick', 'nontick'
#' @param agency_ids Filter on agency id, default to NULL for all available agencies,otherwise provide a vector of agency ids
#' @keywords pools
#' @return Dataframe of pools data
#' @examples
#' \dontrun{
#' token = getToken()
#' getPools(token, start_year = 2020, end_year = 2021, arthropod = 'tick', 55)}
#' @export




getPools<- function(token, start_year, end_year, arthropod, agency_ids = NULL){

  valid_arthopods = c("tick", "mosquito", "nontick")
  if(!(is.numeric(start_year)) | !(is.numeric(end_year))){
    stop("Incorrect date format, start_year and end_year must be numeric")
  }
  if(end_year<start_year){
    stop("Invalid year range, check parameters")
  }
  if(is.null(token)| !is.character(token)){
    stop("Invalid token. Check username and password")
  }
  if(end_year>year(today())){
    stop("Impossible year range. Check end_year")
  }
  if (any(!(arthropod %in% valid_arthopods))) {
    stop("Invaid arthropod type selected. Choose from: 'mosquito', 'tick', 'nontick'")
  }
  # Handle multiple agency_ids
  if (!is.null(agency_ids) & length(agency_ids) > 1) {
    # If multiple agencies are provided, iterate over them, retrieve data, and merge
    pools_list <- lapply(agency_ids, function(aid) {
      getPools(token, start_year, end_year, arthropod, agency_ids = aid)
    })
    # Merge all the data together into a single dataframe
    merged_pools <- bind_rows(pools_list)
    return(merged_pools)
  }


  url <- "https://api.vectorsurv.org/v1/arthropod/pool"

  headers <- c(
    Authorization = paste("Bearer", token),
    "Content-Type" = "application/json"
  )

  #
  pools = data.frame()
  i=1
  while(i>0){
    params <- list(
       type = arthropod,
      `populate[]` = "agency",
      `populate[]` = "test",
      `populate[]` = "status",
      `populate[]` = "trap",
      `populate[]` = "sex",
      `populate[]` = "lures",
      `populate[]` = "species",
      `populate[]` = "site",
      `populate[]` = "location",


      pageSize = "1000",
      page= as.character(i),
      `query[surv_year][$between][0]` = start_year,
      `query[surv_year][$between][1]` = end_year,
      `query[agency][0]` = agency_ids


    )

    # Create the query string with URL parameters
    query_string <- paste0(
      "&", paste(names(params), "=", unlist(params), collapse = "&",sep="")
    )

    # Append the query string to the URL
    url_with_params <- modify_url(url, query = params)
    tryCatch({
      response <- GET(url_with_params, add_headers(headers))
      content <- content(response, as = "text")
      df_content = fromJSON(content, flatten = T)
      if(response$status_code!=200){

        stop(content(response, as = "parsed"))
      }
      if(length(df_content$rows)<=0){break}
      pools =  rbind(pools, df_content$rows)



    }, error = function(e) {
      stop(e)
    })

    i=i+1
  }

  if(nrow(pools)<=0){
    return(data.frame())
  }

  #Prevents conflicting data types within $test list
  pools$test=lapply(pools$test, as.data.frame)

  pools = pools%>%
    unnest(test, keep_empty = T, names_sep = "_")
  colnames(pools) =  str_replace(colnames(pools), "test_test_","test_")%>%
    str_replace_all(pattern = "\\.",replacement = "_")

  pools$pool_longitude <- do.call(rbind, lapply(pools$location_shape_coordinates, function(x) unlist(x)))[,1]
  pools$pool_latitude <- do.call(rbind, lapply(pools$location_shape_coordinates, function(x) unlist(x)))[,2]


  sites = getSites(token)
  sites_zip = sites[c("id", "city", "postal_code", "region")] #selects the columns with relevant information, this can be changed of course
  regions = getRegions(token)
  colnames(sites_zip)[1] = "site_id" #rename for join function

  pool_site = inner_join(pools, sites_zip, by = 'site_id') #join site information to collections
  regions_county = regions[c("id","parent","type","geoid", "namelsad")] #select id and county name
  colnames(regions_county)[1] = "region" #rename for join function
  pools = inner_join(pool_site, regions_county, by = "region")
  pools=pools %>%
    mutate(namelsad = if_else(!(type %in% c("state","county")),
                              # For geoid > 5, join and update 'namelsad'
                              left_join(., regions %>% select(id, namelsad), by = c("parent" = "id")) %>%
                                mutate(namelsad = coalesce(namelsad.y, namelsad.x)) %>%
                                pull(namelsad),
                              # For geoid <= 5, keep the original 'namelsad'
                              namelsad))


  colnames(pools)[which(names(pools) == "namelsad")] <- "county"
  if(arthropod=="mosquito"){
    pools$lures=lapply(pools$lures, as.data.frame)

    pools =
      pools%>%
      unnest(lures, keep_empty = T,names_sep ="_" )
  pools=pools%>%select(id,pool_num,agency_id,agency_code,agency_name,site_id,site_code,site_name,pool_longitude, pool_longitude,pool_latitude,city,postal_code, county,geoid,collection,comments,
                 surv_year,collection_date,species_display_name,species_full_name,sex_type,sex_name,trap_acronym,trap_name,trap_presence,lures_id, lures_code, lures_description, lures_weight,num_count,test_id,test_value,test_date,
                 test_method_name,test_method_acronym,test_target_acronym,test_target_vector,
                 test_target_icd_10,test_status_name,test_agency_name,test_agency_code,test_agency_state_acronym, add_date ,updated)

  }
  if(arthropod!="mosquito"){
    pools=pools%>%select(pool_id,pool_num,agency_id,agency_code,agency_name,site_id,site_code,site_name,pool_longitude, pool_longitude,pool_latitude,city,postal_code, county,geoid,collection,pool_comments,
                         surv_year,collection_date,species_display_name,species_full_name,sex_type,sex_name,trap_acronym,trap_name,trap_presence,num_count,test_id,value,test_date,
                         method_name,method_acronym,target_acronym,target_vector,
                         target_icd_10,status_name,test_agency_name,test_agency_code,test_agency_state_acronym, add_date ,updated)
  }

  return(pools)

}
