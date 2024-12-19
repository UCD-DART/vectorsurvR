#' @title Get arthropod collections data
#' @description
#' `getArthroCollections()` obtains collections data on a year range [start_year, end_year] for authorized VectorSurv Gateway accounts.
#' @param token A valid access token returned from `getToken()`
#' @param start_year Start year of data
#' @param end_year  End year of data
#' @param arthropod Specify arthropod type from: 'mosquito', 'tick'
#' @param agency_ids Filter on agency id, default to NULL for all available agencies,otherwise provide a vector of agency ids
#' @return A dataframe of collections
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr unnest
#' @importFrom stringr str_replace str_replace_all
#' @importFrom dplyr bind_rows select if_else pull coalesce inner_join left_join
#' @export
#' @examples
#' \dontrun{
#' token = getToken()
#' collections = getArthroCollections(token, 2021, 2022, 'mosquito',55, TRUE)}

getArthroCollections <- function(token, start_year, end_year, arthropod, agency_ids = NULL){



  valid_arthopods = c("tick", "mosquito")

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

  headers <- c(
    Authorization = paste("Bearer", token),
    "Content-Type" = "application/json"
  )

  # Handle multiple agency_ids
  if (!is.null(agency_ids) & length(agency_ids) > 1) {
    # If multiple agencies are provided, iterate over them, retrieve data, and merge
    collections_list <- lapply(agency_ids, function(aid) {
      getArthroCollections(token, start_year, end_year, arthropod, agency_ids = aid)
    })


    # Merge all the data together into a single dataframe
    merged_collections <- bind_rows(collections_list)
    return(merged_collections)
  }

  if(arthropod=='mosquito'){
    url <- "https://api.vectorsurv.org/v1/arthropod/collection"

    #Gathers data matching parameters
    collections=data.frame()
    i = 1
    while(i>0){
      params <- list(

        `populate[]` = "arthropods",
        `populate[]` = "agency",
        `populate[]` = "trap",
        `populate[]` = "lures",
        `populate[]` = "location",
        `populate[]` = "site",

        pageSize = "1000",
        page = as.character(i),
        `query[surv_year][$between][0]` = start_year,
        `query[surv_year][$between][1]` = end_year,
        `query[agency][0]` = agency_ids

      )



      # Append the query string to the URL
      url_with_params <- modify_url(url, query = params)

      tryCatch({
        response <- GET(url_with_params, add_headers(headers))
        content <- content(response, as = "text")
        df_content = fromJSON(content, flatten = T)
        if(response$status_code!=200){
          stop(content(response, 'parsed'))
        }
        #Breaks loop when df_content returns no more data
        if(length(df_content$rows)<=0){break}

        collections =  rbind(collections, df_content$rows)

      }, error = function(e) {
        stop(e)
      })

      i=i+1
    }
    if(nrow(collections)<=0){
      return(data.frame())
    }


    #Prevents conflicting data types within $arthropods list
    collections$arthropods=lapply(collections$arthropods, as.data.frame)
    collections =
      collections%>%
      unnest(arthropods, keep_empty = T,names_sep ="_" )

    collections =
      collections%>%
      unnest(lures, keep_empty = T,names_sep ="_" )

    colnames(collections) =  str_replace(colnames(collections), "arthropods_","")%>%
      str_replace_all(pattern = "\\.",replacement = "_")
    colnames(collections)[1] = 'collection_id'
    collections = collections %>%  filter(!species_display_name%in%c("V pensylvanica","D variabilis" ,"D occidentalis","I pacificus","Dermacentor","V germanica"))

    # separate collection location coordinates
    collections$collection_longitude <- do.call(rbind, lapply(collections$location_shape_coordinates, function(x) unlist(x)))[,1]
    collections$collection_latitude <- do.call(rbind, lapply(collections$location_shape_coordinates, function(x) unlist(x)))[,2]
    #regional

    sites = getSites(token)
    sites_zip = sites[c("id", "city", "postal_code", "region")] #selects the columns with relevant information, this can be changed of course
    regions = getRegions(token)
    colnames(sites_zip)[1] = "site_id" #rename for join function

    col_site = left_join(collections, sites_zip, by = 'site_id') #join site information to collections
    regions_county = regions[c("id","parent","type","geoid", "namelsad")] #select id and county name
    colnames(regions_county)[1] = "region" #rename for join function
    collections = left_join(col_site, regions_county, by = "region")

    collections=collections %>%
      mutate(namelsad = if_else(!(type %in% c("state","county")),
                                # For geoid > 5, join and update 'namelsad'
                                left_join(., regions %>% dplyr::select(id, namelsad), by = c("parent" = "id")) %>%
                                  mutate(namelsad = coalesce(namelsad.y, namelsad.x)) %>%
                                  pull(namelsad),
                                # For geoid <= 5, keep the original 'namelsad'
                                namelsad))


    colnames(collections)[which(names(collections) == "namelsad")] <- "county"



    #remove unwanted/redundant columns
    collections = collections %>%
      select(collection_id,collection_num, collection_date,
             agency_id, agency_code, agency_name, surv_year,
             comments,identified_by,species_display_name,
             sex_name,sex_type,trap_acronym, lures_code, lures_description, lures_weight,num_trap,
             trap_nights,trap_problem_bit,num_count,
             site_id, site_code, site_name,collection_longitude,collection_latitude,city,postal_code, county,geoid, add_date,
             deactive_date, updated)



    return(collections)
  }


  if(arthropod=='tick'){
    url <- "https://api.vectorsurv.org/v1/tick/collection"
    collections=data.frame()
    i = 1
    while(i>0){
      params <- list(
        `populate[]` = "agency",
        `populate[]` = "ticks",
        `populate[]` = "sample_method",
        `populate[]` = "trap",
        `populate[]` = "location",
        `populate[]` = "site",
        pageSize = "1000",
        page = as.character(i),
        `query[surv_year][$between][0]` = start_year,
        `query[surv_year][$between][1]` = end_year,
        `query[agency][0]` = agency_ids

      )
      url_with_params <- modify_url(url, query = params)

      tryCatch({
        response <- GET(url_with_params, add_headers(headers))
        content <- content(response, as = "text")
        df_content = fromJSON(content, flatten = T)
        if(response$status_code!=200){
          stop(content(response, 'parsed'))
        }

        #Breaks loop when df_content returns no more data
        if(length(df_content$rows)<=0){break}

        collections =  rbind(collections, df_content$rows)

      }, error = function(e) {
        stop(e)
      })

      i=i+1
    }
    if(nrow(collections)<=0){
      return(data.frame())
    }
    collections$ticks=lapply(collections$ticks, as.data.frame)

    collections =
      collections %>%
      unnest(ticks, keep_empty = T,names_sep = "_" )

        colnames(collections) =  str_replace(colnames(collections), "ticks_","")%>%
      str_replace_all(pattern = "\\.",replacement = "_")
    collections$collection_longitude <- do.call(rbind, lapply(collections$location_shape_coordinates, function(x) unlist(x)))[,1]
    collections$collection_latitude <- do.call(rbind, lapply(collections$location_shape_coordinates, function(x) unlist(x)))[,2]


    colnames(collections)[1] = 'collection_id'

    sites = getSites(token)
    sites_zip = sites[c("id", "city", "postal_code", "region")] #selects the columns with relevant information, this can be changed of course
    regions = getRegions(token)
    colnames(sites_zip)[1] = "site_id" #rename for join function

    col_site = left_join(collections, sites_zip, by = 'site_id') #join site information to collections
    regions_county = regions[c("id","parent","type","geoid", "namelsad")] #select id and county name
    colnames(regions_county)[1] = "region" #rename for join function
    collections = left_join(col_site, regions_county, by = "region")
    collections=collections %>%
      mutate(namelsad = if_else(!(`type.y` %in% c("state","county")),
                                # For geoid > 5, join and update 'namelsad'
                                left_join( regions %>% select(id, namelsad), by = c("parent" = "id")) %>%
                                  mutate(namelsad = coalesce(namelsad.y, namelsad.x)) %>%
                                  pull(namelsad),
                                # For geoid <= 5, keep the original 'namelsad'
                                namelsad))


    colnames(collections)[which(names(collections) == "namelsad")] <- "county"
    #

    collections = collections %>%
            select(collection_id,collection_num, collection_date_start,collection_date_end,
             agency_id, agency_code, agency_name, surv_year,
             comments,identified_by,species_display_name,
             sex_name,sex_type,trap_acronym,bloodfed, attached, num_count,trap_problem_bit,sample_method_name,sample_method_value,host,humidity,wind_speed,temperature,conditions_moisture,conditions_sunlight,
             site_id, site_code, site_name,collection_longitude,collection_latitude,city,postal_code, county,geoid, add_date,
             deactive_date, updated)

    return(collections)
  }
}

