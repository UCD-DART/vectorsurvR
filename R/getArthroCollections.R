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
#' @importFrom dplyr bind_rows select
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

    # separate collection location coordinates
    collections$collection_longitude <- do.call(rbind, lapply(collections$location_shape_coordinates, function(x) unlist(x)))[,1]
    collections$collection_latitude <- do.call(rbind, lapply(collections$location_shape_coordinates, function(x) unlist(x)))[,2]

    colnames(collections) =  str_replace(colnames(collections), "arthropods_","")%>%
      str_replace_all(pattern = "\\.",replacement = "_")
    colnames(collections)[1] = 'collection_id'

    #remove unwanted/redundant columns
    collections = collections %>%
      select(collection_id,collection_num, collection_date,
             agency_id, agency_code, agency_name, surv_year,
             comments,identified_by,species_display_name,
             sex_name,sex_type,trap_acronym, num_trap,
             trap_nights,trap_problem_bit,num_count,
             site_id, site_code, site_name,collection_longitude,collection_latitude,add_date,
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

    collections = collections %>%
      select(collection_id,collection_num, collection_date_start,collection_date_end,
             agency_id, agency_code, agency_name, surv_year,
             comments,identified_by,species_display_name,
             sex_name,sex_type,trap_acronym,bloodfed, attached,type, num_count,trap_problem_bit,sample_method_name,sample_method_value,host,humidity,wind_speed,temperature,conditions_moisture,conditions_sunlight,
             site_id, site_code, site_name,collection_longitude,,collection_latitudeadd_date,
             deactive_date, updated)

    return(collections)
  }

}

