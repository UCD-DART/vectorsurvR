#' @title Get arthropod collections data
#' @description
#' `getArthroCollections()` obtains collections data on a year range [start_year, end_year] for authorized VectorSurv Gateway accounts.
#' @param token A valid access token returned from `getToken()`
#' @param start_year Start year of data
#' @param end_year  End year of data
#' @param arthropod Specify arthropod type from: 'mosquito', 'tick'
#' @return A dataframe of collections data specific to users account
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr unnest
#' @importFrom stringr str_replace str_replace_all
#' @export
#' @examples
#' \dontrun{
#' token = getToken()
#' collections = getArthroCollections(token, 2021, 2022, 'mosquito')}

getArthroCollections <- function(token, start_year, end_year, arthropod){
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








  headers <- c(
    Authorization = paste("Bearer", token),
    "Content-Type" = "application/json"
  )

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
        pageSize = "1000",
        page = as.character(i),
        `query[surv_year][$between][0]` = start_year,
        `query[surv_year][$between][1]` = end_year


      )


      # Append the query string to the URL
      url_with_params <- modify_url(url, query = params)

      tryCatch({
        response <- GET(url_with_params, add_headers(headers))
        content <- content(response, as = "text")
        df_content = fromJSON(content, flatten = T)
        if(response$status_code!=200){
          print(content(response, as = "parsed"))
          stop("Error, see response above")
        }
        #Breaks loop when df_content returns no more data
        if(length(df_content$rows)<=0){break}

        collections =  rbind(collections, df_content$rows)

      }, error = function(e) {
        stop(e)
      })

      i=i+1
    }
    #Prevents conflicting data types within $arthropods list
    collections$arthropods=lapply(collections$arthropods, as.data.frame)

    collections =
      collections%>%
      unnest(arthropods, keep_empty = T,names_sep ="_" )

    colnames(collections) =  str_replace(colnames(collections), "arthropods_","")%>%
      str_replace_all(pattern = "\\.",replacement = "_")

    colnames(collections)[1] = 'collection_id'


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
        pageSize = "1000",
        page = as.character(i),
        `query[surv_year][$between][0]` = start_year,
        `query[surv_year][$between][1]` = end_year
      )
      url_with_params <- modify_url(url, query = params)

      tryCatch({
        response <- GET(url_with_params, add_headers(headers))
        content <- content(response, as = "text")
        df_content = fromJSON(content, flatten = T)
        if(response$status_code!=200){
          print(content(response, as = "parsed"))
          stop("Error, see response above")
        }

        #Breaks loop when df_content returns no more data
        if(length(df_content$rows)<=0){break}

        collections =  rbind(collections, df_content$rows)

      }, error = function(e) {
        stop(e)
      })

      i=i+1
    }
    collections$ticks=lapply(collections$ticks, as.data.frame)

    collections =
      collections %>%
      unnest(ticks, keep_empty = T,names_sep = "_" )

    colnames(collections) =  str_replace_all(colnames(collections),
                                             pattern = "\\.",
                                             replacement = "_")
    colnames(collections)[1] = 'collection_id'

    return(collections)
  }

}

