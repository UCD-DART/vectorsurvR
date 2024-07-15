#' Get Pools data
#'
#' Retrieves VectorSurv pools data for desired year range
#' @param token access token retrived from `getToken()`
#' @param start_year  Beginning of year range
#' @param end_year End of year range
#' @param arthropod Specify arthropod type from: 'mosquito', 'tick', 'nontick'
#' @param agency_id Filter on agency id, default to NULL for all available agencies, otherwise specify a single agency by code
#' @keywords pools
#' @return Dataframe of pools data
#'
#' @examples
#' \dontrun{
#' token = getToken()
#' getPools(token, start_year = 2020, end_year = 2021, arthropod = 'tick', 55)}
#' @export




getPools<- function(token, start_year, end_year, arthropod, agency_id=NULL){

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
      #type = arthropod,
      `populate[]` = "agency",
      `populate[]` = "test",
      `populate[]` = "status",
      `populate[]` = "trap",
      `populate[]` = "species",
      `populate[]` = "sex",

      pageSize = "1000",
      page= as.character(i),
      `query[surv_year][$between][0]` = start_year,
      `query[surv_year][$between][1]` = end_year,
      `query[agency]` = agency_id


    )

    # Create the query string with URL parameters
    query_string <- paste0(
      "&", paste(names(params), "=", unlist(params), collapse = "&",sep="")
    )

    # Append the query string to the URL
    url_with_params <- modify_url(url, query = params)
    #print(url_with_params)
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
  #Prevents conflicting data types within $test list
  pools$test=lapply(pools$test, as.data.frame)
  pools = pools%>%
    unnest(test, keep_empty = T, names_sep = "_")
  colnames(pools) =  str_replace(colnames(pools), "test_","")%>%
    str_replace_all(pattern = "\\.",replacement = "_")
  colnames(pools)[c(1,5,13)] = c("pool_id","pool_comments","test_id")

  return(pools)

}
