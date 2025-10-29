#' @title Get sites data
#' @description
#' `getSites()` obtains site data for authorized VectorSurv Gateway accounts.
#' @param token A valid access token returned from `getToken()`
#' @param agency_ids Filter on agency id, default to NULL for all available agencies,otherwise provide a vector of agency ids, such as `agency_ids = c(55,56)`

#' @return A dataframe of site data
#' @export
#' @examples
#' \dontrun{
#' token = getToken()
#' sites = getSites(token)}


getSites <- function(token, agency_ids = NULL) {
  if (is.null(token) || !is.character(token)) {
    stop("Invalid token. Check username and password")
  }

  if (!is.null(agency_ids) && length(agency_ids) > 1) {
    return(bind_rows(lapply(agency_ids, function(aid) {
      getSites(token, agency_ids = aid)
    })))
  }

  # Initialize empty results
  sites <- data.frame()
  page <- 1

  # Paginated API requests
  while(TRUE) {
    # Build and execute request
    req <- request("https://api.vectorsurv.org/v1/site") %>%
      req_headers(
        Authorization = paste("Bearer", token),
        "Content-Type" = "application/json"
      ) %>%
      req_url_query(
        pageSize = "1000",
        page = as.character(page),
        `query[agency][0]` = if (!is.null(agency_ids)) agency_ids else NULL

      )

    tryCatch({
      # Get and parse response
      response <- req_perform(req)
      content <- resp_body_string(response)
      df_content <- fromJSON(content, flatten = TRUE)

      # Break if no more results
      if (length(df_content$rows) <= 0) break

      # Append results
      sites <- bind_rows(sites, df_content$rows)
      page <- page + 1

    }, error = function(e) {
      stop("API request failed: ", e$message)
    })
  }

  return(sites)
}
