

getAgency <- function(token) {
  if (is.null(token) || !is.character(token)) {
    stop("Invalid token. Check username and password")
  }

  # Initialize empty results
  agency <- data.frame()
  page <- 1

  # Paginated API requests
  while(TRUE) {
    # Build request with httr2
    req <- request("https://api.vectorsurv.org/v1/agency") %>%
      req_headers(
        Authorization = paste("Bearer", token),
        "Content-Type" = "application/json"
      ) %>%
      req_url_query(
        pageSize = "1000",
        page = as.character(page),
        `populate[]` = "shape",

      )

    # Execute request and process with jsonlite
    tryCatch({
      response <- req_perform(req)
      content <- resp_body_string(response)
      df_content <- fromJSON(content, flatten = TRUE)

      if (length(df_content$rows) <= 0) break
      agency <- bind_rows(agency, df_content$rows)
      page <- page + 1
    }, error = function(e) {
      stop("API request failed: ", e$message)
    })
  }

  return(agency)
}
