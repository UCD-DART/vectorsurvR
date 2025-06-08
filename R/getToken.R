#' @title Get authentication token
#' @description getToken() returns a token needed to run getArthroCollections() and getPools().
#' Prints agencies associated with account credentials. The function prompts users for a VectorSurv
#' account credentials.
#' @keywords authentication
#' @return User token
#' @importFrom rstudioapi askForPassword
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json
#' @examples
#'  \dontrun{token = getToken()}
#' @export
getToken = function() {

  # Prompt credentials
  username <- askForPassword("Gateway Username")
  password <- askForPassword("Gateway Password")

  # Create and send request
  req <- request("https://api.vectorsurv.org/login") %>%
    req_headers("Content-Type" = "application/json") %>%
    req_body_json(list(
      username = username,
      password = password
    ))

  # Perform request and handle response
  resp <- req_perform(req)
  response_content <- resp_body_json(resp)

  if (is.null(response_content$token)) {
    stop(response_content)
  }

  token <- response_content$token
  agencies <- c()

  for (i in seq_along(response_content$agencies)) {
    agencies <- rbind(agencies, paste(
      "Agency Id:", response_content$agencies[[i]]$id,
      "| Agency Name:", response_content$agencies[[i]]$name,
      sep = " "
    ))
  }

  print(agencies)

  return(token)
}
