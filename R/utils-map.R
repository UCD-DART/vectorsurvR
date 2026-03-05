# R/utils-map.R  (internal, not exported)
.show_map <- function(map, height = 400, delay = 3) {
  if (!requireNamespace("webshot2", quietly = TRUE) ||
      !requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop(
      "Packages 'webshot2' and 'htmlwidgets' are needed when html = FALSE.\n",
      "Install them with: install.packages(c('webshot2', 'htmlwidgets'))\n",
      "Then run: webshot2::install_chromote()",
      call. = FALSE
    )
  }

  tmp_html <- tempfile(fileext = ".html")
  tmp_png  <- tempfile(fileext = ".png")

  htmlwidgets::saveWidget(map, tmp_html, selfcontained = FALSE)
  webshot2::webshot(tmp_html, file = tmp_png, vwidth = 900, vheight = height, delay = 3)
  knitr::include_graphics(tmp_png)
}
