setgraphicspath <- function(img.dir) {
  if (!is.null(img.dir)) {
    cat(
      "\\graphicspath{{\"",
      img.dir,
      "/",  # graphicspath needs a trailing slash
      "\"}}",
      sep=""
    )
  }
}
