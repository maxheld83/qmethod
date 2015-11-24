setbabel <- function(babel.language) {
  if (!is.null(babel.language)) {
    cat(
      "\\usepackage[",
      babel.language, # for international support
      "]{babel}",
      sep=""
    )
  }
}
