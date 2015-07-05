q.loaplot <- function(results) {

  # Input verification =========================================================
  if (class(results) != "QmethodRes") {  # only accept results object
    stop("The object provided is not of class 'QmethodRes'.")
  }

  # Preliminaries
  loaplots <- combs.plots <- NULL

  # Create all possible plots, including redundant ones ========================
  for (v in colnames(results$loa)) {
    for (h in colnames(results$loa)) {
      g <- ggplot(data = results$loa, mapping = aes_string(x = h, y = v, label = "row.names(results$loa)"))
      g <- g + geom_text()
      g <- g + xlim(-1,1) + ylim(-1,1)  # factor loadings always range from -1 to 1, make sure that plots are comparable
      g <- g + coord_fixed()  # distortions of axes are bad and nonsensical
      if (!is.null(results$brief$fcolors)) {  # if factors have colors
        g <- g + theme(axis.text.x = element_text(colour = results$brief$fcolors[which(colnames(results$loa) == h)]))
        g <- g + theme(axis.text.y = element_text(colour = results$brief$fcolors[which(colnames(results$loa) == v)]))
      }
      if (v != h)  # write results only if they're not the same factor
      loaplots[[v]][[h]] <- g
    }
  }

  # print out only the non-redundant ones (the combinations, not permutations)
  combs <- combn(x = colnames(results$loa), m = 2, simplify = FALSE)
  for (c in combs) {
    name <- paste(c[1], c[2], sep = "-")
    combs.plots[[name]] <- loaplots[[c[1]]][[c[2]]]
  }
  do.call(what = "grid.arrange", args = c(combs.plots, ncol = round(sqrt(length(combs)))))

  return(invisible(loaplots))  # return them all
}