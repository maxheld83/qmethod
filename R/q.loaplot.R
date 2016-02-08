q.loaplot <- function(results, quietly = FALSE, names = TRUE, points = FALSE, alpha = 1, density = FALSE, grid = TRUE) {

  # Input verification =========================================================
  if (!is.logical(quietly) || !is.vector(quietly) || length(quietly) != 1) {
    stop("The argument set for quietly must be a logical vector of length 1.")
  }
  if (class(results) != "QmethodRes") {  # then it must be loas
    #stop("The object provided is not of class 'QmethodRes'.")
    results$loas <- results
  }
  #TODO add test for loas

  # Preliminaries
  loaplots <- combs.plots <- NULL

  # protect against matrix input
  results$loa <- as.data.frame(results$loa)

  # Create all possible plots, including redundant ones ========================
  for (v in colnames(results$loa)) {
    for (h in colnames(results$loa)) {
      g <- ggplot(data = results$loa, mapping = aes_q(x = as.name(v), y = as.name(h), label = rownames(results$loa)))  # the assignment of v and h is weird, but otherwise the axes are the wrong way around. Horizontal should be specified first.
      if (names) {
        g <- g + geom_text()
      }
      if (points) {
        g <- g + geom_point(alpha = alpha)
      }
      if (density) {
        g <- g + geom_density_2d()
      }
      g <- g + xlim(-1,1) + ylim(-1,1)  # factor loadings always range from -1 to 1, make sure that plots are comparable
      g <- g + coord_fixed()  # distortions of axes are bad and nonsensical
      if (!is.null(results$brief$fcolors)) {  # if factors have colors
        g <- g + theme(axis.text.x = element_text(colour = results$brief$fcolors[which(colnames(results$loa) == v)]))
        g <- g + theme(axis.text.y = element_text(colour = results$brief$fcolors[which(colnames(results$loa) == h)]))
      }
      if (v != h) { # write results only if they're not the same factor
        loaplots[[v]][[h]] <- g
      }
    }
  }

  if(!quietly) {  # create convenient all-in-one-plot
    # if always in existence, this stuff *COULD* be read in from the rotations.angle in brief, would avoid duplication and decrease risk of inconsistency
    combs <- combn(x = colnames(results$loa), m = 2, simplify = FALSE)
    for (c in combs) {
      name <- paste(c[1], c[2], sep = "-")
      combs.plots[[name]] <- loaplots[[c[1]]][[c[2]]]
    }
    if (grid) {
      do.call(what = "grid.arrange", args = c(combs.plots, ncol = ceiling(sqrt(length(combs)))))
    } else {
      print(combs.plots)
    }
  }
  return(invisible(loaplots))  # return them all
}
