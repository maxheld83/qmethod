#' @title Q methodology: create Q factor loadings plot
#'
#' @description Plots the loadings of Q-sorts on all factors against all factors.
#'
#' @param results A results object from \code{\link{qmethod}} or a loadings table.
#' @param names Logical, indicating whether rownames (of Q-sorts) from the loadings table should be printed.
#' Should be set to \code{FALSE} when there are very many Q-sorts, or when there are no meaningful names.
#' @param points Logical, indicating whether a point should be plotted at the precise location of a Q-sort.
#' Defaults to \code{FALSE}.
#' Defaults to \code{TRUE}.
#' @param alpha Numerical vector of length one between 0 and 1, indicating transparency of points.
#' Set to below 1 to avoid overplotting.
#' Defaults to \code{1}, in which case points are not transparent.
#' @param density Logical, indicating whether a 2-d density estimate should be added.
#' Helpful for very many Q-sorts with much overplotting.
#' Defaults to \code{FALSE}.
#' @param grid Logical, indicating whether the \emph{combinations} of plots should be returned as a grid or individually, when \code{quietly = FALSE}.
#' Defaults to \code{TRUE}.
#' @param quietly Logical, indicating whether a summary plot should be printed.
#' Defaults to \code{FALSE}.
#' @details During rotation procedures and final interpretation in Q methodology, researchers need to inspect the loadings of participant Q-sorts (as people-variables) on the extracted factors.
#' Instead of a tabular view, this function creates a simple plot.
#' @return Returns a list of lists of \pkg{ggplot2} objects, one for each pair of extracted factors.
#' @author Maximilian Held
#' @note The returned object includes all possible \emph{permutations} of factor pairs.
#' Because the plot of factor A against factor B is the same as factor B against factor A, only with transposed axes, half of these plots are redundant.
#' They are included to allow researchers to conveniently access any given pair of factors.
#'
#' The function also returns a grid of all \emph{non-redundant} factor pairs, that is, the \emph{combinations} of factor pairs.
#'
#' If there are factor names from \code{\link{q.fnames}} or colors from \code{\link{q.fcolors}} in \code{results}, axes will be named and colored accordingly.
#' Remember that factor names and colors may be specific to a particular rotation procedure, and may not be appropriate before rotations are completed.
#' @examples
#' data(lipset)  # use data included with this package
#' results <- qmethod(lipset[[1]], nfactors=3, rotation="varimax")  # run analysis
#' loaplots <- q.loaplot(results = results)  # produces plots list
#' loaplots$f1$f2  # retrieves loaplot of f1 vs f2

q.loaplot <- function(results,
                      names = TRUE,
                      points = FALSE,
                      alpha = 1,
                      density = FALSE,
                      grid = TRUE,
                      #flaglines = FALSE,
                      #nstat,
                      quietly = FALSE) {

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
