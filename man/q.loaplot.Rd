\name{q.loaplot}
\alias{q.loaplot}
\title{Q methodology: create Q factor loadings plot}
\description{
  Plots the loadings of Q-sorts on all factors against all factors.
}
\usage{
  q.loaplot(
    results,
    quietly = FALSE,
    names = TRUE,
    points = FALSE,
    alpha = 1,
    density = FALSE,
    grid = TRUE
  )
}
\arguments{
  \item{results}{
    A results object from \code{\link{qmethod}} or a loadings table.
  }
  \item{quietly}{
    Logical, indicating whether a summary plot should be printed.
    Defaults to \code{FALSE}.
  }
  \item{names}{
    Logical, indicating whether rownames (of Q-sorts) from the loadings table should be printed.
    Should be set to \code{FALSE} when there are very many Q-sorts, or when there are no meaningful names.
    Defaults to \code{TRUE}.
  }
  \item{points}{
    Logical, indicating whether a point should be plotted at the precise location of a Q-sort.
    Defaults to \code{FALSE}.
  }
  \item{alpha}{
    Numerical vector of length one between 0 and 1, indicating transparency of points.
    Set to below 1 to avoid overplotting.
    Defaults to \code{1}, in which case points are not transparent.
  }
  \item{density}{
    Logical, indicating whether a 2-d density estimate should be added.
    Helpful for very many Q-sorts with much overplotting.
    Defaults to \code{FALSE}.
  }
  \item{grid}{
    Logical, indicating whether the \emph{combinations} of plots should be returned as a grid or individually, when \code{quietly = FALSE}.
    Defaults to \code{TRUE}.
  }
}
\details{
  During rotation procedures and final interpretation in Q methodology, researchers need to inspect the loadings of participant Q-sorts (as people-variables) on the extracted factors.
  Instead of a tabular view, this function creates a simple plot.
}

\value{
  Returns a list of lists of \pkg{ggplot2} objects, one for each pair of extracted factors.
}

\author{Maximilian Held}

\note{
  The returned object includes all possible \emph{permutations} of factor pairs.
  Because the plot of factor A against factor B is the same as factor B against factor A, only with transposed axes, half of these plots are redundant.
  They are included to allow researchers to conveniently access any given pair of factors.

  The function also returns a grid of all \emph{non-redundant} factor pairs, that is, the \emph{combinations} of factor pairs.

  If there are factor names from \code{\link{q.fnames}} or colors from \code{\link{q.fcolors}} in \code{results}, axes will be named and colored accordingly.
  Remember that factor names and colors may be specific to a particular rotation procedure, and may not be appropriate before rotations are completed.
}

\examples{
  data(lipset)  # use data included with this package
  results <- qmethod(lipset[[1]], nfactors=3, rotation="varimax")  # run analysis
  loaplots <- q.loaplot(results = results)  # produces plots list
  loaplots$f1$f2  # retrieves loaplot of f1 vs f2
}
