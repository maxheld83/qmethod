\name{qfwe}
\alias{qfwe}
\title{Q methodology: calculate weighted Q-sorts}
\description{Calculates factor weights and weights flagged Q-sorts accordingly.}
\usage{qfwe(dataset, loa, flagged, scoremethod = "Brown1980")}

\arguments{
  \item{dataset}{a matrix or a data frame containing raw data, with statements as rows, Q-sorts as columns, and the column scores in the distribution in each cell.}
  \item{loa}{matrix or data frame of \code{nqsorts} rows and \code{nfactors} columns, with values of factor loadings for Q-sorts, calculated using, e.g., \code{principal(...)$loadings}.}
  \item{flagged}{matrix or data frame of \code{nqsorts} rows and \code{nfactors} columns, with \code{TRUE} values for the Q-sorts that are flagged. Automatic flagging can be aplied using \code{\link{qflag}}.
    Manual flagging can be done by providing a logical matrix with \code{nqsorts} rows and \code{nfactors} columns to the argument \code{flagged}.}
  \item{scoremethod}{a character vector of length 1, indicating the factor scoring method to be used. In each case, resulting loadings are multiplied by raw data, then standardized in \code{\link{qzscores}}.
    Options are:
      \itemize{
        \item \code{"Brown1980"} (default), loadings are weighted as per Q-methodological convention (Brown 1980: 242).
          Raw data is then multiplied by weighted loadings.
        \item \code{"none"}, loadings are passed on unchanged.
      }
  }
}

\references{
  Brown, S. R., 1980 \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press.

  See further references on the methodology in \code{\link{qmethod-package}}.
}

\note{
  This is a function used within \code{\link{qzscores}}.
  Rarely to be used independently.
}

\author{Aiora Zabala}

\examples{
data(lipset)
library(psych)
loa <- as.data.frame(unclass(principal(lipset[[1]], nfactors = 3,
                                       rotate = "varimax")$loadings))
flagged <- qflag(loa = loa, nstat = nrow(lipset[[1]]))
wraw_all <- qfwe(dataset = lipset[[1]], loa = loa, flagged = flagged)
head(wraw_all)
}
