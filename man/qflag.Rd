\name{qflag}
\alias{qflag}
\title{Q methodology: automatic flagging of Q-sorts}
\description{Applies alternative algorithms to pre-flag Q-sorts automatically, for posterior calculation of the statement scores.}
\usage{
  qflag(
    loa,
    nstat,
    threshold = "0.05",
    allow.confounded = FALSE
  )
}

\arguments{
  \item{loa}{a Q-sort factor loading matrix obtained, for example from\cr \code{unclass(\link[psych]{principal}(...)$loadings)}, or from \code{\link{qmethod}(...)$loa}.}
  \item{nstat}{number of statements in the study.}
  \item{threshold}{
    Character string to identify one of several criteria to flag Q-sorts:
    \enumerate{
      \item{\code{"0.05"} (default) for Q-sorts which factor loading is higher than the threshold for p-value < 0.05, as per van Exel et al (2001: 388)}
      \item{\code{"0.01"} for Q-sorts which factor loading is higher than the threshold for p-value < 0.01}
      \item{\code{"none"} in which case all Q-sorts will be preliminarily flagged on all factors, but confounded Q-sorts may still be disallowed as per \code{allow.confounded}.}
    }
  }
  \item{allow.confounded}{
    Logical.
    Defaults to \code{FALSE}, in which case for confounded Q-sorts flagged for \emph{more than one factor}, only the factor with the highest square loading for that Q-sort will be flagged, as per van Exel et al (2001: 388).
    If \code{TRUE}, confounded Q-sorts will be allowed.
  }
}

\note{
  To disable all flagging, run with \code{threshold = "none"} and \code{allow.confounded = TRUE}.

  See package \href{https://github.com/aiorazabala/qmethod/wiki/Advanced-analysis}{wiki} for manual flagging.
}

\value{
  Returns a logical matrix with Q-sorts as rows, and factors as columns.
}

\references{Brown, S. R., 1980 \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press.

Van Exel, J., de Graaf, G., Rietveld, P., 2011. "'I can do perfectly well without a car!'" \emph{Transportation} 38, 383-407 (Page 388, footnote 8).

See further references on the methodology in \code{\link{qmethod-package}}.}

\note{This is a function used within \code{\link{qmethod}}. Rarely to be used independently.}

\author{Aiora Zabala, Maximilian Held}

\examples{
data(lipset)
library(psych)
loa <- as.data.frame(unclass(principal(lipset[[1]], nfactors = 3,
                                       rotate = "varimax")$loadings))
flagged <- qflag(loa = loa, nstat = nrow(lipset[[1]]))
summary(flagged)
}
