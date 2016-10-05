\name{western.divs}
\alias{western.divs}
\docType{data}
\title{ character vector }
\description{
  subset of state climate division identifier codes for 11 western states in the contiguous United States.
}
\usage{data(western.divs)}
\format{
  The format is:
 chr [1:110] "AZ01" "AZ02" "AZ03" "AZ04" "AZ05" "AZ06" "AZ07" "CA01" ...
}
\details{
 useful for matching character strings to plot values for the region
     
 these codes are a subset of the climate division IDs in attributes(climdiv.poly)$region.id

}
\seealso{
  \code{\link{western.ST}}, \code{\link{western.states}}, \code{\link{div.code}}, \code{\link{climdiv.poly}}
}
\author{
  Anthony L. Westerling
}
\examples{
data(western.divs)
}
\keyword{datasets}
