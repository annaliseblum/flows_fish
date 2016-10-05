\name{in.climdiv}
\alias{in.climdiv}
\title{ Function to Extract U.S. Climate Division Data }
\description{
  Function returns values from a selected Climate Division data set given geographic coordinates and dates.  Matches each geographic coordinate to the Climate Division Polygon it is contained within.  User can specify any practical number of leading months, and values will be returned for them as well.
}
\usage{
in.climdiv(pts, years, months, leads = 0, polys = climdiv.poly, divdata)
}
\arguments{
  \item{pts}{ Nx2 array of lon and lat values. If there is only one row in \code{pts} (ie, you are getting data for only one location), you need to make sure \code{pts} is a matrix with two dimensions. }
  \item{years}{ N years for initial value you want to extract (ie, one year for each geographic coordinate). }
  \item{months}{ N months for initial value you want to extract (ie, one month for each geographic coordinate). }
  \item{leads}{ number of leading months you wish values extracted for (eg, if you specified 7 you would get 8 values, one for the month specified in "months" and the seven months preceeding the month specified in "months"). }
  \item{polys}{ polygon list object.Ê Default is climate division polygons in climdiv.poly. }
  \item{divdata}{ array of division data (divdat.pdsi, divdat.tmp, etc.) formatted by \code{update.divdata}.  }
}
\details{
  This function compares each lat, lon sequentially to the polygon list to identify which perimeter contains the coordinate.  
  
  Points exactly on the boundary between multiple climate divisions are arbitrarily assigned to one of them.
}
\value{
Returns an Nx(L+1) array, where N is the number of geographic coordinates in pts, and L is the number of leading months specified in \code{leads}.
}
\references{ 
in preparation
}
\author{ 
Anthony L. Westerling
http://tenaya.ucsd.edu/~westerli/westerling.html
}
\note{ 
This function requires the splancs library for R to be loaded.
If you get errors when "pts" has only one row, a simple solution is to add a second row of dummy coordinates.
Geographic coordinates which do not lie within any climate division will return NAs.
}

\section{Warning }{Depending on the speed of your computer, this might be slow for matching very large numbers of coordinates.} 

\seealso{ \code{\link[climdiv]{update.divdata}} }
\examples{
data(climdiv.poly)
data(div.code)
data(divdat.pdsi)
lon=c(-118,-117,-116,-115)
lat=c(36,37,38,39)
pts=cbind(t(t(lon)),t(t(lat)))
years=1971:1974
months=rep(8,4)
leads=12
in.climdiv(pts,years,months,leads,divdata=divdat.pdsi)
#returns PDSI values for four arbitrary points and years.  Returns August and 12 preceding months.
}
\keyword{ manip }
\keyword{ array }
