\name{climdiv.poly}
\alias{climdiv.poly}
\docType{data}
\title{ Polygon List}
\description{
  Polygon List Object containing U.S. Climate Division boundaries.
}
\usage{
  data(climdiv.poly)
  plot(climdiv.poly
}
\details{
  This data set allows you to plot filled polygons for each climate division.
  
  See attributes(climdiv.poly)$region.id for climate division IDs.
}
\source{
  <ftp://nsdi.usgs.gov/pub/dsdl/climate_div.tar.gz> 
  
  This data set was extracted from a shapefile and converted to a polygon object using the maptools library in R.
}
\references{
  http://water.usgs.gov/GIS/metadata/usgswrd/XML/climate_div.xml
  http://lwf.ncdc.noaa.gov/oa/climate/onlineprod/drought/ftppage.html
}
\examples{
  #plot the western climate divisions colored by state
  data(col.coolhot20)
  data(climdiv.poly)
  quartz()
  plot(climdiv.poly,border=FALSE, col=assign.poly.color(c("CA","NV","AZ","NM","CO","UT","WY","MT","ID","WA","OR"), col.coolhot20[5:15],climdiv.poly), xlim=c(-125,-102))
	}
\keyword{datasets}
