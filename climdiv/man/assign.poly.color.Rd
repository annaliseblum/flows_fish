\name{assign.poly.color}
\alias{assign.poly.color}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Assign Colors for Plotting Polygons in a Polygon List}
\description{
  assigns a color to all polygons with the same string in their region.id attribute
}
\usage{
assign.poly.color(name, color, plist)
}
\arguments{
  \item{name}{ vector of character strings to match to polygon region id attribute }
  \item{color}{ vector of colors }
  \item{plist}{ polylist object to be plotted }
}
\details{
  This function generates a list of colors corresponding to each polygon in a polylist object by matching the string you supply to elements of the region.id attributes of a polylist. Plot (plot.polylist) cannot plot subsets of the polygons in a polylist. This function assigns color = NA for polygons you do not wish to plot. It also allows you to assign a single color easily to all the component polygons of a single entity.  Sets fill colors of polygons not selected to NA, so they are not filled.
}
\value{
  list of colors the same length as the number of polygons in a polylist
}
\references{ http://tenaya.ucsd.edu/~westerli/westerling.html }
\author{ Anthony L. Westerling }

\examples{

#plot the western climate divisions colored by state
data(col.coolhot20)
data(climdiv.poly)
quartz()
plot(climdiv.poly, border=FALSE, col=assign.poly.color(c("CA", "NV", "AZ", "NM", "CO", "UT","WY","MT","ID","WA","OR"), col.coolhot20[5:15],climdiv.poly), xlim=c(-125,-102))
}
\keyword{ color }

