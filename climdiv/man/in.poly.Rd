\name{in.poly}
\alias{in.poly}
\title{ internal function called by in.climdiv }
\description{
  tests geographic coordinates in an array for inclusion within a list of polygons.
}
\usage{
in.poly(pts, polys)
}
\arguments{
  \item{pts}{ Nx2 array of lon and lat values. If there is only one row in \code{pts}, you need to make sure \code{pts} is a matrix with two dimensions.}
  \item{polys}{ a polygon list like that returned by \code{\link[maptools]{Map2poly}} in \code{maptools}.}
}
\details{
  tests geographic coordinates in an array for inclusion within a list of polygons.
}
\value{
  Returns an array with the coordinates and an index number corresponding to the polygon in \code{polys} that contains each coordinate.
}
\references{ in preparation }
\author{ 
Anthony L. Westerling
http://meteora.ucsd.edu/~westerli/westerling.html
}
\note{ 
If you get errors when \code{pts} has only one row, a simple solution is to add a second row with dummy coordinates.
}
\seealso{ \code{\link[splancs]{inout}}, \code{\link[climdiv]{in.climdiv}} }
\examples{

## The function is currently defined as
function(pts,polys){ 
    n.pts=dim(pts)[1] 
    n.ply=length(polys)
    Z=array(NA,c(n.pts,3)) 
    Z[,1:2]=pts
    
    for(i in 1:n.pts){
	j=1
	loop=TRUE 
	while (loop){ 
	    test=inout(t(pts[i,]),polys[[j]]) 
	    if(test){ 
		Z[i,3]=j
		loop=FALSE 
	    } 
	    if(j>=n.ply){loop=FALSE} 
	    j=j+1 
	} 
    } 
    Z 
  }
}
\keyword{ manip }

