\name{plot.divdata}
\alias{plot.divdata}
\title{ function to plot values by climate division polygons }
\description{
  Plots values by selected division polygons with legend color bar
}
\usage{
plot.divdata(values, names = NULL, zlim = NULL, symmetric = TRUE, pal = col.coolhot20, polys = climdiv.poly, add = FALSE, xlim = c(-125, -102), ylim = c(32, 49), fig = c(0.05, 0.95, 0.1, 1), fig.legend = c(0.2, 0.9, 0.05, 0.15), rnd.legend = 0, bty = "n", axes = "n", states = NULL, col.states = "black", lwd.states = 1.5, border = NA, lwd.border = 0.5, legend = TRUE)
}
\arguments{
  \item{values}{ the index to be plotted.  Must be either a vector whose length is equal to the number of polygons in \code{polys}, or else the \code{names} must be provided.}
  \item{names}{ strings identifying the Climate Divisions to be plotted.  If the entire division code is used, a single division will be plotted. If only state abbreviations are used, will plot by state instead of by division.}
  \item{zlim}{ either \code{NULL} or the maximum and minimum values to plot.  Values outside the range in zlim will not be plotted. If zlim is NULL, zlim will be calculated from the data in \code{values} according to the value of \code{symmetric}. }
  \item{symmetric}{ logical.  If \code{TRUE}, data are plotted on a symmetric scale around zero, with the range equal to \code{-/+ abs(max(values))}. If \code{FALSE}, data are plotted in an asymmetric scale without regard to the origin (unless the data range in \code{values} happens to be symmetric around the origin). }
  \item{pal}{ color palette to use in filling the Climate Division polygons.  A character vector of \code{R}-recognized colors.  The number of colors determines how the data are plotted.  If there is an  even number of colors and \code{symmetric} is \code{TRUE}, there will be no color corresponding to zero... values will all be shaded as positive or negative.  Any zero values will be arbitrarily assigned random \code{sign}.}
  \item{polys}{ the polygon list containing the region borders to be plotted.  This function is designed to be used to \code{plot} data to climate division boundaries in \code{climdiv.poly}, but conceivably other boundaries formatted as polygon lists could be used.}
  \item{add}{ logical.  If \code{TRUE}, the climate division polygons are added to an existing plot in the active graphics device.  If \code{FALSE}, a new plot is started.  The default is \code{FALSE}. }
  \item{xlim}{ length = 2 vector giving the min and max longitude to be plotted. }
  \item{ylim}{ length = 2 vector giving the min and max latitude to be plotted. }
  \item{fig}{ length = 4 vector of the form c(x1,x2,y1,y2), specifying the figure area to be used in plotting the climate division map.  By varying \code{fig} and \code{fig.legend} you can control the placement of the legend color bar. If \code{add} is \code{TRUE}, \code{fig} is ignored, in which case a \code{par(fig=c(x1,x2,y1,y2))} statement might be needed when creating the plot to which the  climate division polygons are to be added, in order to leave room for  the legend color bar. }
  \item{fig.legend}{ length = 4 vector of the form c(x1,x2,y1,y2), specifying the figure area to be  used in plotting the legend. }
  \item{rnd.legend}{ integer.  the level of rounding to use for the labels in the legend. }
  \item{bty}{ the box type around the plotted map. Ignored if adding to an existing figure. }
  \item{axes}{ the type of axes to plot (see \code{par}).  The default is to not plot any axes. }
  \item{states}{ draw boundaries for selected states. Acceptible values are \code{NULL}, \code{TRUE}, or a character vector of lower case state names c("california", "new mexico", etc.).  NULL shuts off drawing of state  level boundaries. TRUE draws all boundaries within the plotted region. Character vector draws the specified state boundaries.  Drawing state boundaries causes \code{plot.divdata} to load the \code{maps} library. }
  \item{col.states}{ the color for the state boundaries. }
  \item{lwd.states}{ the line width for the state boundaries. }
  \item{border}{ draw borders around the climate division polygons.  \code{NA} suppresses borders.  Otherwise, takes a character string with the color to use for the drawing the border. }
  \item{lwd.border}{ the line width for the borders around the climate
     division polygons. Ignored if \code{border} is \code{NA} }
  \item{legend}{ logical.  \code{TRUE} (default) plots a labeled color bar for a legend. }
}
\details{
  \code{plot.divdata} uses fig statements to plot the climate division map and color bar legend in the same figure.  To plot multiple maps in the same figure, for each map use \code{\link[maps]{map}} to plot a base map of state boundaries, then use \code{plot.divdat} to add climate division polygons.   
}
\references{ in preparation }
\author{ 
     Anthony L. Westerling
     
     http://tenaya.ucsd.edu/~westerli/westerling.html }
\note{ ~~further notes~~ }

\section{Warning }{
     The first five years or so of the drought indices should not be used, since the Palmer is autoregressive.  
	} 

\seealso{ \code{\link{update.divdata}}, \code{\link{climdiv.poly}}, \code{\link[maps]{map}} }
\examples{

     data(col.coolhot20)
	data(climdiv.poly)
     values=1:length(climdiv.poly)    
     #western US
     quartz()
     plot.divdata(values,names=NULL,zlim=NULL,symmetric=FALSE,
	 pal=col.coolhot20,polys=climdiv.poly,border="white",add=FALSE,
	 xlim=c(-125,-102),ylim=c(32,49),fig=c(.05,.95,0.2,1),fig.legend=c(.2,.8,.05,.15),
	 rnd.legend=0,bty='n',axes='n',states=TRUE,col.states='black',lwd.states=1.5,lwd.border=.5)
     
     #lower 48
     quartz()
     plot.divdata(values,names=NULL,zlim=NULL,symmetric=FALSE,
	 pal=col.coolhot20,polys=climdiv.poly,border="black",add=FALSE,
	 xlim=c(-125,-65),ylim=c(32,49),fig=c(.05,.95,0.2,1),
	 rnd.legend=0,fig.legend=c(.2,.8,.25,.35),
	 bty='n',axes='n',states=TRUE,col.states='black',lwd.states=1.5,
	 lwd.border=.5)
     
     #adding to map
     western.states=c('california','nevada','arizona',
	 'new mexico','colorado','utah','wyoming','montana',
	 'idaho','washington','oregon')
     library(maps)
     quartz()
     map('state',region=western.states)
     plot.divdata(values,names=NULL,zlim=NULL,symmetric=FALSE,
	 pal=col.coolhot20,polys=climdiv.poly,border=NA,add=TRUE,
	 xlim=c(-125,-102),ylim=c(32,49),fig=c(.05,.95,0.2,1),fig.legend=c(.2,.8,.05,.15),
	 rnd.legend=0,bty='n',axes='n',states=TRUE,lwd.states=2)
     par(new=TRUE,fig=c(.05,.95,0.2,1))
     
     #clipping to 11 western states
     
     western.ST = c("CA","NV","AZ","NM","CO","UT","WY","MT","ID","WA","OR")
     #western.ST shown here as an example.  
     #It is also available via data(western.ST)
     
	data(div.code)
     values[!(substring(div.code,1,2) \%in\% western.ST)]=NA
     
     western.states=c('california','nevada','arizona',
	 'new mexico','colorado','utah','wyoming','montana',
	 'idaho','washington','oregon')
     
     #western.states shown here as an example.  
     #It is also available via data(western.states)
     
     library(maps)
     quartz()
     map('state',region=western.states)
     plot.divdata(values,names=NULL,zlim=NULL,symmetric=FALSE,
	 pal=col.coolhot20,polys=climdiv.poly,add=TRUE,
	 xlim=c(-125,-102),ylim=c(32,49),
	 fig=c(.05,.95,0.2,1),fig.legend=c(.2,.8,.05,.15),
	 rnd.legend=0,bty='n',axes='n',
	 states=western.states,col.states='black',lwd.states=2,
	 border=NA,lwd.border=.5)
     par(fig=c(0,1,0,1))
     title('Western Climate Divisions',line=-3)
     
     
     #matching by divisions
     names=div.code[substring(div.code,1,2) \%in\% western.ST]
     values=values[!is.na(values)]
     
     library(maps)
     quartz()
     map('state',region=western.states)
     plot.divdata(values,names=names,zlim=NULL,symmetric=FALSE,
	 pal=col.coolhot20,polys=climdiv.poly,add=TRUE,
	 xlim=c(-125,-102),ylim=c(32,49),
	 fig=c(.05,.95,0.2,1),fig.legend=c(.2,.8,.05,.15),
	 rnd.legend=0,bty='n',axes='n',
	 states=western.states,col.states='black',lwd.states=2,
	 border=NA,lwd.border=.5)
     par(fig=c(0,1,0,1))
     title('Western Climate Divisions',line=-3)
     
     #using symmetric scale
     values=scale(values)
     
     quartz()
     map('state',region=western.states)
     plot.divdata(values,names=names,zlim=NULL,symmetric=FALSE,
	 pal=col.coolhot20,polys=climdiv.poly,add=TRUE,
	 xlim=c(-125,-102),ylim=c(32,49),
	 fig=c(.05,.95,0.2,1),fig.legend=c(.2,.8,.05,.15),
	 rnd.legend=1,bty='n',axes='n',
	 states=western.states,col.states='black',lwd.states=2,
	 border=NA,lwd.border=.5)
     par(fig=c(0,1,0,1))
     title('Western Climate Divisions',line=-3)
     
     #using symmetric scale centered on white at zero (col.coolhot21 instead of col.coolhot20)
     data(col.coolhot21)
     quartz()
     map('state',region=western.states)
     plot.divdata(values,names=names,zlim=NULL,symmetric=FALSE,
	 pal=col.coolhot21,polys=climdiv.poly,add=TRUE,
	 xlim=c(-125,-102),ylim=c(32,49),
	 fig=c(.05,.95,0.2,1),fig.legend=c(.2,.8,.05,.15),
	 rnd.legend=1,bty='n',axes='n',
	 states=western.states,col.states='black',lwd.states=2,
	 border=NA,lwd.border=.5)
     par(fig=c(0,1,0,1))
     title('Western Climate Divisions',line=-3)

}
\keyword{ hplot }
\keyword{ aplot }
