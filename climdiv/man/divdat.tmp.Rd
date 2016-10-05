\name{divdat.tmp}
\alias{divdat.tmp}
\docType{data}
\title{ Monthly Mean Temperature by United States Climate Division}
\description{
  Monthly Mean Temperature for the contiguous United States, by climate division, 1895 to present.
}
\usage{data(divdat.tmp)}
\format{
     year month ST01 ST02 ST03  .  .  .
	
     19XX  1       

	19XX  2

	19XX  3

       .   .

	  .   .

	  .   .
     
where the first column is the year, the second is the month, and each remaining column contains data for a single climate division.

Climate divisions are coded "ST##" where "ST" is the state and "##" is the number of the climate division within the state.
}
\source{
http://lwf.ncdc.noaa.gov/oa/climate/onlineprod/drought/ftppage.html
}
\references{
NCDC, 1994, Time Bias Corrected Divisional Temperature-Precipitation-Drought Index. Documentation for dataset TD-9640. Available from DBMB, NCDC, NOAA, Federal Building, 37 Battery Park Ave. Asheville, NC 28801-2733. 12pp.

Karl, Thomas R., Williams, Claude N., Young, Pamela J., and Wendland, Wayne M. January 1986: A Model to Estimate the Time of Observation Bias Associated With Monthly Mean Maximum, Minimum and Mean Temperatures for the United States, Journal of Climate and Applied Meteorology.

Heddinghause, Thomas R., and Sabol, Paul. 1991:  A Review of the Palmer Drought Severity Index and Where Do We Go From Here, Proceedings of the Seventh Conference on Applied Climatology, pp. 242-246.
}
\examples{
data(divdat.tmp)
}
\keyword{datasets}
