\name{update.divdata}
\alias{update.divdata}
\title{ function to download, format and archive climate division data sets }
\description{
Connects to NOAA's website and downloads a current raw climate division data set; formats data as an array.  For most purposes, update.divdata() is recommended. update.divdata() archives the climate division data sets as R data sets, obtainable via a call to the data() statement in R (eg data(divdat.pdsi), data(divdat.tmp), etc.)
     
     The new data set is either saved as an updated R data set in the climdiv library, callable via /code{data()}, or else returned in a list.
	}
\usage{
update.divdata(type = c("pdsi", "phdi", "pmdi", "zndx", "pcp", "tmp"), update = TRUE)
}
\arguments{
  \item{type}{ the type of data to download. one or more of c("pdsi", "phdi", "pmdi", "zndx", "pcp", "tmp") }
  \item{update}{ true or false }
}
\details{

     \code{update.divdata} has two modes.  If update = \code{FALSE}, then only one data set can be specified at a time for downloading, and the value returned is a list named divdata with two components:
     
     type:  describes the type of data (one of pdsi, phdi, etc.) returned

     data:  an array with year, month, and data for each climate division in columns (see format description below)

     If \code{update} = \code{TRUE} (the default), then one or more of the listed data sets can be specified (the default is all six).  The specified data sets are then downloaded, formated, and saved in the climdiv library.  They are accessible via a call to data().  No value is returned by the function \code{update.divdata}.
     
     Data sets available for download are
     
     pdsi   Palmer Drought Severity Index by Climate Division
     phdi   Palmer Hydrological Drought Index by Climate Division
     pmdi   Modified Palmer Drought Severity Index by Climate Division
     zndx   Palmer Z Index by Climate Division
     pcp    Precipitation by Climate Division
     tmp    Temperature by Climate Division
     
     Data are monthly from 1895 to the most recent month.  
}
\value{

     Returns a data array containing the current pdsi data set in a
     convenient array, of the form
     
     year month ST01 ST02 ST03  .  .  .
	
     19XX  1       

	19XX  2

	19XX  3

       .   .

	  .   .

	  .   .
     
     where the first column is the year, the second is the month, and each remaining column contains data for a single climate division.

     no value is returned when update = \code{TRUE}.  The downloaded data set is archived in the \code{climdiv} library, accessible via \code{data()}.

}
\references{ 
     for more information on this data set, see
     
     http://lwf.ncdc.noaa.gov/oa/climate/onlineprod/drought/ftppage.html

     this is also usefull for checking to see if the filename has been changed, etc.
}
\author{ 
     Anthony L. Westerling
     
     http://tenaya.ucsd.edu/~westerli/westerling.html
}
\note{ ~~further notes~~ }

\section{Warning }{
     The first five years or so of the drought indices should not be used, 
     since the Palmer is autoregressive.  

     This function is only going to work on a computer connected to the internet....
}
\examples{

#updates all six data sets and saves them in the climdiv library
#    update.divdata()
# 	data(divdat.pdsi)

#retrieves one data set and returns it as a list
#    divdat.pdsi=update.divdata('pdsi',update=F)
}	
\keyword{ data }
\keyword{ datasets }
