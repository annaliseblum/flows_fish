##functions

##createMAFDC
createMAFDC<-function(x) { #x=dataframe with 2 columns: year and cfs
  asmatrix<-data.frame(split(x[,2],x[1]))
  sortedmatrix<-apply(asmatrix,2,sort) #sort the flows with in each year in increasing order
  MAFDC<-apply(sortedmatrix,1,median) #then take median of each "day-tile" column
  return(MAFDC)
}

#standardize variables
standard<-function(x) { #x=time series of 7day LFs
  stand.var<-(x-mean(x))/sd(x)
  return(stand.var)
}



##7Q10 function

comp7Q10<-function(x) { #x=time series of 7day LFs
  lmomsx<-lmoms(log(x)) #get lmoments of time series of 7day LFs, must be no zeros
  pe3x<-parpe3(lmomsx) #get pearson type 3 parameters from lmoments
  emp7Q10<-exp(quape3(.1,pe3x)) #pull quantile=.1 and exponentiate it back into real space
  return(emp7Q10)
}

#1 - to make residual plots
##like in minitab

#For qqnorm why do you use qqline?
#abline(0,1) #why isn't it just this line??
##HOW TO ADD TO MAIN TITLE THE ORIGINAL LM RESULT NAME??

ResidPlots<-function(x){ #pull out the ratios from the Lmoments list of site lists of flows
  par(mfrow = c(2, 2),mar=c(4,4,3,4)) # c(bottom, left, top, right) 
  
  if (class(x)=="lm") {
    resid<-x$resid
  } else {
    resid<-resid(x)
  }
  #Plot 1 Normal probability Plot
  qqnorm(resid,xlab="Residual",ylab="Percent",main="Normal Probability Plot")
  qqline(resid) 
  grid()
  usr <- par("usr") ##  par( "usr" ) returns a vector containing xleft, xright, ybottom, ytop.
  if(length(resid)<5000){
    text(usr[1], usr[4],bquote("S-W p=" ~.(round(shapiro.test(resid)$p.value,digits=4))),adj=c(-.2, 1.5))
  }
  
  ##plot 2 residual versus Fits
  plot(predict(x),resid,xlab="Fitted Value",ylab="Residual",main="Versus Fits")
  abline(0,0)
  
  ##plot 3 Histogram
  hist(resid,xlab="Residual",ylab="Frequency",main="Histogram")
  
  ##plot 4 ACF
  acf(resid,main="Autocorrelation")
  
  #make default one plot again
  par(mfrow=c(1,1),mar=rep(4,4))  
}


##FROM http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

