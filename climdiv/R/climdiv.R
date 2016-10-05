.packageName <- "climdiv"
filepath=.find.package("climdiv")

.First.lib<-function(lib=filepath,pkg=.packageName){
    require(maptools)
    require(splancs)
    data("climdiv.poly",package="climdiv")
    data("div.code",package="climdiv")  
    }

.First.lib(filepath,.packageName)


"assign.poly.color" <-
function(name,color,plist){

    
    plist.id=attributes(plist)$region.id
    n=length(plist.id)
    m=length(name)
    
    plist.color=array(NA,n)
    for(i in 1:m){
	j=grep(name[i],plist.id)
	plist.color[j]=color[i]
    }
    plist.color
}

"update.divdata" <-
function(
    type=c("pdsi","phdi","pmdi","zndx","pcp","tmp"),
    update=TRUE){

    if(as.logical((length(type)!=1)*(update==FALSE))) {

	cat('Please set type = one of "pdsi" "phdi" "pmdi" "zndx" "pcp" "tmp"','\n')

    } else {

	for( t in 1:length(type)){

	    infile=paste("http://www1.ncdc.noaa.gov/pub/data/cirs/drd964x.",type[t],".txt",sep="")
	    outfile=paste("divdat.",type[t],".rda",sep="")
	    
	    cat("infile ",infile,'\n')
	    cat("outfile", outfile,'\n')

#READ IN FIXED WIDTH PDSI DATA FILE
	    temp.connect=file(descr=infile,open="rt")

	    cat("connected to ",temp.connect,'\n')

	    data=read.fwf(file=temp.connect,
		header=FALSE,as.is=TRUE,
		widths=c(2,2,1,5,7,7,7,7,7,7,7,7,7,7,7,7))

	    cat("data read in successfully",'\n')

#NAME COLUMNS
	    attributes(data)[[1]]=c("StateCode","District","DataType","Year","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

#GET FINAL YEAR
	    YYYY=data[dim(data)[1],"Year"]


	    outdata=array(NA,c(12*(YYYY-1894),2+dim(data)[1]/(YYYY-1894)))

	    for (year in 1:(YYYY-1894)){
		outdata[(year-1)*12+(1:12),1]=rep(year+1894,12)
	    }

	    outdata[,2]=sequence(rep(12,(YYYY-1894)))
	    
	    for (div in 1:(dim(data)[1]/(YYYY-1894))){

		outdata[,div+2]=
		as.numeric(unlist(t(data[(div-1)*(YYYY-1894)+(1895:YYYY)-1894,4+1:12])))
	    }
	    cat("data formatted for output",'\n')

	    StateCode=
	    rbind(
		c('01','AL'),
		c('02','AZ'),
		c('03','AR'),
		c('04','CA'),
		c('05','CO'),
		c('06','CT'),
		c('07','DE'),
		c('08','FL'),
		c('09','GA'),
		c('10','ID'),
		c('11','IL'),
		c('12','IN'),
		c('13','IA'),
		c('14','KS'),
		c('15','KY'),
		c('16','LA'),
		c('17','ME'),
		c('18','MD'),
		c('19','MA'),
		c('20','MI'),
		c('21','MN'),
		c('22','MS'),
		c('23','MO'),
		c('24','MT'),
		c('25','NE'),
		c('26','NV'),
		c('27','NH'),
		c('28','NJ'),
		c('29','NM'),
		c('30','NY'),
		c('31','NC'),
		c('32','ND'),
		c('33','OH'),
		c('34','OK'),
		c('35','OR'),
		c('36','PA'),
		c('37','RI'),
		c('38','SC'),
		c('39','SD'),
		c('40','TN'),
		c('41','TX'),
		c('42','UT'),
		c('43','VT'),
		c('44','VA'),
		c('45','WA'),
		c('46','WV'),
		c('47','WI'),
		c('48','WY'))
	    
	    
	    temp2=matrix(as.character(unlist(data[seq(1,dim(data)[1],(YYYY-1894)),1:2])),nrow=344)
	    temp2[,1]=ifelse(as.numeric(temp2[,1])<10,paste('0',temp2[,1],sep=''),temp2[,1])
	    temp2[,2]=ifelse(as.numeric(temp2[,2])<10,paste('0',temp2[,2],sep=''),temp2[,2])
	    
	    temp.code=merge(temp2,StateCode,by.x=1,by.y=1)    
	    
	    DivNames=paste(temp.code[,3],temp.code[,2],sep='')
	    
	    colnames(outdata)=c("year","month",DivNames)
	    close(temp.connect)
	    
	    cat("connection closed",'\n')
	    cat("update = ",update,'\n')
	    if (update) {
		
		cat("updating",'\n')
		
		if(type[t]=="pdsi") {
		    divdat.pdsi=outdata
		    save(divdat.pdsi,file=paste(filepath,"/data/",outfile,sep=""))
		}
		    
		if(type[t]=="phdi") {
		    divdat.phdi=outdata
		    save(divdat.phdi,file=paste(filepath,"/data/",outfile,sep=""))
		}

		if(type[t]=="pmdi") {
		    divdat.pmdi=outdata
		    save(divdat.pmdi,file=paste(filepath,"/data/",outfile,sep=""))
		}
    
		if(type[t]=="zndx") {
		    divdat.zndx=outdata
		    save(divdat.zndx,file=paste(filepath,"/data/",outfile,sep=""))
		}

		if(type[t]=="pcp") {
		    divdat.pcp=outdata
		    save(divdat.pcp,file=paste(filepath,"/data/",outfile,sep=""))
		}
    
		if(type[t]=="tmp") {
		    divdat.tmp=outdata
		    save(divdat.tmp,file=paste(filepath,"/data/",outfile,sep=""))
		}
		
		divdata=list(type=type,path=paste(filepath,"/data/",sep=""))

	    } else {
		cat("returning values",'\n')
		divdata=list(type=type,data=outdata)
	    }
	    
	}
	
	divdata
	
    }
    
        
}

"in.poly" <-
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

"in.climdiv" <-
function(pts,years,months,leads=0,polys=climdiv.poly,divdata){

    Z=in.poly(pts,polys)
    Z=cbind(Z,array(NA,c(dim(Z)[1],1+leads)))
    div=div.code[as.numeric(Z[,3])]
    for(i in 1:dim(Z)[1]){
	if(!is.na(div[i])){
		Z[i,4:(4+leads)]=divdata[(years[i]-1895)*12+months[i]-(0:leads),div[i]]
	    } else {
		Z[i,4:(4+leads)]=rep(NA,leads+1)
	    }
	    
	}
	colnames(Z)=c("lon","lat","poly",paste(rep("lead",leads+1),as.character(0:leads),sep=""))
	list(pts=Z[,1:2],year=years,month=months,poly=Z[,3],div=div,divdata=Z[,4:(4+leads)])
    }

"plot.divdata" <-
function(values,names=NULL,zlim=NULL,symmetric=TRUE,
    pal=col.coolhot20,polys=climdiv.poly,add=FALSE,
    xlim=c(-125,-102),ylim=c(32,49),
    fig=c(.05,.95,0.1,1),fig.legend=c(.2,.9,.05,.15),rnd.legend=0,
    bty='n',axes='n',states=NULL,col.states='black',lwd.states=1.5,
    border=NA,lwd.border=.5,legend=TRUE){
    
    index=values
    if(!is.null(zlim)){
	if(-zlim[1]==zlim[2]){symmetric=TRUE} else {symmetric=FALSE}
    } else {
	if(symmetric){
	    z2=(max(abs(index[!is.na(index)])))
	    zlim=c(-z2,z2)
	} else {
	    z2=(max(index[!is.na(index)]))
	    z1=(min(index[!is.na(index)]))
	    zlim=c(z1,z2)
	}
    }

    even=((length(pal)/2)==floor(length(pal)/2))
    
    if(symmetric){
	if(even){
	    index[index==0]=rnorm(sum(index==0),mean=0,sd=.0001)
	    dims=length(pal)/2
	    i=sign(index)*ceiling(dims*abs(index)/zlim[2])+ifelse(sign(index)<0,dims+1,dims)
	    col=pal[i]
	} else {
	    dims=(length(pal)-1)/2
	    i=round(dims*index/zlim[2])+(dims+1)
	    col=pal[i]
	} 
    } else {
	dims=length(pal)-1
	i=round(dims*(index-zlim[1])/(zlim[2]-zlim[1]))+1
	col=pal[i]
    }
    
    error=FALSE
    
    if(!is.null(names)){
	pcol=assign.poly.color(names,col,polys)
    } else {
	if(length(col)==length(polys)){	    
	    pcol=col
	} else {
	    cat ("error : need to specify polygon names or else need
		to provide",length(polys),"values (one for each polygon in 
		    the polygon list)")
	error=TRUE
	}
	
    }
  
    if(!error){

	if(!add){par(fig=fig,mar=c(0,0,0,0))}
	plot(polys,border=NA,col=pcol,add=add,xlim=xlim,ylim=ylim,bty='n',xaxt='n',yaxt='n')
	if(!is.na(border)){
	    for (j in 1:length(polys)){
		polygon(polys[[j]],border=border,lwd=lwd.border)
	    }
	}
    
	if(!is.null(states)){
	    library(maps)
	    if(is.na(as.logical(states))){
		map('state',region=states,add=TRUE,col=col.states,lwd=lwd.states)
	    } else {
	        map('state',add=TRUE,col=col.states,lwd=lwd.states)
	    }	    
	}
	if(!add){
	    par(fig=fig,new=TRUE)
	    plot(polys,border=NA,col=NA,xlim=xlim,ylim=ylim,bty=bty,xaxt=axes,yaxt=axes)
	}
	if(legend){
	    par(fig=fig.legend,new=TRUE,mar=c(1, 0, 0, 0) + 0.1)
	    plot(x=1:length(pal),y=rep(1,length(pal)),type='n',bty='n',xaxt='n',yaxt='n',bty='n')
	    image(x=1:length(pal),y=0:1,z=matrix(c(1:length(pal)),nrow=length(pal)),col=pal,add=TRUE)
	    at=quantile(c(1,length(pal)))
	    lab=as.character(round(quantile(zlim),rnd.legend))
	    axis(side=1,at=at,labels=lab)
	}
    
    }

}

