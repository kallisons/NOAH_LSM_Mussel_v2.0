require(caTools)
require(fields)
require(RNetCDF)

AssemblyInfo<-read.table("/Users/kasmith/Documents/MusselBedStructure/RCode/AssemblyInfo_BedStructure2.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

#CNTCTList<-c(0.05, 0.90)

for(z in 41:nrow(AssemblyInfo)){

site<-AssemblyInfo$Site[z]
point<-sprintf("%1.2f", AssemblyInfo$Point[z])


foldername<-paste("/Volumes/HELIOS/MusselBedStructure/MusselTemperatures/", site, "/", point, "/", sep="")
filenames<-list.files(foldername)

#coefficients of relationship between temperature, exposure time and survival from Allison lab experiment data files
coefs<-matrix(nrow=3,ncol=3)
coefs[,1]<-c(42.15,43.03,43.64) # the intercept of the relationship between probit(survival) and temperature at a fixed time of 30 min
coefs[,2]<-c(-0.93,-0.97,-1.00) # the slope of the above relationship
coefs[,3]<-c(2.56,3.13,4.52) # the relationship between probit(survival) and log10(exposure time) at a fixed temperature
#coefs[,3]<-c(2.56,3.13,4.52) # including 10 min 100% survival values for day 1
#coefs[,3]<-c(3.27,3.13,4.52) # not including 10 min 100% survival values for day 1

for (f in 1:length(filenames)){


frameshift<-0  # shift starting row of data matrix

minsurv<-matrix(nrow=132,ncol=3)  #matrix for the monthly cumulative survival values saved separately for each geographic location
                        # 120 rows for 11 years of 12 months
						# 3 columns for Year, Month, Survival
colnames(minsurv)<-c("year", "month", "survival")
y_index<-(0:131)%/%12   # pointers to years in this matrix
minsurv_dates<-0:131
minsurv_date_units<-"months since 1997-01-15"
dates<-utcal.nc(minsurv_date_units,minsurv_dates)
unix_units<-"seconds since 1970-01-01"
unix_dates<-utinvcal.nc(unix_units,dates) 
posix_dates<-as.POSIXlt(strptime("1970-01-01","%Y-%m-%d")+unix_dates)


print(filenames[f])
a<-read.table(paste(foldername, filenames[f],sep=""),header=T,sep="\t")
a<-subset(a, a$Unix>=852095142)
a$Temp2<-runmean(a$Temp, 5)
a$min<-as.numeric(substr(a$Time,4,5))
a<-subset(a, a$min==a$min[1] | a$min==a$min[1]+30)
a<-a[,c("Unix", "Date", "Time", "Temp2")]
colnames(a)[4]<-"Temp"
numdate<-a[,1]
ldate<-as.POSIXlt(strptime("1970-01-01","%Y-%m-%d",tz="GMT")+numdate)

y<-ldate$year+1900

tsx<-a$Temp 


for (year in 1997:2007){
 ndays<-length(unique(as.POSIXlt(numdate[y==year]+strptime("1970-01-01","%Y-%m-%d",tz="GMT"))$yday))
 cpmatrix<-matrix(nrow=ndays,ncol=16)  #matrix for yearly survival curves, 1 for each time interval tt (there are 16 of these)
 mcpmatrix<-matrix(nrow=12,ncol=16) #matrix for monthly cumulative survival values, 1 for each time interval and 1 for each month
  TSX<-tsx[y==year]
  TSX<-TSX[(1+frameshift):length(TSX)]
  NUMDATE<-numdate[y==year]
  NUMDATE<-NUMDATE[(1+frameshift):length(NUMDATE)]  
  for (tt in 1:16){
  tt10<-tt*30
  lft<-"left"
  ts1<-runmin(x=TSX,k=tt,alg="C",endrule="trim") #get running minima of temperature series for time interval tt
  t_index<-(1:length(ts1))%/%tt #index for new series of time increments of length tt
  tss<-tapply(ts1,t_index,min)
  dsx<-runmin(x=NUMDATE,k=tt,alg="C",endrule="trim") #get running minima of date series for time interval tt
  ds1<-tapply(dsx,t_index,min)
  dss<-as.POSIXct(ds1+strptime("1970-01-01","%Y-%m-%d",tz="GMT")) #convert to posix dates
  
  mday<-as.POSIXlt(dss)$mday+1 #find first day of each month
  month<-as.POSIXlt(dss)$mo+1 #find the months
  
  
  
  #PT<-(coefs[c,1]+coefs[c,2]*tss+coefs[c,3]*log10(30))-coefs[c,3]*log10(tt10)
  PT1<-(coefs[1,1]+coefs[1,2]*tss+coefs[1,3]*log10(30))-coefs[1,3]*log10(tt10)
  PT2<-(coefs[2,1]+coefs[2,2]*tss+coefs[2,3]*log10(30))-coefs[2,3]*log10(tt10)
  PT3<-(coefs[3,1]+coefs[3,2]*tss+coefs[3,3]*log10(30))-coefs[3,3]*log10(tt10)
  #calculate probit of survival from temperature and duration of exposure
  #the "c" value picks the coefs for day 1, day 2, or day 3 exposures from experimental results
  #2.633 is the slope of the relationship between  probit and log10(time) at fixed temperature (single exposure)
  #slope would be 3.62 for 2 days and 4.04 for 3 days based on coeffs matrix from Probit_Statistics_Allison_MarkDenny.R
  #-0.88 is the slope of the relationship between probit at T at fixed time of 30 min and 39.6 is the intercept
  #1 day slope value is -0.92, 2 day value is -0.83, 3 day value is -0.90
  #1 day intercept value is 41.7, 2 day value is 37.3, 3 day value is 39.8
  
  
  
  
   #**Calculating monthly values for the figure**#
  
  for (m in 1:12) #loop through the months
  { 
    mdata1<-pnorm(PT1[month==m]-5) # get daily survival data for the month assuming day 1 exposures
    mdata2<-pnorm(PT2[month==m]-5) # get daily survival data for the month assuming day 2 exposures
    mdata3<-pnorm(PT3[month==m]-5) # get daily survival data for the month assuming day 3 exposures
	
	mdata1<-c(1,mdata1) #initial value on last day of previous month = 1 (all of monthly group alive on that date)
	mdata2<-c(1,mdata2) #initial value on last day of previous month = 1 (all of monthly group alive on that date)
	mdata3<-c(1,mdata3) #initial value on last day of previous month = 1 (all of monthly group alive on that date)
	
	mdata<-mdata1  #initialize the daily mortality data with day 1 data
	for (d in 2:length(mdata1))  #look for 2 day runs of mortality 
		{if (mdata1[d]<1 & mdata1[d-1]<1) {mdata[d]<-min(c(mdata2[d],mdata1[d]))}
		}
	for (d in 3:length(mdata1))  #look for 3 day runs of mortality 
		{if (mdata1[d]<1 & mdata[d-1]<1 & mdata[d-2]<1) {mdata[d]<-min(c(mdata3[d],mdata2[d],mdata1[d]))}
		}
	
	mcpmatrix[m,tt]<-cumprod(mdata)[length(mdata)]  # get the survival fraction on the last day of the month and save it
  }
 
 
  cmd<-paste("d",tt10,"<-dss",sep="")
  eval(parse(text=cmd))
  
   }

 year_index<-year-1997   

pindx<-year_index*12+1:12 # year_index*12 + m finds the right row in minsurv matrix, 

minsurv[pindx,1]<-rep(year)
print(year)
minsurv[pindx,2]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
minsurv[pindx,3]<-apply(mcpmatrix,1,min)  #this finds the lowest monthly survival values among all of the exposure time intervals
 
 
 
 }

minsurv[,3]<-round(minsurv[,3], digits=2) 
newname<-paste(substr(filenames[f], 1, nchar(filenames[f])-16), "_Survival.txt", sep="")
outfile<-paste("/Volumes/HELIOS/MusselBedStructure/MusselSurvival/", site, "/", newname, sep="")
write.table(minsurv, outfile, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="\t")

}

} 
 
#postscript("/Users/kasmith/Documents/MusselTempMortality/Graphs/PercentMortalityProbit.ps", family="Times", width=6, height=4)

#image.plot(z=(1-minsurv)*100,zlim=c(0,100), col=tim.colors(20), xlab="Time", ylab="Sites", nlevel=20,  x=seq(1997, 2008, len=nrow(minsurv)), y=seq(0,4), legend.lab="Percent Mortality", horizontal=TRUE)

#abline(h=0, lwd=3, col="black")
#abline(h=1, lwd=3, col="black")
#abline(h=2, lwd=3, col="black")
#abline(h=3, lwd=3, col="black")
#abline(h=4, lwd=3, col="black")

#dev.off()

 
 