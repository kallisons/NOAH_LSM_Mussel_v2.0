require(caTools)
require(fields)
require(RNetCDF)

#Check that the working directory is the RCode_ProcessOutput folder in the NOAH_LSM_Mussel_v2.0 folder. If needed, use the setwd() command in R to change the working directory to the RCode_ProcessOutput folder.
getwd() #shows the current working directory for the R gui. 

Files_MusselTemps<-list.files("../ExampleProcessedOutput/MusselTemperatures/")

for(z in 1:nrow(Files_MusselTemps)){

site<-substr(Files_MusselTemps[z], 13, 18)
cat("site:", site)
point<-sprintf("%1.2f", as.numeric(substr(Files_MusselTemps[z], 21, 24)))
cat("shore level: ", as.numeric(point)*100, "%", sep="")
cntct<-sprintf("%1.2f", as.numeric(substr(Files_MusselTemps[z], 31, 34)))
cat("contact: ", as.numeric(cntct)*100, "%", sep="")
beddepth<-as.numeric(substr(Files_MusselTemps[z], 44, 44))
cat("beddepth =", beddepth, "cm")

#Coefficients of the relationship among temperature, exposure time and survival from laboratory studies of the intertidal mussel, Mytilus californianus. Data published in:  Mislan, K.A.S., B. Helmuth, and D.S. Wethey. 2014. Geographical variation in climatic sensitivity of intertidal mussel zonation.  Global Ecology and Biogeography. 23:744-756. 
coefs<-matrix(nrow=3,ncol=3)
coefs[,1]<-c(42.16,43.02,43.64) # Coefficient D (1 Day, 2 Days, 3 Days)
coefs[,2]<-c(-0.93,-0.97,-1.00) # # Coefficient C (1 Day, 2 Days, 3 Days)
coefs[,3]<-c(3.14,3.01,4.09) # Coefficient A (1 Day, 2 Days, 3 Days)

#Read in Mussel Temperature Data.    
print(Files_MusselTemps[z])
a<-read.table(paste("../ExampleProcessedOutput/MusselTemperatures/", Files_MusselTemps[z],sep=""),header=T,sep="\t")

#Due to the time conversion from UTC, there are some time steps that extend into a year outside the intended time frame.  This code removes the time steps if there are less than 28 days with measurements in a particular year.   
a$min<-as.numeric(substr(a$Time,4,5))
tstep<-abs(a$min[2]-a$min[1])
tsteps.month<-(60/tstep)*24*28
a$Year<-as.numeric(substr(a$Date,1,4))
a$YearMonth<-substr(a$Date,1,7)
yearsummary<-table(a$Year)
yearselect<-subset(yearsummary, yearsummary>=tsteps.month)
a<-subset(a, a$Year==names(yearselect))
a<-a[order(a$Unix),]

#The laboratory data used to derive the coefficients for the survival analysis are for 30 minute time intervals.  Convert the time steps from 6 minutes to 30 minutes using a running mean (5 x 6 minutes is 30 minutes of temperatures).
a$Temp2<-runmean(a$Temp, 5)
a<-subset(a, a$min==a$min[1] | a$min==a$min[1]+30)
a<-a[,c("Unix", "Year", "YearMonth", "Date", "Time", "Temp2")]
colnames(a)[6]<-"Temp"
y<-a$Year
tsx<-a$Temp 

#Create a data matrix to store the mussel survival data. The number of rows in the matrix is equal to total number months and there are three columns for Year, Month, Survival.  The assumption of the code below is that all the years being analyzed have 12 months.  Modify the code below if shorter time periods are used. 
years<-unique(a$Year)
yearmonths<-unique(a$YearMonth)
minsurv<-as.data.frame(matrix(nrow=length(years)*12,ncol=3))
colnames(minsurv)<-c("year", "month", "survival")
minsurv$year<-as.numeric(substr(yearmonths, 1, 4))
minsurv$month<-as.numeric(substr(yearmonths, 6, 7))


#frameshift<-0  # shift starting row of data matrix

for(year in years[1]:years[length(years)]){
 	a1year<-subset(a, a$Year==year)
 	ndays<-length(unique(a1year$Date))
 	
 	#Matrix for monthly cumulative survival values, nrows=months and ncols=tt
 	mcpmatrix<-matrix(nrow=12,ncol=16) 
   
  	for(tt in 1:16){
  	tt10<-tt*30
  	
  	#Running minima of temperature series for time interval tt
  		ts1<-runmin(x=a1year$Temp,k=tt,alg="C",endrule="trim") 
  		t_index<-(1:length(ts1))%/%tt #index for tt time increments
  		tss<-tapply(ts1,t_index,min)
  
	#Running minima of date series for time interval tt
  		dsx<-runmin(x=a1year$Unix,k=tt,alg="C",endrule="trim") 
  		ds1<-tapply(dsx,t_index,min) #uses t_index from above
  
# dss<-as.POSIXct(ds1+strptime("1970-01-01","%Y-%m-%d",tz="GMT")) #convert to posix dates
  
	#Calculate probit of survival from temperature and duration of exposure  
  		PT1<-(coefs[1,1]+coefs[1,2]*tss+coefs[1,3]*log10(30))-coefs[1,3]*log10(tt10)
  		PT2<-(coefs[2,1]+coefs[2,2]*tss+coefs[2,3]*log10(30))-coefs[2,3]*log10(tt10)
  		PT3<-(coefs[3,1]+coefs[3,2]*tss+coefs[3,3]*log10(30))-coefs[3,3]*log10(tt10)  
  
	#Calculate monthly survival values
  	for(m in 1:12){ 
    		mdata1<-pnorm(PT1[month==m]-5) #daily survival for day 1 exposures
    		mdata2<-pnorm(PT2[month==m]-5) #daily survival for day 2 exposures
    		mdata3<-pnorm(PT3[month==m]-5) #daily survival for day 3 exposures
	
	#Set initial value on last day of previous month = 1 (all alive)
		mdata1<-c(1,mdata1) 
		mdata2<-c(1,mdata2)
		mdata3<-c(1,mdata3)
	
	#Initialize with day 1 exposures
	mdata<-mdata1  
	
	#Check for 2 consecutive days of mortality
		for(d in 2:length(mdata1)){
			if (mdata1[d]<1 & mdata1[d-1]<1) {mdata[d]<-min(c(mdata2[d],mdata1[d]))}
		}
		
	#Check for 3 consecutive days of mortality
		for(d in 3:length(mdata1)){  
			if (mdata1[d]<1 & mdata[d-1]<1 & mdata[d-2]<1) {mdata[d]<-min(c(mdata3[d],mdata2[d],mdata1[d]))}
		}
	
	#Save the survival fractions from the last day of the month
	mcpmatrix[m,tt]<-cumprod(mdata)[length(mdata)]  
  	
  	}
 
#  cmd<-paste("d",tt10,"<-dss",sep="")
#  eval(parse(text=cmd))
  
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
 

 
 