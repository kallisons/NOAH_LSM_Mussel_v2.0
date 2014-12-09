#R Packages that need to be installed
require(caTools)
require(fields)

#Check that the working directory is the RCode_ProcessOutput folder in the NOAH_LSM_Mussel_v2.0 folder. If needed, use the setwd() command in R to change the working directory to the RCode_ProcessOutput folder.
getwd() #shows the current working directory for the R gui. 

#Change your system time to be in UTC.    
Sys.setenv(TZ="UTC") 

Files_MusselTemps<-list.files("../ExampleProcessedOutput/MusselTemperatures/")

#Coefficients of the relationship among temperature, exposure time and survival from laboratory studies of the intertidal mussel, Mytilus californianus. Data published in:  Mislan, K.A.S., B. Helmuth, and D.S. Wethey. 2014. Geographical variation in climatic sensitivity of intertidal mussel zonation.  Global Ecology and Biogeography. 23:744-756. 
	coefs<-as.data.frame(matrix(nrow=3,ncol=3))
	colnames(coefs)<-c("A", "C", "D")
	coefs$A<-c(-3.14,-3.01,-4.09) # Coefficient A (1 Day, 2 Days, 3 Days)
	coefs$C<-c(-0.93,-0.97,-1.00) # # Coefficient C (1 Day, 2 Days, 3 Days)
	coefs$D<-c(42.16,43.02,43.64) # Coefficient D (1 Day, 2 Days, 3 Days)


for(z in 1:length(Files_MusselTemps)){

#Print file characteristics to the R gui. 
	site<-substr(Files_MusselTemps[z], 13, 18)
	cat("site:", site, "\n")
	point<-sprintf("%1.2f", as.numeric(substr(Files_MusselTemps[z], 21, 24)))
	cat("shore level: ", as.numeric(point)*100, "%", "\n", sep="")
	cntct<-sprintf("%1.2f", as.numeric(substr(Files_MusselTemps[z], 31, 34)))
	cat("contact: ", as.numeric(cntct)*100, "%", "\n", sep="")
	beddepth<-as.numeric(substr(Files_MusselTemps[z], 44, 44))
	cat("beddepth:", beddepth, "cm", "\n")
	cat("----------------------", "\n")


#Read in Mussel Temperature Data that was processed in Step 1.    
	a<-read.table(paste("../ExampleProcessedOutput/MusselTemperatures/", Files_MusselTemps[z],sep=""),header=T,sep="\t")

#Due to the time conversion from UTC, there are some time steps that extend into a year outside the intended time frame.  This code removes the time steps if there are less than 28 days with measurements in a particular year.   
	a$min<-as.numeric(substr(a$Time,4,5))
	tstep<-abs(a$min[2]-a$min[1])
	tsteps.month<-(60/tstep)*24*28
	a$Year<-as.numeric(substr(a$Date,1,4))
	a$YearMonth<-substr(a$Date,1,7)
	yearsummary<-table(a$Year)
	yearselect<-subset(yearsummary, yearsummary>=tsteps.month)
	a<-subset(a, Year %in% as.numeric(names(yearselect)))
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


	for(year in years[1]:years[length(years)]){
 		a1year<-subset(a, a$Year==year)
 		ndays<-length(unique(a1year$Date))
 		month<-as.numeric(substr(a1year$YearMonth, 6, 7))
	
 	
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
			dss<-as.POSIXct(ds1+strptime("1970-01-01","%Y-%m-%d", tz="GMT")) #convert to posix dates
	  		month<-as.POSIXlt(dss)$mo+1 #find t months
   
#Calculate probit of survival from temperature and duration of exposure  
  			PT1<-coefs$A[1]*log10(tt10)+(coefs$C[1]*tss+coefs$D[1])-(coefs$A[1]*log10(30))
  			PT2<-coefs$A[2]*log10(tt10)+(coefs$C[2]*tss+coefs$D[2])-(coefs$A[2]*log10(30))
  			PT3<-coefs$A[3]*log10(tt10)+(coefs$C[3]*tss+coefs$D[3])-(coefs$A[3]*log10(30))  
    
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
	
#Check for 2 consecutive days of mortality. Place correct data in mdata if 2 consecutive days occur.
				for(d in 2:length(mdata1)){
					if (mdata1[d]<1 & mdata1[d-1]<1) {mdata[d]<-min(c(mdata2[d],mdata1[d]))}
				}
		
#Check for 3 consecutive days of mortality. Place correct data in mdata if 3 consecutive days occur.
				for(d in 3:length(mdata1)){  
					if (mdata1[d]<1 & mdata[d-1]<1 & mdata[d-2]<1) {mdata[d]<-min(c(mdata3[d],mdata2[d],mdata1[d]))}
				}
	
#Save the survival fractions from the last day of the month
				mcpmatrix[m,tt]<-cumprod(mdata)[length(mdata)]  
  			}
   		}

#Find the right row in minsurv matrix
 		year_index<-year-years[1]   
		pindx<-year_index*12+1:12  
		minsurv[pindx,1]<-rep(year)
		minsurv[pindx,2]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
		minsurv[pindx,3]<-apply(mcpmatrix,1,min)  #Finds lowest monthly survival among all the exposure intervals
 	}

	minsurv[,3]<-round(minsurv[,3], digits=2) 
	newname<-paste("Survival_", substr(Files_MusselTemps[z], 13, nchar(Files_MusselTemps[z])-4), ".txt", sep="")
	outfile<-paste("../ExampleProcessedOutput/MusselSurvival/", newname, sep="")
	write.table(minsurv, outfile, quote=FALSE, col.names=TRUE, row.names=FALSE, sep="\t")
} 
 

 
 