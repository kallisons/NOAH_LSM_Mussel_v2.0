#Check that the working directory is the RCode_ProcessOutput folder in the NOAH_LSM_Mussel_v2.0 folder. If needed, use the setwd() command or the Misc menu in R to change the working directory to the RCode_ProcessOutput folder.
getwd() #shows the current working directory for the R gui. 

#Change your system time to be in UTC for time conversions.    
Sys.setenv(TZ="UTC")

#File path to folder with the files
folder<-paste("../ExampleProcessedOutput/MusselTemperatures/")
files<-list.files(folder)

#Create table for data entry
temp.sites<-as.data.frame(matrix(NA, length(files), 13))
colnames(temp.sites)<-c("Site", "Point", "cntct", "beddepth", "num.days", "num.months", "num.years", "mean.daily.max", "sd.daily.max", "mean.month.max", "sd.month.max", "mean.ann.max", "sd.ann.max")

#Read data in and calculate mean daily, monthly, and annual maximum mussel temperatures
for(f in 1:length(files)){
	print(files[f])
	data<-read.table(paste(folder, files[f], sep=""), header=TRUE)
	
#Due to the time conversion from UTC, there are some time steps that extend into a year outside the intended time frame.  This code removes the time steps if there are less than 28 days with measurements in a particular year.   
	data$min<-as.numeric(substr(data$Time,4,5))
	tstep<-abs(data$min[2]-data$min[1])
	tsteps.month<-(60/tstep)*24*28
	data$Year<-as.numeric(substr(data$Date,1,4))
	data$YearMonth<-substr(data$Date,1,7)
	yearsummary<-table(data$Year)
	yearselect<-subset(yearsummary, yearsummary>=tsteps.month)
	data<-subset(data, Year %in% as.numeric(names(yearselect)))
	data<-data[order(data$Unix),]
	
#Calculate daily, monthly, and annual maximum mussel temperatures in the table		
	dailymax<-aggregate(data$Temp, list(data$Date), max)
	colnames(dailymax)<-c("Date", "dailymax")
	dailymax<-dailymax[-1,]
	
	dailymax$yearmonth<-substr(dailymax$Date, 1,7)
	monthlymax<-aggregate(dailymax$dailymax, list(dailymax$yearmonth), max)
	colnames(monthlymax)<-c("yearmonth", "monthlymax")
	
	monthlymax$year<-substr(monthlymax$yearmonth, 1,4)
	annmax<-aggregate(monthlymax$monthlymax, list(monthlymax$year), max)
	colnames(annmax)<-c("year", "annmax")
	
#Enter information about the site and period of time in the table	
	temp.sites$Site[f]<-substr(files[f], 13, 18)
	temp.sites$Point[f]<-substr(files[f], 21, 24)
	temp.sites$cntct[f]<-substr(files[f], 31, 34)
	temp.sites$beddepth[f]<-substr(files[f], 44, 44)
	temp.sites$num.days[f]<-length(unique(data$Date))
	temp.sites$num.months[f]<-length(unique(data$YearMonth))
	temp.sites$num.years[f]<-length(yearselect)

#Enter mean daily, monthly, and annual maximum mussel temperatures in the table
	if(length(unique(data$Date))>2){
	temp.sites$mean.daily.max[f]<-mean(dailymax$dailymax)
	temp.sites$sd.daily.max[f]<-sd(dailymax$dailymax)
	}else{
	cat("Daily mean was not calculated because there are less than three days of data.", "\n")	
	}

	if(length(unique(data$YearMonth))>2){
	temp.sites$mean.month.max[f]<-mean(monthlymax$monthlymax)
	temp.sites$sd.month.max[f]<-sd(monthlymax$monthlymax)
	}else{
	cat("Monthly mean was not calculated because there are less than three months of data.", "\n")	
	}	
	
	if(length(yearselect)>2){
	temp.sites$sd.ann.max[f]<-sd(annmax$annmax)
	temp.sites$mean.ann.max[f]<-mean(annmax$annmax)
	}else{
	cat("Annual mean was not calculated because there are less than three years of data.", "\n")	
	}
	cat("-------------------", "\n")		
}
	
#Round digits in the table to 2 decimal places
temp.sites$mean.daily.max<-round(temp.sites$mean.daily.max, digits=2)
temp.sites$sd.daily.max<-round(temp.sites$sd.daily.max, digits=2)

temp.sites$mean.month.max<-round(temp.sites$mean.month.max, digits=2)
temp.sites$sd.month.max<-round(temp.sites$sd.month.max, digits=2)

temp.sites$mean.ann.max<-round(temp.sites$mean.ann.max, digits=2)
temp.sites$sd.ann.max<-round(temp.sites$sd.ann.max, digits=2)

#Calculate standard error from standard deviation
temp.sites$se.daily.max<-temp.sites$sd.daily.max/(sqrt(temp.sites$num.days))
temp.sites$se.daily.max<-round(temp.sites$se.daily.max, digits=2)

temp.sites$se.month.max<-temp.sites$sd.month.max/(sqrt(temp.sites$num.months))
temp.sites$se.month.max<-round(temp.sites$se.month.max, digits=2)

temp.sites$se.ann.max<-temp.sites$sd.ann.max/(sqrt(temp.sites$num.years))
temp.sites$se.ann.max<-round(temp.sites$se.ann.max, digits=2)

#Write output to text file
outfile<-paste("../ExampleProcessedOutput/MaxMusselTemperatures/MaxMusselTemps.txt", sep="")
write.table(temp.sites, outfile, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

