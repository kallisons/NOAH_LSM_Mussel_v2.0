require(chron)

#Check that the working directory is the RCode_ProcessOutput folder in the NOAH_LSM_Mussel_v2.0 folder. If needed, use the setwd() command to change the working directory to the RCode_ProcessOutput folder.
getwd() #shows the current working directory for the R gui.    

#Change your system time to be in UTC.    
Sys.setenv(TZ="UTC")

#File Path
Files_ModelOutput<-list.files("../ExampleModelOutput/") 

beddepth<-"3" #depth layer in mussel bed in cm, total mussel bed is 8 cm, contact is varied at 4 cm
	
for(z in 1:length(Files_ModelOutput)){
site<-substr(Files_ModelOutput[z], 1, 6) #Santa Cruz, CA, USA 
cat("site:", site, "\n")
longitude<--122.065 #Longitude of Santa Cruz, CA, USA
cat("longitude:", longitude, "\n")
point<-sprintf("%1.2f", as.numeric(substr(Files_ModelOutput[z], 9, 12)))
cat("shore level: ", as.numeric(point)*100, "%", "\n", sep="")	
cntct<-sprintf("%1.2f", as.numeric(substr(Files_ModelOutput[z], 19, 22)))
cat("contact: ", as.numeric(cntct)*100, "%", "\n", sep="")
year<-as.numeric(substr(Files_ModelOutput[z], 24,27))
cat("year: ", year, "\n", sep="")

widths=list(jday=0,hhmm=0,f=0,RNETcalc=0,
CH=0,CM=0,H=0,S=0,AET=0,RES=0,Fup=0,FLX1=0, FLX2=0,FLX3=0,Tskin=0,Q1=0,ETPS=0,
X1=0,X2=0,X3=0,X4=0,X5=0,
X6=0,X7=0,X8=0,X9=0,X10=0,
X11=0,X12=0,X13=0,X14=0,X15=0,
X16=0,X17=0,X18=0,X19=0,X20=0)

data<-data.frame(scan(paste("../ExampleModelOutput/", Files_ModelOutput[z], sep=""), widths, multi.line=TRUE))

data$nchar<-nchar(data$hhmm)
data$hhmm<-ifelse(data$nchar==1, paste("000", data$hhmm, sep=""), ifelse(data$nchar==2, paste("00", data$hhmm, sep=""), ifelse(data$nchar==3, paste(0, data$hhmm, sep=""), data$hhmm)))
data$Date<-paste(year, data$jday, data$hhmm)
store<-strptime(data$Date, "%Y %j %H%M")
store2<-as.POSIXct(store)
class(store2)<-("numeric")
data$Unix<-store2
data$Unix<-data$Unix+longitude*240  #Convert from UTC to local time based on solar noon.  
data$Unix<-round(data$Unix, digits=0)
DateConvert<-data$Unix
class(DateConvert)<-"POSIXct"
DateExtract<-as.POSIXlt(DateConvert)
data$Date<-as.character(DateExtract, format="%Y-%m-%d")
data$Time<-as.character(DateExtract, format="%H:%M")

data<-data[,c("Unix","Date","Time", paste("X", beddepth, sep=""))]
colnames(data)[4]<-"Temp"
data$Temp<-data$Temp-273.15
data$Temp<-round(data$Temp, digits=1)

#File Path
out<-paste("../ExampleProcessedOutput/MusselTemperatures/MusselTemps_", site, "_p", point, "_cntct", cntct, "_beddepth", beddepth, ".txt", sep="")

write.table(data, out, col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

}