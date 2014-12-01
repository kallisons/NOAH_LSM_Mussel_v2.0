require(chron)

AssemblyInfo<-read.table("/Users/kasmith/Documents/MusselBedStructure/RCode/AssemblyInfo_BedStructure2.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

AssemblyInfo<-subset(AssemblyInfo, AssemblyInfo$Site=="USCATP")

#CNTCTList<-seq(0.05, 0.90, 0.05)

#CNTCTList<-c(0.05, 0.90)

#for(z in 13:16){

#beddepth<-1 continue from 47

beddepth<-"Tskin"
	
for(z in 1:nrow(AssemblyInfo)){

site<-AssemblyInfo$Site[z]
print(site)
print(z)
longitude<-AssemblyInfo$Lon[z]
point<-sprintf("%1.2f", AssemblyInfo$Point[z])
height<-sprintf("%03.0f", round(AssemblyInfo$Height[z],digits=2)*100)	

#if(site=="USCATP"){CNTCTList<-seq(0.05, 0.90, 0.05)}else{CNTCTList<-c(0.05,0.90)}
CNTCTList<-c(0.05,0.90)

for(q in 1:length(CNTCTList)){

cntct<-sprintf("%1.2f", CNTCTList[q])
print(cntct)

Folder<-paste("/Volumes/HELIOS/MusselBedStructure/Output/",  site, "/", point, "/CNTCT", cntct, "/", sep="")
#Folder<-paste("/Users/kasmith/Documents/MusselBedStructure/Output/",  site, "/", point,"_h", height, "/CNTCT", cntct, "/", sep="")
#Folder<-"/Users/kasmith/Documents/MusselBedStructure/Output/USCABD/0.25_h046/"

Files<-list.files(path=Folder, pattern=NULL, full.names=FALSE, recursive=FALSE)

for(b in 1:length(Files)){

widths=list(jday=0,hhmm=0,f=0,RNETcalc=0,
CH=0,CM=0,H=0,S=0,AET=0,RES=0,Fup=0,FLX1=0, FLX2=0,FLX3=0,Tskin=0,Q1=0,ETPS=0,
X1=0,X2=0,X3=0,X4=0,X5=0,
X6=0,X7=0,X8=0,X9=0,X10=0,
X11=0,X12=0,X13=0,X14=0,X15=0,
X16=0,X17=0,X18=0,X19=0,X20=0)

year<-as.numeric(substr(Files[b], nchar(Files[b])-7, nchar(Files[b])-4))
data<-data.frame(scan(paste(Folder, Files[b], sep=""),widths,multi.line=TRUE))

data$nchar<-nchar(data$hhmm)
data$hhmm<-ifelse(data$nchar==1, paste("000", data$hhmm, sep=""), ifelse(data$nchar==2, paste("00", data$hhmm, sep=""), ifelse(data$nchar==3, paste(0, data$hhmm, sep=""), data$hhmm)))
data$Date<-paste(year, data$jday, data$hhmm)
store<-strptime(data$Date, "%Y %j %H%M")
store2<-as.POSIXct(store)
class(store2)<-("numeric")
data$Unix<-store2
data$Unix<-data$Unix+longitude*240
data$Unix<-round(data$Unix, digits=0)
DateConvert<-data$Unix
class(DateConvert)<-"POSIXct"
DateExtract<-as.POSIXlt(DateConvert)
data$Date<-as.character(DateExtract, format="%Y-%m-%d")
data$Time<-as.character(DateExtract, format="%H:%M")

#data<-data[,c("Unix","Date","Time", paste("X", beddepth, sep=""))]
data<-data[,c("Unix","Date","Time", beddepth)]
colnames(data)[4]<-"Temp"
data$Temp<-data$Temp-273.15
data$Temp<-round(data$Temp, digits=1)

assign(Files[b], data)

}

AllData<-do.call("rbind", lapply(Files, as.name))

out<-paste("/Volumes/HELIOS/MusselBedStructure/MusselTemperatures/", beddepth, "/", site, "/", point, "/", site,"_", point, "_cntct", cntct, "_MusselTemps.txt", sep="")
write.table(AllData, out, col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

#rmobj=subset(ls(), ls()!="AssemblyInfo" & ls()!="CNTCTList" & ls()!="q" & ls()!="z")
#rm(list=rmobj)
#rm(list=ls()[4])

}
}