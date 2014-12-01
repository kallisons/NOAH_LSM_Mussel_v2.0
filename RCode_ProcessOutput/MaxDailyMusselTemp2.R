site<-"USCATP"

AssemblyInfo<-read.table("/Users/kasmith/Documents/MusselBedStructure/RCode/AssemblyInfo_BedStructure2.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

AssemblyInfo<-subset(AssemblyInfo, AssemblyInfo$Site=="USCATP")

#temp.sites<-as.data.frame(matrix(NA, nrow(AssemblyInfo)*18, 9))
#colnames(temp.sites)<-c("Site", "Point", "CNTCT", "mean.daily.max", "sd.daily.max", "mean.month.max", "sd.month.max", "mean.ann.max", "sd.ann.max")

temp.sites<-as.data.frame(matrix(NA, nrow(AssemblyInfo)*2, 9))
colnames(temp.sites)<-c("Site", "Point", "CNTCT", "mean.daily.max", "sd.daily.max", "mean.month.max", "sd.month.max", "mean.ann.max", "sd.ann.max")

points<-sprintf("%1.2f", c(0.25,0.50,0.75,1.00))

beddepth<-"7cm"

d<-1
for(e in 1:length(points)){

point<-points[e]
folder<-paste("/Volumes/HELIOS/MusselBedStructure/MusselTemperatures/", beddepth, "/", site, "/", point, "/", sep="")
files<-list.files(folder)

if(length(files)>2){
files<-files[c(1,length(files))]
	
}


for(f in 1:length(files)){
	print(files[f])
	data<-read.table(paste(folder, files[f], sep=""), header=TRUE)
	
	dailymax<-aggregate(data$Temp, list(data$Date), max)
	colnames(dailymax)<-c("Date", "dailymax")
	dailymax<-dailymax[-1,]
	
	dailymax$yearmonth<-substr(dailymax$Date, 1,7)
	
	monthlymax<-aggregate(dailymax$dailymax, list(dailymax$yearmonth), max)
	colnames(monthlymax)<-c("yearmonth", "monthlymax")
	
	monthlymax$year<-substr(monthlymax$yearmonth, 1,4)
	annmax<-aggregate(monthlymax$monthlymax, list(monthlymax$year), max)
	colnames(annmax)<-c("year", "annmax")
	
	
	temp.sites$Site[d]<-substr(files[f], 1, 6)
	temp.sites$Point[d]<-substr(files[f], 8, 11)
	temp.sites$CNTCT[d]<-substr(files[f], 18, 21)
	temp.sites$mean.daily.max[d]<-mean(dailymax$dailymax)
	temp.sites$sd.daily.max[d]<-sd(dailymax$dailymax)
	temp.sites$mean.month.max[d]<-mean(monthlymax$monthlymax)
	temp.sites$sd.month.max[d]<-sd(monthlymax$monthlymax)
	temp.sites$mean.ann.max[d]<-mean(annmax$annmax)
	temp.sites$sd.ann.max[d]<-sd(annmax$annmax)
	
	d<-d+1
	}
	
}

temp.sites$mean.daily.max<-round(temp.sites$mean.daily.max, digits=2)
temp.sites$sd.daily.max<-round(temp.sites$sd.daily.max, digits=2)

temp.sites$mean.month.max<-round(temp.sites$mean.month.max, digits=2)
temp.sites$sd.month.max<-round(temp.sites$sd.month.max, digits=2)

temp.sites$mean.ann.max<-round(temp.sites$mean.ann.max, digits=2)
temp.sites$sd.ann.max<-round(temp.sites$sd.ann.max, digits=2)

temp.sites$se.month.max<-temp.sites$sd.month.max/(sqrt(12*11))
temp.sites$se.month.max<-round(temp.sites$se.month.max, digits=2)

temp.sites$se.ann.max<-temp.sites$sd.ann.max/(sqrt(11))
temp.sites$se.ann.max<-round(temp.sites$se.ann.max, digits=2)


outfile<-paste("/Volumes/HELIOS/MusselBedStructure/MusselDailyMaxTemp/", beddepth, "/", site, "_MusselDailyMaxTemp.txt", sep="")

write.table(temp.sites, outfile, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

