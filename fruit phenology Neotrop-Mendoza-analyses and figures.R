source(".\\fruit phenology Neotrop-Mendoza-functions.R")

library(sp)
library(raster)
library(rgdal)
library(maps)
library(mapdata)
library(spatialEco)

#### DATASETS #####
neolong<-read.delim("Mendoza_dat_GPC.txt") #dataset including the 214 studies reviewed
drivers<-read.delim("drivers.txt") #environmental drivers of each site
ests<-read.delim(file="nb spp kier.txt") ## appendix of Kier et al. 2005 JBiogeograph with the estimated number of spp

####SPATIAL ANALYSES####
##Adding vegetation types from WWF##

# Create a directory for the data
localDir <- 'R_GIS_data'
if (!file.exists(localDir)) {
  dir.create(localDir)
}

#Load the Olson's vegetation data from the WWF site (65 Mb)
url <- "http://assets.worldwildlife.org/publications/15/files/original/official_teow.zip"

file <- paste(localDir,basename(url),sep='/')
if (!file.exists(file)) {
  download.file(url, file)
  unzip(file,exdir=localDir)
}

# Show the unzipped files 
list.files(localDir)

# layerName is the name of the unzipped shapefile without file type extensions 
layerName <- "wwf_terr_ecos"  
data_name <- "WWF"
# Read in the data
file<-paste(getwd(),localDir,"WWF.RData",sep="/")
if (!file.exists(file)) {
  data_projected <- readOGR(dsn=paste(getwd(),localDir,"official",sep="/"), layer=layerName) 
  
  # What is this thing and what's in it?
  class(data_projected)
  slotNames(data_projected)
  # It's an S4 "SpatialPolygonsDataFrame" object with the following slots:
  # [1] "data"        "polygons"    "plotOrder"   "bbox"        "proj4string"
  
  # What does the data look like with the default plotting command? 
  #plot(data_projected)
  
  # Could use names(data_projected@data) or just:
  names(data_projected)
  
  
  # Reproject the data onto a "longlat" projection and assign it to the new name
  assign(data_name,spTransform(data_projected, CRS("+proj=longlat")))
  
  # The WWF dataset is now projected in latitude longitude coordinates as a
  # SpatialPolygonsDataFrame.  We save the converted data as .RData for faster
  # loading in the future.
  save(list=c(data_name),file=paste(getwd(),localDir,"WWF.RData",sep="/"))
}
if(!file.exists(paste(getwd(),localDir,"spjoin.RData",sep="/"))) {load(file)
loc=data.frame(x=neolong$long,y=neolong$lat,ID=neolong$ID,locality=neolong$locality,vegetation=neolong$vegetation, biome=neolong$biome,species=neolong$species,Nindiv=neolong$Nindiv,studylength=neolong$studylength)
coordinates(loc)<-c("x","y")
crs.geo <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 ")  # geographical, datum WGS84
proj4string(loc) <- crs.geo  # define projection system of our data
summary(loc)
class(loc)

#spatial join between polygons (data_projected) and points (loc)
pts.poly <-over(loc, data_projected)
spjoin<-data.frame(loc,pts.poly)
save(spjoin,file=paste(getwd(),localDir,"spjoin.RData",sep="/"))}
load(paste(getwd(),localDir,"spjoin.RData",sep="/"))

#kml file for neolong
file<-paste(getwd(),localDir,"dat.kml",sep="/") 
if(!file.exists(file)) writeOGR(loc, dsn =paste(getwd(),localDir,"dat.kml" ,sep="/"), layer="neolong", driver = "KML") #kml file created (for interactive map)


#### SOME STATS ABOUT THE DATABASE####

####How many studies does our dataset have####
uniquestudy=lengthunique(neolong$ID)

####What is the spatial distribution of studies?####
#pointsmap(dataset=neolong,circcex=1.5,bg="gray90")
pointsmap=function(dataset=neolong,circcex=1.5,bg="gray90",...){
  par(mfrow = c(1,1), pty = "m", ask = TRUE, mar = c(3,3,3,2))
  lwd.var <- 6  #value of lwd arguments for maps
  map('world', interior = FALSE,fill = FALSE, col = "gray75", lwd = lwd.var, xlim = c(-110, -30), ylim = c(-55, 35))
  draw.tropics(lwidth = lwd.var*0.75)
  points(neolong, pch = 20, col = "steelblue",bg =bg, cex = circcex)
  
}

### map with vegetation types
#mapveg(dataset=cresults2,circcex=1,bg=c("gray90","black"))
mapveg=function(dataset=cresults2,circcex=1,bg=c("gray90","black"),...){
  
  par(mfrow = c(1,1), pty = "m", ask = TRUE, mar = c(3,3,3,2))
  lwd.var <- 6  #value of lwd arguments for maps
  
  map('world', interior = FALSE,fill = FALSE, col = "gray75", lwd = lwd.var, xlim = c(-110, -30), ylim = c(-55, 35))
  draw.tropics(lwidth = lwd.var*0.75)
  points(x = dataset$long[dataset$vegmalhi=="rainforest"], y = dataset$lat[dataset$vegmalhi=="rainforest"], pch=21, bg =bg[1], cex = circcex)   #draw circles for labels  
  points(x = dataset$long[dataset$vegmalhi=="dry"], y = dataset$lat[dataset$vegmalhi=="dry"], pch=21, bg =bg[2], cex = circcex)  
  legend(-60,40, pch=21,pt.bg=bg,legend=c("rainforest","dry forest"),bty="n")
}


## exploring the repitition of study sites

#coord=coordinates[order(coordinates$lat),]

#bubble plot according to the number of species
#bubble plot according to the study length
bubble.map=function(dataset=neo){
  par(mfrow=c(3,1),mar=c(0,2,0.5,1), bty="o", oma=c(0,1,0,1))
  pointsmap(dataset,type="p",ylab="",xlab="", axes=F, main="studies' positions")
  axis(side=1, labels=FALSE)
  axis(side=2, las=2, cex=1)
  bubble.plot(xv=dataset$long[which(is.na(dataset$S)==F)], yv=dataset$lat[which(is.na(dataset$S)==F)], rv=dataset$S[which(is.na(dataset$S)==F)], maint="number of species",axes=F,ylab="", xlab="")
  mtext(text="latitude", side=2, line=3, cex=1.5 )
  axis(side=1, labels=FALSE)
  axis(side=2, las=2, cex=1)
  bubble.plot(xv=dataset$long[which(is.na(dataset$studylength)==F)], yv=dataset$lat[which(is.na(dataset$studylength)==F)], rv=dataset$studylength[which(is.na(dataset$studylength)==F)], maint="study length",ylab="", xlab="",axes=F)
  axis(side=1, cex=1)
  axis(side=2, las=2, cex=1)
  mtext(text="longitude", side=1, line=3.5, cex=1.5 )
}

####what is the studied variable?####

studyvar=function(neo=neo,...){
  par(mar=c(4,3,3,3))
  nbspp=length(which(neo$sppnb=="yes")) 
  indnb=length(which(neo$indnb=="yes")) 
  biomass=length(which(neo$biomass=="yes"))
  Fournier=length(which(neo$Fournier=="yes"))
  nbfruits=length(which(neo$nbfruits=="yes"))
  month=length(which(neo$month=="yes"))
  summaryvar=data.frame(var=c("# species", "# indiv","# fruits", "fruit biomass", "Fournier"),freq=c(sum(nbspp+month),indnb, nbfruits, biomass, Fournier))  
  summaryvar$per=(summaryvar$freq/sum(summaryvar$freq))*100
  print(summaryvar)
  par(mar=c(3,5,3,1),...)
  barplot(summaryvar$freq, names.arg=summaryvar$var, las=1, ylim=c(0,120),ylab="",cex.axis=1.5,cex=2) 
  mtext(side=2,text="number of study sites",cex=2,line=3.5)
}

####what are the censuring frequency times?####
censtime=function(data=neolong){
  
  freqcens=numeric()## we transform the qualitative variable in a quantitative one
  weekly=length(which(data$census.frequency=="weekly"|data$census.frequency=="dayly" )) 
  biweekly=length(grep("biweekly",data$census.frequency))
  monthly=length(grep("monthly",data$census.frequency))-length(which(data$census.frequency=="bimonthly")) 
  day20=length(which(data$census.frequency=="20-day"|data$census.frequency=="every six weeks"))
  bimonthly=length(which(data$census.frequency=="bimonthly"))
  sporadic=length(grep("sporadic",data$census.frequency))+length(grep("irregular",data$census.frequency))
  unespecified=length(grep("unespecified",data$census.frequency))+length(grep("herbarium",data$census.frequency))
  
  summaryvar=data.frame(var=c("weekly", "biweekly","monthly", "day20","bimonthly","sporadic","unespecified"),freq=c(weekly,biweekly, monthly, day20,bimonthly,sporadic,unespecified))  
  summaryvar$per=(summaryvar$freq/sum(summaryvar$freq))*100
  
}



#### which are the long-term datasets?####
longterm=neolong[which(neolong$studylength>=120),]
longtermtable=data.frame(ID=longterm$ID,author=longterm$ref, locality=longterm$locality,length=longterm$studylength, DOI=longterm$DOI)

#### what is the surface of each Olson's biomes####

biomes=function(data=ecoregions){
  surface=aggregate(data.frame(area=data$AREA),by=list(biome=data$BIOME),sum)
  surface[order(surface$area),]
  surface$percentage=(surface$area/sum(surface$area))*100
  biomenumber=aggregate(data.frame(bnumber=wwf$BIOME),by=list(biome=wwf$BIOME_NAME),unique)
  
}


####CLIMATIC DRIVERS####
#frequency of studies without statistical test
perctest=aggregate(data.frame(nstu=drivers$ID),by=list(presencetest=drivers$presencetest),length)
perctest2=(perctest$nstu[perctest$presencetest=="no"]/sum(perctest$nstu))*100

#First, I calculate the number of datasets related to each environmental variable
freqdriv1=aggregate(data.frame(nstu=drivers$ID),by=list(climvar=drivers$climvar),length)
freqdriv1[order(freqdriv1$nstu,decreasing=T),] #ordering drivers according to their importance

#Second, I explore datasets without statistical analyses
driversnotest<-drivers[drivers$presencetest=="no",]
freqdriv2=aggregate(data.frame(nstu=driversnotest$ID),by=list(climvar=driversnotest$climvar),length)
freqdriv2[order(freqdriv2$nstu,decreasing=T),] #ordering drivers according to their importance

#Third, I explore datasets with statistical analyses
driverstest<-drivers[drivers$presencetest=="yes",]
freqdriv3=aggregate(data.frame(nstu=driverstest$ID),by=list(climvar=driverstest$climvar),length)
freqdriv3[order(freqdriv3$nstu,decreasing=T),] #ordering drivers according to their importance
#frequency of each type of statistical test
freqtest=aggregate(data.frame(nstu=driverstest$ID),by=list(test=driverstest$typetest),lengthunique)

#sign of correlations
signrain=aggregate(data.frame(nstu=driverstest[driverstest$climvar=="rainfall",]$ID),by=list(signcorr=driverstest[driverstest$climvar=="rainfall",]$signcorr),length)
signtemp=aggregate(data.frame(nstu=driverstest[driverstest$climvar=="temperature",]$ID),by=list(signcorr=driverstest[driverstest$climvar=="temperature",]$signcorr),length)
signdl=aggregate(data.frame(nstu=driverstest[driverstest$climvar=="daylength",]$ID),by=list(signcorr=driverstest[driverstest$climvar=="daylength",]$signcorr),length)
signflooding=aggregate(data.frame(nstu=driverstest[driverstest$climvar=="flooding"|driverstest$climvar=="tide levels",]$ID),by=list(signcorr=driverstest[driverstest$climvar=="flooding"|driverstest$climvar=="tide levels",]$signcorr),length)
signirradiance=aggregate(data.frame(nstu=driverstest[driverstest$climvar=="irradiance"|driverstest$climvar=="solar radiation",]$ID),by=list(signcorr=driverstest[driverstest$climvar=="irradiance"|driverstest$climvar=="solar radiation",]$signcorr),length)
signENSO=aggregate(data.frame(nstu=driverstest[driverstest$climvar=="ENSO",]$ID),by=list(signcorr=driverstest[driverstest$climvar=="ENSO",]$signcorr),length)
signhumid=aggregate(data.frame(nstu=driverstest[driverstest$climvar=="air humidity",]$ID),by=list(signcorr=driverstest[driverstest$climvar=="air humidity",]$signcorr),length)
evapo=driverstest[driverstest$climvar=="evaporation",]


#how many drivers were included in each study?
nbstudies=aggregate(data.frame(nbvar=drivers$climvar), by=list(ID=drivers$ID),lengthunique)
nbstudies[order(nbstudies$nbvar,decreasing=T),]
table(nbstudies$nbvar)

#link each study to its vegetation type and explore its seasonality regarding precipitation
driv<-merge(drivers,neolong, by="ID",all.x=TRUE) #we include vegetation type in the drivers' dataset
raindriv=driv[driv$climvar=="rainfall",]
signrain=factor(levels=c("positive","negative","none","ambiguous"))
for (i in 1:length(raindriv$signcorr))
{
  if (raindriv$signcor[i]=="positive"|raindriv$signcorr[i]=="negative"|raindriv$signcorr[i]=="none") signrain[i]=raindriv$signcorr[i]
  else signrain[i]= "ambiguous"
}
raindriv=data.frame(raindriv,signrain)
signrainveg=aggregate(data.frame(nstu=raindriv$ID),by=list(signcorr=raindriv$signcorr,vegtype=raindriv$vegetation),length)
tt=table(raindriv$vegetation, raindriv$signrain)
rainforest=chisq.test(c(positive=24, negative=19,none=19))
#desert=chisq.test(c(positive=6, negative=2,none=1))
#dry=chisq.test(c(positive=9, negative=5,none=1))
#cerrado=chisq.test(c(positive=6, negative=2,none=3))
#grassland=chisq.test(c(positive=4, negative=4,none=3))
#montane=chisq.test(c(positive=5, negative=3,none=1))


#### FIGURES OF THE PAPER #####

#### Figure1: bibliographic analysis of the number of papers including the term "phenology", "phenology + tropic" and "phenology +tropic +fruit" in Scopus####

figure1=function(filename="figure1.tif"){
  
  pheno=read.delim("Scopus-phenolog.txt") #this query was done on the 24/04/2016 using the term "phenolog*" for ALL document types and fields "TITLE-ABS-KEY" in Scopus
  phenotrop=read.delim("Scopus-phenolog AND trop.txt") #this query was done on the 24/04/2016 using the term "phenolog* AND trop*" for ALL document types and fields "TITLE-ABS-KEY" in Scopus
  phenotropfr=read.delim("Scopus-phenolog AND trop AND fruit.txt") #this query was done on the 24/04/2016 using the term "phenolog* AND trop* AND fruit*" for ALL document types and fields "TITLE-ABS-KEY" in Scopus
  totalpub=read.delim("Scopus-totalpub.txt") #this query was done on the 24/04/2016 using the terms Ecology OR Biometeorology OR Evolution for ALL document types and fields "TITLE-ABS-KEY" in Scopus
  scyear1=merge(pheno,phenotrop,by="YEAR", all.x=T)
  scyear2=merge(scyear1,phenotropfr,by="YEAR", all.x=T)
  scyear3=merge(scyear2,totalpub,by="YEAR", all.x=T)[-c(1:91),] #we exclude datasets before 1970
  scyear96=rbind(data.frame(YEAR=1995,pheno=sum(scyear3$pheno[scyear3$YEAR<1996]),phenotrop=sum(scyear3$phenotrop[scyear3$YEAR<1996],na.rm=T),phenotropfr=sum(scyear3$phenotropfr[scyear3$YEAR<1996],na.rm=T),totalpub=sum(scyear3$totalpub[scyear3$YEAR<1996],na.rm=T)),scyear3[scyear3$YEAR>=1996,])
  
  tiff(filename=filename,height=1000,width=2100,pointsize=24)
  par(mar=c(8,6,2,6))
  counts=t(as.matrix(scyear3[,2:4],beside=TRUE))
  counts2=t(as.matrix(scyear3[,2:4]/scyear3$totalpub,beside=TRUE)) #we standarized by the total amount of publications in ecological fields
  barplot(counts,las=2,ylim=c(0,2500),names.arg=as.character(scyear3$YEAR),col=c("grey80","darkblue","red"),cex.axis=1.15,cex=1.15)
  #barplot(counts2,las=2,ylim=c(0,0.5),names.arg=as.character(scyear3$YEAR),col=c("grey80","darkblue","red"),cex.axis=1.15,cex=1.15)
  mtext(side=2,text="number of publications in topics of this review",line=4.5,cex=1.5,las=0)
  mtext(side=1,text="year of addition to Scopus database",line=5,cex=1.5)
  legend(1,2500,legend=c("phenolog*", "phenolog* + tropic*","phenolog* + tropic* + fruit*"),bty="n",border=rep("black",3),cex=1.5,fill=c("grey80","darkblue","red"))
  legend(1,2000,legend=c("Ecology, Biometeorology or Evolution publications"),bty="n",lty=2,cex=1.5,col=c("grey40"),lwd=2)
  
  lm1=lm(log(scyear3$pheno)~scyear3$YEAR)# exponential least square fit for the number of publications as function of the publication year
  summary(lm1) #very good adjustement to an exponential fit
  
  lm2=lm(log(scyear3$pheno[scyear3$YEAR>=1996])~scyear3$YEAR[scyear3$YEAR>=1996])# exponential least square fit for the number of publications as function of the publication year (since 1996)
  summary(lm2) 
  #test of the exponential fit
  #plot(scyear3$YEAR,scyear3$pheno) 
  #lines(scyear3$YEAR, exp(predict(lm1,list(scyear3$YEAR))),col="blue")
  #lines(scyear3$YEAR[scyear3$YEAR>=1996], exp(predict(lm2,list(scyear3$YEAR[scyear3$YEAR>=1996]))),col="red")
  
  
  par(new=T)
  plot(scyear3$YEAR, scyear3$totalpub/40,col="grey40",ylim=c(0,2100),lty=2,lwd=2,type="l",axes=F,xlab="",ylab="")
  axis(side=4, at=seq(0,2000,500),labels=seq(0,2000,500)*40,col="grey40",las=1,cex.axis=1.15,col.axis="grey40")
  mtext(side=4,text="number of publications in ecological fields",line=4.5,cex=1.5,las=0)
  
  #plot(scyear$YEAR[scyear>=1996], scyear$pheno[scyear>=1996],type="l",xlab="",ylab="",las=1,bty="l",lwd=2)
  #lines(scyear$YEAR, scyear$tropicpheno,col="blue",lwd=2)
  #
  dev.off()
}

####Figure 3A: How many countries do we have in our dataset?####

figure3A=function(data=neolong,cex=2, filename="figure3A.tif",...){
  
  tiff(filename=filename,height=1600,width=2500,pointsize=24) #
  par(mar=c(12,5,5,1),cex=cex)
   
  data$country=as.character(data$country)
  lengthunique(data$country) #number of study sites
  barplot(sort(table(data$country), decreasing=T),names.arg=names(sort(table(data$country), decreasing=T)), las=2, ylim=c(0,120), ylab="") 
  mtext(side=2,text="number of studies",line=3,cex=3)
  dev.off()
}

####Figure 3B: which type of methods did authors use for studying phenology?#### ## needs to be amended
figure3B=function(data=neolong,filename="figure3B.tif"){
  direct=aggregate(data.frame(nstu=data$ID), by=list(direct=data$DO, marked=data$marked),length)
  indirect=aggregate(data.frame(nstu=data$ID),by=list(LT=data$LT,herbarium=data$herbarium, feces=data$feces,ground=data$ground.survey),length)
  
  marked=length(which(data$marked=="marked"))
  unmarked=length(which(data$marked=="unmarked"))
  lt=length(which(data$LT=="yes")) 
  mean(data$trapsurface,na.rm=T) #mean surface of seed traps
  sd(data$trapsurface,na.rm=T) #sd surface of seed traps
  range(data$Ntraps,na.rm=T) #range of number of traps
  mean(data$Ntraps,na.rm=T) #range of number of traps
  sd(data$Ntraps,na.rm=T) #range of number of traps
  
  herbarium=length(which(data$herbarium=="yes"))
  feces=length(which(data$feces=="yes"))
  ground=length(which(data$ground.survey=="yes"))
  
  summaryvar=matrix(ncol=2,nrow=6, dimnames = list(c("marked","unmarked" ,"traps", "herbarium","feces","ground surveys"),c("direct","indirect")))
  summaryvar[,1]=c(marked,unmarked,0,0,0,0)
  summaryvar[,2]=c(0,0,lt, herbarium,feces,ground)
  percentages=(summaryvar/lengthunique(neolong$ID))*100
  
  tiff(filename=filename,height=700,width=900,pointsize=24)
  par(mar=c(3,5,3,1), cex=1.25)
  barplot(summaryvar, las=1, ylim=c(0,200),ylab="number of study sites",col=c("violet","violetred4","darkorange","moccasin","tan","tan4"),width=c(10,10),legend=rownames(summaryvar),args.legend=list(bty="n"))
  
  dev.off()
}

####Figure6: how many  species were studied by vegetation type?####

figure5=function(neolong=neolong,filename="figure6.tif"){
  tiff(filename=filename,height=700,width=1100,pointsize=24)
  par(mar=c(12,6,2,2))
  boxplot(neolong$s~neolong$GPC,las=2,col=c("deeppink","bisque","brown","green1","red","orange","purple","blue1","darkgreen"))
  mtext(side=2,"number of species sampled",line=4)
  dev.off()
}

figure6b=function(data=spjoin,filename="figure6b.tif"){
  tiff(filename=filename,height=900,width=1100,pointsize=24)
  par(mar=c(4,16,1,2),mfrow=c(2,1))
  ##SAMPLING EFFORT  "se" calculates a ratio p with sampling effort per spp

    se=merge(data, ests,by="ECO_ID",all.x=T) ##I need to check to points without spatial information
    se$p=se$species/se$sp_wfig
    summary(se$p)
   
  meanratio=aggregate(data.frame(ratio=se$p), by=list(vegetation=se$GPC), mean,na.rm=T)
  meansp=aggregate(data.frame(nbspp=se$s), by=list(vegetation=se$GPC), median,na.rm=T)
  sdratio=aggregate(data.frame(ratio=se$p), by=list(vegetation=se$GPC), sd,na.rm=T)
  medianratio=aggregate(data.frame(ratio=se$p), by=list(vegetation=se$GPC), median,na.rm=T)
  veg=meanratio$vegetatio[order(meanratio$ratio)]
  veg2=medianratio$vegetatio[order(medianratio$ratio)]
  se$newGPC=factor(as.character(se$GPC),levels=as.character(medianratio$vegetation[order(medianratio$ratio)]))
  b=boxplot(se$p~se$newGPC,las=1,col=c("deeppink","brown","darkgreen","red","blue1","purple","orange","green1","bisque"),horizontal=T,cex.axis=1)
  mtext(side=1,"sampling effort ratio",line=2.5,cex=1.3)
  se$sppGPC=factor(as.character(se$GPC),levels=as.character(meansp$vegetation[order(meansp$nbspp)]))
  #b=boxplot(se$s~se$sppGPC,las=1,col=c("brown","bisque","darkgreen","deeppink","purple","green1","blue1","red","orange"),horizontal=T,cex.axis=1)
  b2=boxplot(se$s~se$newGPC,las=1,col=c("deeppink","brown","darkgreen","red","blue1","purple","orange","green1","bisque"),horizontal=T,cex.axis=1)
  mtext(side=1,"number of species sampled",line=2.5,cex=1.3)
  dev.off()
}



####Figure S1: What is the length of studies?####

#figureS1 plots a barplot with the frequency of studies according to their sampling length
figureS1=function(data=neolong, filename="figureS1.tif",cex=2,...){
  meansampling=mean(data$studylength,na.rm=T)
  oneyear=which(data$studylength<=12)  
  twoyear=which(data$studylength<=24& data$studylength>12)
  threeyear=which(data$studylength<=36& data$studylength>24)
  fouryear=which(data$studylength<=48& data$studylength>36)
  fiveyear=which(data$studylength<120&data$studylength>48)
  tenyear=which(data$studylength>=120)
  summarylength=data.frame(time=c("1 year", "2 years","3 years", "4 years", "5-9 years","10 or more years"),freq=c(length(oneyear),length(twoyear), length(threeyear), length(fouryear), length(fiveyear),length(tenyear)))
  summarylength$perc=(summarylength$freq/sum(summarylength$freq))*100
  par(mar=c(10,5,2,2),oma=c(1,4,4,4),cex=cex, ...)
  tiff(filename=filename,height=900,width=1200,pointsize=24)
  barplot(summarylength$perc, names.arg=summarylength$time, las=1, ylim=c(0,60),ylab="percentage of studies (%)") 
  dev.off()  
}

####Figure S2:
#figureS2(data=neolong, filename="figureS2.tif")
figureS2=function(data=neolong, filename="figureS2.tif",cex=2,...){
  
  tiff(filename=filename,height=700,width=1000,pointsize=12) #
  par(mar=c(4,4,4,4),cex=cex)
  plot(sort(neolong$Nindiv[which(!is.na(data$Nindiv))]/neolong$species[which(!is.na(data$Nindiv))],decreasing=T),axes=F,ylab="",xlab="",xlim=c(0,120),pch=20)
  #plot(sort(log(neolong$Nindiv[which(!is.na(data$Nindiv))]/sum(neolong$Nindiv[which(!is.na(data$Nindiv))])+1),decreasing=T))
  abline(h=15,lty=2)
  axis(side=1,at=seq(0,120,20),labels=seq(0,120,20))
  mtext(side=1, text="Studies rank",line=3,cex=cex)
  axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=2)
  mtext(side=2, text="number of individuals/number of species",line=3,cex=cex)
  #proportion of studies with at least 15 individuals sampled per species
  length(which(neolong$Nindiv[which(!is.na(data$Nindiv))]/neolong$species[which(!is.na(data$Nindiv))]>=15))/length(neolong$Nindiv[which(!is.na(data$Nindiv))])
  
  dev.off()  
}
