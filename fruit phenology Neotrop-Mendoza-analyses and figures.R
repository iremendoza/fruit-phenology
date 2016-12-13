
library(sp)
library(raster)
library(rgdal)
library(maps)
library(mapdata)
library(spatialEco)
library (ggplot2)

####A SIMPLE FUNCTION (for internal operations)####
lengthunique = function(x) return(length(unique(x)))

#### DATASETS #####
neolong <- read.delim("Mendoza_dat_GPC.txt") #database including the 218 datasets reviewed: "DO" means Direct Observations
lengthunique(neolong$ID)
drivers <- read.delim("drivers.txt") #environmental drivers of each dataset
lengthunique(drivers$ID)
ests <- read.delim("nb spp kier.txt") ## appendix of Kier et al. 2005 JBiogeograph with the estimated number of spp

####SPATIAL ANALYSES####
##Adding vegetation types from WWF##

# Create a directory for the data
localDir <- 'R_GIS_data'
if (!file.exists(localDir)) {
  dir.create(localDir)
}

#Load the Olson's vegetation data from the WWF site (65 Mb)
url <- "http://assets.worldwildlife.org/publications/15/files/original/official_teow.zip"

file <- paste(localDir, basename(url), sep='/')
if (!file.exists(file)) {
  download.file(url, file)
  unzip(file, exdir=localDir)
}

# Show the unzipped files 
list.files(localDir)

# layerName is the name of the unzipped shapefile without file type extensions 
layerName <- "wwf_terr_ecos"  
data_name <- "WWF"
# Read in the data
file <- paste(getwd(),localDir,"WWF.RData",sep="/")
if (!file.exists(file)) {
  data_projected <- readOGR(dsn = paste(getwd(), localDir,"official",sep="/"), layer=layerName) 
  
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

loc = data.frame(x = neolong$long, y = neolong$lat, ID = neolong$ID, ref = neolong$ref, locality = neolong$locality, vegetation = neolong$vegetation, biome = neolong$biome, species = neolong$species, Nindiv = neolong$Nindiv, studylength = neolong$studylength)
coordinates(loc)<-c("x","y")
crs.geo <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 ")  # geographical, datum WGS84
proj4string(loc) <- crs.geo  # define projection system of our study locations
#summary(loc)
#class(loc)

#Spatial join between our study locations (loc) and the polygons with the vegetation types
if(!file.exists(paste(getwd(),localDir,"spjoin.RData",sep="/"))) { 
load(file)

#spatial join between polygons (data_projected) and points (loc)
pts.poly <- over(loc, data_projected)
spjoin <- data.frame(loc, pts.poly)
spjoin$ECO_ID[spjoin$ID == 29] <- 60170
spjoin$ECO_ID[spjoin$ID == 75] <- 61301
save(spjoin, file = paste(getwd(), localDir, "spjoin.RData", sep = "/"))
}
load(paste(getwd(),localDir,"spjoin.RData",sep="/"))

#kml file for neolong (used for interactive map)
file <- paste(getwd(),localDir,"dat.kml",sep="/") 
if(!file.exists(file)) writeOGR(loc, dsn =paste(getwd(),localDir,"dat.kml" ,sep="/"), layer="neolong", driver = "KML") #kml file created (for interactive map)
##LACKING:include reference, GPC classification type and biome name in dat.kml

#### SOME STATS ABOUT THE DATABASE####

####How many datasets does our dataset have?####
uniquestudy = lengthunique(neolong$ID) #218

####How many unique references does our dataset have?####
uniqueref = lengthunique(neolong$ref) #177
tt <-table(sort(neolong$ref))
length(which(tt>1)) ## not unique references

### surface of the studied polygon: 17700000 km2
(studyperarea = 17000000/218)

####what are the censuring frequency times?####
censtime = function(data = neolong){
  
  freqcens = numeric()## we transform the qualitative variable in a quantitative one
  weekly = length(which(data$frequency=="weekly"|data$frequency=="dayly" )) 
  biweekly = length(grep("biweekly",data$frequency))
  monthly = length(grep("monthly",data$frequency))-length(which(data$frequency == "bimonthly")) 
  day20 = length(which(data$frequency == "20-day"|data$frequency == "every six weeks"))
  bimonthly = length(which(data$frequency == "bimonthly"))
  sporadic = length(grep("sporadic", data$frequency)) + length(grep("irregular", data$frequency))
  unespecified = length(grep("unespecified",data$frequency))+length(grep("herbarium",data$frequency))
  
  summaryvar = data.frame(var = c("weekly", "biweekly","monthly", "day20","bimonthly","sporadic","unespecified"),freq = c(weekly, biweekly, monthly, day20, bimonthly, sporadic, unespecified))  
  summaryvar$per = (summaryvar$freq/sum(summaryvar$freq))*100
  return(summaryvar)
  
}

####vegetation types####
vegtyp = aggregate(data.frame(nbstu = neolong$ID), by = list(vegetation = neolong$vegetation), lengthunique)
vegtyp[order(vegtyp $ nbstu, decreasing = T),]

#### which are the long-term datasets?####
longterm = neolong[which(neolong$studylength >= 120),]
(longtermtable = data.frame(ID = longterm$ID, author = longterm$ref, locality = longterm$locality, length = longterm$studylength, DOI = longterm$DOI))

####CLIMATIC DRIVERS####
#frequency of studies without statistical test
(perctest = aggregate(data.frame(nstu = drivers$ID), by = list(presencetest = drivers$presencetest),length))
(perctest2 = (perctest$nstu[perctest$presencetest=="no"]/sum(perctest$nstu))*100)

#First, I calculate the number of datasets related to each environmental variable
freqdriv1 = aggregate(data.frame(nstu=drivers$ID),by =list(climvar=drivers$climvar),length)
(freqdriv1[order(freqdriv1$nstu,decreasing=T),]) #ordering drivers according to their importance

####Table 2####
#total frequencies for table 2
(rainfreq = (160/218) *100)
(tempfreq =  (42/218) *100)
(daylengthfreq =  (20/218) *100)
(floodingfreq =  (13/218) *100)
(irradiancefreq =  (7/218) *100)
(ensofreq =  (3/218) *100)
(airfreq =  (3/218) *100)
(evapofreq =  (1/218) *100)
(nonefreq =  (48/218) *100)

sum(freqdriv1$nstu[freqdriv1$climvar == "biotic"])/lengthunique(drivers$ID) ## percentage of studies addressing a biotic variable
(lengthunique(drivers$ID) -(freqdriv1$nstu[freqdriv1$climvar == "biotic"] + freqdriv1$nstu[freqdriv1$climvar == "none"]))/lengthunique(drivers$ID) ## percentage of studies addressing a biotic variable
## percentage of studies addressing an environmental variable

#Second, I explore datasets without statistical analyses (Table 2)
driversnotest <- drivers[drivers$presencetest=="no",]
freqdriv2 = aggregate(data.frame(nstu = driversnotest$ID),by = list(climvar = driversnotest$climvar),length)
(freqdriv2[order(freqdriv2$nstu,decreasing=T),]) #ordering drivers according to their importance ##Table 2 (statistically non-tested)

#Third, I explore datasets with statistical analyses (Table 2)
driverstest <- drivers[drivers$presencetest == "yes",]
(lengthunique(driverstest$ID)/lengthunique(drivers$ID))*100
freqdriv3 = aggregate(data.frame(nstu = driverstest$ID),by=list(climvar=driverstest$climvar),length)
freqdriv3[order(freqdriv3$nstu,decreasing = T),] #ordering drivers according to their importance

#frequency of each type of statistical test
(freqtest = aggregate(data.frame(nstu=driverstest$ID),by=list(test=driverstest$typetest),lengthunique)) 
(3+1)/lengthunique(driverstest$ID) #datasets including cross-correlations or moving correlation

#sign of correlations for Table 2
(signrain = aggregate(data.frame(nstu = driverstest[driverstest$climvar=="rainfall",]$ID),by = list(signcorr = driverstest[driverstest$climvar=="rainfall",]$signcorr),length))
(signtemp = aggregate(data.frame(nstu = driverstest[driverstest$climvar=="temperature",]$ID),by=list(sign_temp = driverstest[driverstest$climvar=="temperature",]$signcorr),length))
(signdl = aggregate(data.frame(nstu=driverstest[driverstest$climvar=="daylength",]$ID),by=list(sign_daylength = driverstest[driverstest$climvar=="daylength",]$signcorr),length))
(signflooding = aggregate(data.frame(nstu = driverstest[driverstest$climvar == "flooding"|driverstest$climvar == "tide levels",]$ID),by=list(sign_flooding = driverstest[driverstest$climvar == "flooding"|driverstest$climvar == "tide levels", ]$signcorr), length))
(signirradiance = aggregate(data.frame(nstu = driverstest[driverstest$climvar=="irradiance"|driverstest$climvar == "solar radiation", ]$ID), by = list(sign_irrad = driverstest[driverstest$climvar == "irradiance"|driverstest$climvar == "solar radiation",]$signcorr), length))
(signENSO = aggregate(data.frame(nstu = driverstest[driverstest$climvar == "ENSO",]$ID), by = list(sign_ENSO = driverstest[driverstest$climvar=="ENSO",]$signcorr),length))
(signhumid=aggregate(data.frame(nstu = driverstest[driverstest$climvar == "air humidity", ]$ID), by = list(sign_humid = driverstest[driverstest$climvar == "air humidity",]$signcorr), length))
(signevapo = driverstest[driverstest$climvar == "evaporation",])

#how many drivers were included in each study?
nbstudies = aggregate(data.frame(nbvar=drivers$climvar), by=list(ID=drivers$ID),lengthunique)
nbstudies[order(nbstudies$nbvar,decreasing=T),]
table(nbstudies$nbvar)

####Table 3####
#link each study to its vegetation type and explore its seasonality regarding precipitation
driv <- merge(drivers, neolong, by="ID", all.x=TRUE) #we include vegetation type in the drivers' dataset
raindriv = driv[driv$climvar=="rainfall",]
(t3 <- aggregate(data.frame(nstu = neolong$ID), by = list(peak = neolong$peak, veg = neolong$vegetation), length))
vegtyp2 = aggregate(data.frame(nbstu = driv$ID), by = list(vegetation = driv$vegetation), lengthunique)
vegtyp2[order(vegtyp $ nbstu, decreasing = T),]


signrain = factor(levels=c("positive","negative","none","ambiguous"))

for (i in 1:length(raindriv$signcorr))
{
  if (raindriv$signcor[i]=="positive"|raindriv$signcorr[i]=="negative"|raindriv$signcorr[i] == "none") signrain[i] = raindriv$signcorr[i]
  else signrain[i]= "ambiguous"
}
raindriv = data.frame(raindriv,signrain)
signrainveg = aggregate(data.frame(nstu=raindriv$ID),by=list(signcorr=raindriv$signcorr,vegtype=raindriv$vegetation),length)
(tt = table(raindriv$vegetation, raindriv$signrain))
##include table 3 here

(rainforest = chisq.test(c(rainy = 42,  dry =17, aseasonal = 16, transition = 11))) ##significant
#desert = chisq.test(c(positive=6, negative= 4,none=1)) #not valid
dry = chisq.test(c(positive=9, negative=5,none=1))
(cerrado = chisq.test(c(rainy = 11, transition = 6, dry = 0))) ##significant
flooded =  chisq.test(c(rainy = 10, transition = 5, dry = 5)) ##non-significant
#grassland=chisq.test(c(positive=4, negative=4,none=3))
#montane=chisq.test(c(positive=5, negative=3,none=1))

####Supplementary Table 1####
(ST1 <- aggregate (driv$ID, by = list(climvar = driv$climvar, vegetation = driv$vegetation), lengthunique))

##Carlos' graph:

temp <- driv[driv$climvar=="temperature",]
prec <- driv[driv$climvar=="rainfall",]
hist(abs(temp$lat))
hist(abs(prec$lat), breaks = c(0,5,10,15,20,25,30))

#### FIGURES OF THE PAPER #####

#### Figure1: bibliographic analysis of the number of papers including the term "phenology", "phenology + tropic" and "phenology +tropic +fruit" in Scopus####

figure1 = function(filename = "figure1.tif"){
  
  pheno = read.delim("Scopus-phenolog.txt") #this query was done on the 24/04/2016 using the term "phenolog*" for ALL document types and fields "TITLE-ABS-KEY" in Scopus
  phenotrop = read.delim("Scopus-phenolog AND trop.txt") #this query was done on the 24/04/2016 using the term "phenolog* AND trop*" for ALL document types and fields "TITLE-ABS-KEY" in Scopus
  phenotropfr = read.delim("Scopus-phenolog AND trop AND fruit.txt") #this query was done on the 24/04/2016 using the term "phenolog* AND trop* AND fruit*" for ALL document types and fields "TITLE-ABS-KEY" in Scopus
  totalpub = read.delim("Scopus-totalpub.txt") #this query was done on the 24/04/2016 using the terms Ecology OR Biometeorology OR Evolution for ALL document types and fields "TITLE-ABS-KEY" in Scopus
  scyear1 = merge(pheno,phenotrop,by="YEAR", all.x=T)
  scyear2 = merge(scyear1,phenotropfr,by="YEAR", all.x=T)
  scyear3 = merge(scyear2,totalpub,by="YEAR", all.x=T)[-c(1:91),] #we exclude datasets before 1970
  scyear96 = rbind(data.frame(YEAR=1995, pheno = sum(scyear3$pheno[scyear3$YEAR<1996]), phenotrop = sum(scyear3$phenotrop[scyear3$YEAR < 1996],na.rm = T),phenotropfr = sum(scyear3$phenotropfr[scyear3$YEAR < 1996],na.rm = T),totalpub = sum(scyear3$totalpub[scyear3$YEAR<1996],na.rm = T)),scyear3[scyear3$YEAR >= 1996,])
  
  tiff(filename = filename, height = 1000, width = 2100, pointsize = 24)
  par(mar = c(8,6,2,6))
  counts = t(as.matrix(scyear3[,2:4],beside = TRUE))
  counts2 = t(as.matrix(scyear3[,2:4]/scyear3$totalpub, beside = TRUE)) #we standarized by the total amount of publications in ecological fields
  barplot(counts,las = 2, ylim = c(0,2500), names.arg = as.character(scyear3$YEAR), border = TRUE, col = c("grey80","white", "black"), cex.axis = 1.25, cex = 1.25)
  #barplot(counts2,las=2,ylim=c(0,0.5),names.arg=as.character(scyear3$YEAR),col=c("grey80","darkblue","red"),cex.axis=1.15,cex=1.15)
  mtext(side=2, text = "number of publications in topics of this review", line = 4.5, cex = 1.5, las = 0)
  mtext(side = 1, text = "year of addition to Scopus® database", line = 5, cex = 1.5)
  legend(1,2500, legend = c("phenolog*", "phenolog* + trop*","phenolog* + trop* + fruit*"),bty="n",border=rep("black",3),cex=1.5,fill=c("grey80","white","black"))
  legend(1,2000, legend = c("Ecology, Biometeorology or Evolution publications"), bty = "n", lty = 2, cex = 1.5, col = c("grey20"), lwd = 3)
  
  lm1 = lm(log(scyear3$pheno) ~ scyear3$YEAR)# exponential least square fit for the number of publications as function of the publication year
  summary(lm1) #very good adjustement to an exponential fit
  
  lm2 = lm(log(scyear3$pheno[scyear3$YEAR >= 1996]) ~ scyear3$YEAR[scyear3$YEAR >= 1996])# exponential least square fit for the number of publications as function of the publication year (since 1996)
  summary(lm2) 
  #test of the exponential fit
  #plot(scyear3$YEAR,scyear3$pheno) 
  #lines(scyear3$YEAR, exp(predict(lm1,list(scyear3$YEAR))),col="blue")
  #lines(scyear3$YEAR[scyear3$YEAR>=1996], exp(predict(lm2,list(scyear3$YEAR[scyear3$YEAR>=1996]))),col="red")
  
  
  par(new = T)
  plot(scyear3$YEAR, scyear3$totalpub/40, col = "grey20", ylim = c(0,2100), lty = 2, lwd = 3, type = "l", axes = F, xlab = "", ylab = "")
  axis(side = 4, at = seq(0,2000,500), labels = seq(0,2000,500)*40, col = "grey20", las = 1, cex.axis = 1.25, cex = 1.25, col.axis = "grey20")
  mtext(side = 4, text = "number of publications in ecological fields", line = 4.5, cex = 1.5, las = 0)
  
  #plot(scyear$YEAR[scyear>=1996], scyear$pheno[scyear>=1996],type="l",xlab="",ylab="",las=1,bty="l",lwd=2)
  #lines(scyear$YEAR, scyear$tropicpheno,col="blue",lwd=2)
  #
  dev.off()
}

####Figure 2: statistics
#what is the sampling effort in terms of number of species?
hsp = hist(neolong$species)
hsp$counts [1]/ sum(hsp$counts)
summary(neolong$species)

#what is the sampling effort in terms of monitoring length?
hlength = hist(neolong$studylength, breaks = c(12, 24, 36, 48, 60, 120, 480), main = "studylength")
str(hlength)
hlength$counts[1]/ sum(hlength$counts)
summary(neolong$studylength)


####Figure 3: How many countries do we have in our dataset?####

figure3 = function(data = neolong, cex=2, filename = "figure3.tif",...){
  
  data$country = as.character(data$country)
  lengthunique(data$country) #number of study sites
  countryfreq = aggregate(data.frame(numb = data$ID), by = list(country = data$country), lengthunique)
  countryfreq[order(countryfreq$numb, decreasing = T),]
  (statefreq = aggregate(data.frame(numb = data$ID), by = list(state = data$BrazilState), lengthunique))
  max(statefreq$numb)/lengthunique(data$ID) #frequency of studies from São Paulo state in Brazil
  sort(statefreq$numb, decreasing = T) [2]/lengthunique(data$ID) #frequency of studies from Amazonia state in Brazil

  reorder_size <- function(x) {
    factor(x, levels = names(sort(table(data$country), decreasing = T)))
  }
  ggplot(data, aes(reorder_size(country))) + geom_bar(fill="grey80", colour="black") + ylab("Number of datasets") + xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0,  size = 15, colour = "black"), axis.text.y = element_text(size = 15), axis.ticks.x = element_blank(),  panel.background = element_blank(), axis.title.y = element_text(size = 20), plot.margin = unit(c(1, 2, 1, 1), "cm"), panel.margin = unit(c(2,2,2,1), "cm") ) 
  ggsave(filename, device ="tiff")
}

####Figure 4: which type of methods did authors use for studying phenology?####
figure4 = function(data = neolong, filename="figure4.tif"){
  direct = aggregate(data.frame(nstu=data$ID), by=list(direct=data$DO, marked=data$marked),length)
  indirect = aggregate(data.frame(nstu=data$ID),by=list(LT=data$LT,herbarium=data$herbarium, feces=data$feces,ground=data$ground.survey),length)
  
  marked = length(which(data$marked=="marked"))
  unmarked = length(which(data$marked=="unmarked"))
  lt = length(which(data$LT=="yes")) 
  mean(data$trapsurface,na.rm=T) #mean surface of seed traps
  sd(data$trapsurface,na.rm=T) #sd surface of seed traps
  range(data$Ntraps,na.rm=T) #range of number of traps
  mean(data$Ntraps,na.rm=T) #range of number of traps
  sd(data$Ntraps,na.rm=T) #range of number of traps
  
  herbarium = length(which(data$herbarium=="yes"))
  feces = length(which(data$feces=="yes"))
  ground = length(which(data$ground.survey=="yes"))
  
  summaryvar = matrix(ncol=2,nrow=6, dimnames = list(c("marked","unmarked" ,"traps", "herbarium","faeces","ground surveys"), c("direct","indirect")))
  summaryvar[,1] = c(marked,unmarked,0,0,0,0)
  summaryvar[,2] = c(0,0,lt, herbarium,feces,ground)
  percentages = (summaryvar/lengthunique(neolong$ID))*100
  
  tiff(filename=filename,height=700,width=900,pointsize=24)
  par(mar = c(3,5,3,1), cex=1.25)
  barplot(summaryvar, las=1, ylim=c(0,200), ylab="number of datasets", col=c("violet","violetred4","darkorange","moccasin","tan","tan4"),width=c(10,10),legend=rownames(summaryvar),args.legend=list(bty="n"))
  
  dev.off()
}

####Figure6: how many  species were studied by vegetation type?####

figure6 = function(data = spjoin, ests = ests, filename = "figure6.tif"){
  tiff(filename = filename, height = 900, width = 1100, pointsize = 24)
  par(mar = c(4,16,1,2), mfrow = c(2,1))
  
  lm1 = lm(data$species ~ data$vegetation)
  summary (lm1)
  
  ##SAMPLING EFFORT  "se" calculates a ratio p with sampling effort per spp
    se = merge(data, ests, by="ECO_ID",all.x=T) 
    se$p = se$species/se$sp_wfig
    sum(se$sp_wfig) #estimated number of species
    sum(se$species, na.rm = T) #total number of studied species
    sum(se$species, na.rm = T)/sum(se$sp_wfig) #undersampling of plant species (1%)
    
    summary(se$p)
   
  meanratio = aggregate(data.frame(ratio=se$p), by=list(vegetation=se$vegetation),mean,na.rm=T)
  mediannsp = aggregate(data.frame(nbspp=se$species), by=list(vegetation=se$vegetation), median,na.rm=T)
  meansp = aggregate(data.frame(nbspp=se$species), by=list(vegetation=se$vegetation), mean,na.rm=T)
  minsp = aggregate(data.frame(nbspp=se$species), by=list(vegetation=se$vegetation), min,na.rm=T)
  maxsp = aggregate(data.frame(nbspp=se$species), by=list(vegetation=se$vegetation), max, na.rm=T)
  sdratio = aggregate(data.frame(ratio=se$species), by=list(vegetation=se$vegetation), sd,na.rm=T)
  medianratio = aggregate(data.frame(ratio=se$p), by=list(vegetation=se$vegetation), median,na.rm=T)
  veg = meanratio$vegetatio[order(meanratio$ratio)]
  veg2 = medianratio$vegetatio[order(medianratio$ratio)]
  se$newveg=factor(as.character(se$vegetation),levels=as.character(medianratio$vegetation[order(medianratio$ratio)]))
  b=boxplot(se$p~se$newveg,las=1,col=c("deeppink","brown","darkgreen","red","blue1","purple","orange","green1","bisque"),horizontal=T,cex.axis=1)
  mtext(side=1,"sampling effort ratio",line=2.5,cex=1.3)
  se$sppveg=factor(as.character(se$vegetation),levels=as.character(meansp$vegetation[order(meansp$nbspp)]))
  #b=boxplot(se$s~se$sppGPC,las=1,col=c("brown","bisque","darkgreen","deeppink","purple","green1","blue1","red","orange"),horizontal=T,cex.axis=1)
  b2=boxplot(se$species~se$newveg,las=1,col=c("deeppink","brown","darkgreen","red","blue1","purple","orange","green1","bisque"),horizontal=T,cex.axis=1)
  mtext(side=1,"number of species sampled",line=2.5,cex=1.3)
  dev.off()
}



####Figure S1: What is the monitoring length of datasets?####

#figureS1 plots a barplot with the frequency of studies according to their sampling length
figureS1 = function(data = neolong, filename = "figureS1.tif", cex = 2, ...){
  meansampling = mean(data$studylength,na.rm = T)
  oneyear = which(data$studylength <= 12)  
  twoyear = which(data$studylength <= 24 & data$studylength > 12)
  threeyear = which(data$studylength <= 36 & data$studylength > 24)
  fouryear = which(data$studylength <= 48& data$studylength > 36)
  fiveyear = which(data$studylength < 120 & data$studylength > 48)
  tenyear = which(data$studylength >= 120)
  summarylength = data.frame(time = c("1 year", "2 years","3 years", "4 years", "5-9 years","10 or more years"), freq = c(length(oneyear),length(twoyear), length(threeyear), length(fouryear), length(fiveyear), length(tenyear)))
  summarylength$perc = (summarylength$freq/sum(summarylength$freq))*100
  par(mar = c(12,20,2,2), oma = c(8,25,8,8), cex = cex, ...)
  tiff(filename = filename, height = 900, width = 1400, pointsize = 24)
  barplot(summarylength$perc, names.arg = summarylength$time, las = 1, ylim = c(0,60), ylab = "", cex.axis = 1) 
  mtext (side = 2, text = "percentage of datasets (%)", line = 2.2, cex = 1.25)
  dev.off()  
}

####Figure S2:
#figureS2(data = neolong, filename="figureS2.tif")
figureS2 = function(data = neolong, filename="figureS2.tif",cex=2,...){
  
  tiff(filename = filename,height=700,width=1000,pointsize=12) #
  par(mar = c(4,4,4,4), cex = cex)
  plot(sort(neolong$Nindiv[which(!is.na(data$Nindiv))]/neolong$species[which(!is.na(data$Nindiv))],decreasing=T),axes=F,ylab="",xlab="",xlim=c(0,120),pch=20)
  #plot(sort(log(neolong$Nindiv[which(!is.na(data$Nindiv))]/sum(neolong$Nindiv[which(!is.na(data$Nindiv))])+1),decreasing=T))
  abline(h=15,lty=2)
  axis(side=1,at=seq(0,120,20),labels=seq(0,120,20))
  mtext(side=1, text="Rank of datasets",line=3,cex=cex)
  axis(side=2,at=seq(0,100,20),labels=seq(0,100,20),las=2)
  mtext(side=2, text="number of individuals/number of species",line=3,cex=cex)
  #proportion of studies with at least 15 individuals sampled per species
  length(which(neolong$Nindiv[which(!is.na(data$Nindiv))]/neolong$species[which(!is.na(data$Nindiv))]>=15))/length(neolong$Nindiv[which(!is.na(data$Nindiv))])
  
  dev.off()  
}
