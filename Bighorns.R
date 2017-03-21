#Copied over from Bighorns.Rmd 3/20 because graphing wasnt going to plots.

rm(list = ls())
library (dplR)
#library (treeclim)
library (ncdf4)
#library(rgl) #plot 3d
library (Hmisc)

## Import, Visualize, Detrend, and build Tree Ring Chronologies for 10 sites (dplR)

PR1pico <- read.rwl("1PR17.rwl")

PR2pien <- read.rwl("2PRf.rwl")
PR2pien$`2PR017a1`= NULL

TB1 <- read.rwl("1TBall.rwl")
c <- c(1, 2, 3, 4, 7, 12, 13, 19, 20, 27, 28, 31, 32 ,37, 38 ,39, 40, 41, 42 ,43 ,44, 45 ,46, 47 ,48, 49, 50, 51, 52, 53, 54)
TB1psme <- TB1[,c]
c <- c(5,6,8,9,10,11,14, 15, 16, 17, 18,21,22,23,24,25,26,29,30,33,34,35,36)
TB1pien <- TB1[,c]

TB2 <- read.rwl("2TB.2.rwl")
TB2psme <- TB2 # remove PIEN from TB2psme, remove 2TB003a1,a2 which is TB2[:,5:6]

TB2psme$`2TB003a1`= NULL
TB2psme$`2TB003a2`= NULL

BC2pien <- read.rwl("2BC7.rwl") #2015
BC2pien <- BC2pien[1:349,] #crop off 2015

BCKpien <- read.rwl("BCK.4.rwl") #2015
BCKpien <- BCKpien[1:348,] #crop off 2015

GPS <- read.rwl("GPSf.rwl") #2015
GPS <- GPS[1:333,] #crop off 2015
c <- c(2,3,4,5,6,7,9,10,13, 14, 15, 16, 17, 18, 19, 20, 21, 22 ,23 ,24, 25, 26,27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38)
GPSpien <- GPS[,c]
GPSpico <- GPS
GPSpico[,c] <- NULL

DGU <- read.rwl("DGUf.rwl")
c <- c(19,20,21,22,23,38,39)
DGUpien <- DGU[,c]
DGUpico <- DGU
DGUpico[,c] <- NULL

HMRpien <- read.rwl("HMR9G.rwl")
BGOpien <- read.rwl("BGOf.rwl")
WY021pien <- read.rwl("WY021.txt")
WY024pien <- read.rwl("WY024.txt")


# Summary of sites
#summary(TB1)
#summary(TB2)
#summary(BC2pien)
#summary(BCKpien)
#summary(GPS)


# Correlations between rwl 
#corr.rwl.seg(TB1,seg.length=60, pcrit = 0.01) #shows a correlation graph 
#corr.rwl.seg(TB2,seg.length=60, pcrit = 0.01)
#corr.rwl.seg(BC2pien,seg.length=60, pcrit = 0.01)
#corr.rwl.seg(BCKpien,seg.length=60, pcrit = 0.01)
#corr.rwl.seg(GPS,seg.length=60, pcrit = 0.01)

# Option to make skeleton plot 
# xskel.ccf.plot(wy30.crn, series = 1,win.start=1496, win.width=100)


# Make RWIndices for 10 sites


PR1pico.rwi <- detrend(PR1pico, make.plot = F, method = "Spline", nyrs = 30)
PR2pien.rwi <- detrend(PR2pien, make.plot = F, method = "Spline", nyrs = 30)
TB1pien.rwi <- detrend(TB1pien, make.plot = F, method = "Spline", nyrs = 30)
TB1psme.rwi <- detrend(TB1psme, make.plot = F, method = "Spline", nyrs = 30)
TB2psme.rwi <- detrend(TB2psme, make.plot = F, method = "Spline", nyrs = 30)
BC2pien.rwi <- detrend(BC2pien, make.plot = F, method = "Spline", nyrs = 30)
BCKpien.rwi <- detrend(BCKpien, make.plot = F, method = "Spline", nyrs = 30)
DGUpien.rwi <- detrend(DGUpien, make.plot = F, method = "Spline", nyrs = 30)
DGUpico.rwi <- detrend(DGUpico, make.plot = F, method = "Spline", nyrs = 30)
GPSpien.rwi <- detrend(GPSpien, make.plot = F, method = "Spline", nyrs = 30)
GPSpico.rwi <- detrend(GPSpico, make.plot = F, method = "Spline", nyrs = 30)
HMRpien.rwi <- detrend(HMRpien, make.plot = F, method = "Spline", nyrs = 30)
BGOpien.rwi <- detrend(BGOpien, make.plot = F, method = "Spline", nyrs = 30)
WY021pien.rwi <- detrend(WY021pien, make.plot = F, method = "Spline", nyrs = 30)
WY024pien.rwi <- detrend(WY024pien, make.plot = F, method = "Spline", nyrs = 30)

# Create CRN for 10 sites

# test PR1piconna.rwi<- na.omit(PR1pico.rwi)

PR1pico.crn <- chron(PR1pico.rwi, prefix = "PR1", biweight = T, prewhiten = F)
PR2pien.crn <- chron(PR2pien.rwi, prefix = "PR2", biweight = T, prewhiten = F)
TB1pien.crn <- chron(TB1pien.rwi, prefix = "TB1", biweight = T, prewhiten = F)
TB1psme.crn <- chron(TB1psme.rwi, prefix = "TB1", biweight = T, prewhiten = F)
TB2psme.crn <- chron(TB2psme.rwi, prefix = "TB2", biweight = T, prewhiten = F)
BC2pien.crn <- chron(BC2pien.rwi, prefix = "BC2", biweight = T, prewhiten = F)
BCKpien.crn <- chron(BCKpien.rwi, prefix = "BCK", biweight = T, prewhiten = F)
DGUpico.crn <- chron(DGUpico.rwi, prefix = "DGU", biweight = T, prewhiten = F)
DGUpien.crn <- chron(DGUpien.rwi, prefix = "DGU", biweight = T, prewhiten = F)
GPSpico.crn <- chron(GPSpico.rwi, prefix = "GPS", biweight = T, prewhiten = F)
GPSpien.crn <- chron(GPSpien.rwi, prefix = "GPS", biweight = T, prewhiten = F)
HMRpien.crn <- chron(HMRpien.rwi, prefix = "HMR", biweight = T, prewhiten = F)
BGOpien.crn <- chron(BGOpien.rwi, prefix = "BGO", biweight = T, prewhiten = F)
WY021pien.crn <- chron(WY021pien.rwi, prefix = "W21", biweight = T, prewhiten = F)
WY024pien.crn <- chron(WY024pien.rwi, prefix = "W24", biweight = T, prewhiten = F)

#prewhiten? remove DGUpien?

##################

## Check Prewhitening- I think R is taking too large ARs
## EPS cut? for each site? rwi.stats.running(PR1pico)$eps >0.85?
## Plot all series?
  #poster organization? Each site? 





############################### Now we PCA
## Run a nested PCA with all Bighorn Chronologies + WY021pien and WY024pien

# Modify years to those all have in common

n <- max(dim(PR1pico.crn[1]), dim(PR2pien.crn[1]), dim(HMRpien.crn[1]), dim(DGUpico.crn[1]), dim(BGOpien.crn[1]), dim(TB1pien.crn[1]), dim(TB2psme.crn[1]), dim(BC2pien.crn[1]), dim(BCKpien.crn[1]), dim(GPSpien.crn[1]))

PR1picoc <- c(rep(NA, n-max(dim(PR1pico.crn))),PR1pico.crn[,1])
PR2pienc <- c(rep(NA, n-max(dim(PR2pien.crn))),PR2pien.crn[,1])
HMRpienc <- c(rep(NA, n-max(dim(HMRpien.crn))),HMRpien.crn[,1])
#DGUpienc <- c(rep(NA, n-max(dim(DGUpien.crn))),DGUpien.crn[,1])
DGUpicoc <- c(rep(NA, n-max(dim(DGUpico.crn))),DGUpico.crn[,1])
BGOpienc <- c(rep(NA, n-max(dim(BGOpien.crn))),BGOpien.crn[,1])
TB1pienc <- c(rep(NA, n-max(dim(TB1pien.crn))),TB1pien.crn[,1])
TB1psmec <- c(rep(NA, n-max(dim(TB1psme.crn))),TB1psme.crn[,1])
TB2psmec <- c(rep(NA, n-max(dim(TB2psme.crn))),TB2psme.crn[,1])
BC2pienc <- c(rep(NA, n-max(dim(BC2pien.crn))),BC2pien.crn[,1])
BCKpienc <- c(rep(NA, n-max(dim(BCKpien.crn))),BCKpien.crn[,1])
GPSpienc <- c(rep(NA, n-max(dim(GPSpien.crn))),GPSpien.crn[,1])
GPSpicoc <- c(rep(NA, n-max(dim(GPSpico.crn))),GPSpico.crn[,1])

WY021pienc <- WY021pien.crn[399:488,1]
WY024pienc <- WY024pien.crn[387:476,1]


## For 1894-1983
# Cbind all of the chronologies, years 1894-1983
x <- as.data.frame(cbind(PR1picoc, PR2pienc, HMRpienc, DGUpicoc, BGOpienc, TB1pienc,TB1psmec, TB2psmec, BC2pienc, BCKpienc,GPSpienc, GPSpicoc)) #DGUpienc,
rownames(x) <- 1460:2014

y <- x[(555-120):(555-31),]
allwy <- cbind(y,WY024pienc,WY021pienc)
rownames(allwy) <- c(1894:1983)

allprcomp <- prcomp(allwy,retx = T, center = T, scale = T)
allplot <- biplot(prcomp(allwy,retx = T, center = T, scale = T), cex = 0.5)
summary(allprcomp)
attributes(allprcomp)

## Removing PR2pien and GPS, including WY021pien and WY024pien 1894-1983
#x2 <- as.data.frame(cbind(PR1picoc, HMRpienc, DGUc, BGOpienc, TB1c, TB2c, BC2pienc, BCKpienc))
#z <- x2[(555-120):(555-31),]
#allwyz <- cbind(z,WY024pienc,WY021pienc)
#rownames(allwyz) <- c(1894:1983)

## Removing WY024pien and WY021pien 
somewy <- x[(555-120):555,]
rownames(somewy) <- c(1894:2014)

## PRCOMP!!!! (cov)
someprcomp <- prcomp(somewy,retx = T, center = T, scale = T)
someplot <- biplot(prcomp(somewy,retx = T, center = T, scale = T), cex = 0.7)
summary(someprcomp)

pc1 <- data.frame(scale(someprcomp$x[,1])) #Originally 1894:2014
pc2 <- data.frame(scale(someprcomp$x[,2]))



########################
## Compare with local climate
# Round TRC lat lon to CRU.nc precision; Subset CRU.nc to latlontimeseries for specified TRClatlon; Correlate by same lat lon TRC and Climate
# cutting out region lon= -108.000 -107.000, lat=	44.00 45.000

##Jet Indices 1930:2012
jet <- read.table('JetIndices.txt', header = TRUE) #1930-2012
jet[,1] <- NULL
pc1jet <- data.frame(pc1[37:119,]) #1930-2012
pc2jet <- data.frame(pc2[37:119,]) 

pc1rcorr <- rcorr(as.matrix(pc2jet), as.matrix(jet))
sigrpc1jet <- data.frame(matrix(NA,nrow = 1,ncol = 33))
colnames(sigrpc1jet) <- colnames(jet[1:33])

for (i in 2:33){
  if(pc1rcorr$P[i,1] < 0.05){
    sigrpc1jet[1,i-1] <- pc1rcorr$r[i,1]
    }
  else {
    sigrpc1jet[1,i-1] <- NA
  }
}

#PC1 is only significant to the AMReg6 jet -0.3167
#PC2 JFReg5+6, AMReg5

pc1clim <- data.frame(pc1[8:121,]) #1901:2014
pc2clim <- data.frame(pc2[8:121,])

## SCPDSI 
scpdsi <- read.table('scpdsibighorns.txt', header = TRUE) #1901:2015
scpdsi <- scpdsi[1:114,] #1901:2014



pc1rcorr <- rcorr(as.matrix(pc2clim), as.matrix(scpdsi))
sigrpc1 <- data.frame(matrix(NA,nrow = 1,ncol = 12))
colnames(sigrpc1) <- colnames(scpdsi[1:12])

for (i in 1:12){
  if(pc1rcorr$P[i+1,1] < 0.05){
    sigrpc1[1,i] <- pc1rcorr$r[i+1,1]
  }
  else {
    sigrpc1[1,i] <- NA
  }
}

## Temperature PRISM Monthly
temp <- read.table('BHtempmonth.txt', header = TRUE) #1901:2014
temp[,1] <- NULL #remove years from temp
pc1rcorr <- rcorr(as.matrix(pc2clim), as.matrix(temp))
sigrpc1 <- data.frame(matrix(NA,nrow = 1,ncol = 12))
colnames(sigrpc1) <- colnames(temp[1:12])


for (i in 1:12){
  if(pc1rcorr$P[i+1,1] < 0.05){
    sigrpc1[1,i] <- pc1rcorr$r[i+1,1]
  }
  else {
    sigrpc1[1,i] <- NA
  }
}


## Precipitation PRISM Monthly
precip <- read.table('BHprecipmonth.txt', header = TRUE) #1901:2015
precip <- precip[1:114,] #1901:2014
precip[,1] <- NULL #remove years from temp
pc1rcorr <- rcorr(as.matrix(pc2clim), as.matrix(precip))
sigrpc1 <- data.frame(matrix(NA,nrow = 1,ncol = 12))
colnames(sigrpc1) <- colnames(precip[1:12])

for (i in 1:12){
  if(pc1rcorr$P[i+1,1] < 0.05){
    sigrpc1[1,i] <- pc1rcorr$r[i+1,1]
  }
  else {
    sigrpc1[1,i] <- NA
  }
}

## Streamflow from Bighorn River near St. Xavier Naturalized flow USGS gage 06287000
streammonth <- read.table('streammonthly.txt', header = TRUE) #1950-1999
streamall <- read.table('streamtotal.txt', header = T) #1936-2013

pc1stream <- data.frame(pc1[57:106,]) #1950-1999
pc2stream <- data.frame(pc2[57:106,]) 

pc1rcorr <- rcorr(as.matrix(pc2stream), as.matrix(streammonth))

pc1streamall <- data.frame(pc1[43:120,]) #1936-2013
pc2streamall <- data.frame(pc2[43:120,]) 
pc1rcorr <- rcorr(as.matrix(pc1streamall), as.matrix(streamall))
# Stream flow was insignificant

####### LOOK AT PREVIOUS YEARS INFO

## Snotel??


##Field Correlation with Geopotential height? or SI-x


## Plot the PCA time series in Comparison with Local Climate
plot.ts(pc1, xlab = 'years', ylab = 'PC2 Time Series', type = 'l')
