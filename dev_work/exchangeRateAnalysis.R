
library("YRmisc")
options(max.print=10000)

tkrList<-c("DEXUSEU","DEXUSUK","DEXINUS","DEXJPUS","DEXCHUS","DGS10","CPIAUCSL","UNRATE")
xrdf<-xd.fred(tkrList,"1960-01-01","2024-12-01")
names(xrdf)<-c("ym","xeur","xbp","xinr","xjy","xcy","g10","cpi","ur")
head(xrdf,10)
tail(xrdf,10)
xrdf<-xd.fred(tkrList,"1970-01-01","2024-12-01")
names(xrdf)<-c("ym","xeur","xbp","xinr","xjy","xcy","g10","cpi","ur")
head(xrdf,100)
tail(xrdf,10)
xrdf<-na.omit(xrdf)
head(xrdf,10)
tail(xrdf,10)

names(xrdf)
dim(xrdf)
data.class(xrdf)
xrdf$obs<-1:nrow(xrdf)

# cv.pctcng does not work robustly. create percent change function
fPctChng<-function(myVector,n){
	pctChange<-myVector/cv.lag(myVector,n)-1
	return(pctChange)
	}

# create inflation variable
xrdf$infla<-fPctChng(xrdf$cpi,12)
par(mfcol=c(2,2)); ts.plot(xrdf$cpi); ts.plot(xrdf$infla); abline(h=0)

xrdf$rreur<- fPctChng(xrdf$xeur,1)
xrdf$rrbp<-fPctChng(xrdf$xbp,1)
xrdf$rrir<-fPctChng(xrdf$xinr,1)
xrdf$rrjy<- fPctChng(xrdf$xjy,1)
xrdf$rrcy<- fPctChng(xrdf$xcy,1)
rrdf<-xrdf[,c("rreur","rrbp","rrir","rrjy","rrcy")]; # rate of return df

# histograms
par(mfcol=c(3,3));  hist(rrdf[,1]); hist(rrdf[,2]); hist(rrdf[,3]); hist(rrdf[,4]); hist(rrdf[,5]);
par(mfcol=c(3,3));  for (i in 1:5){hist(rrdf[,i])}

# tsplots
par(mfcol=c(3,3)); for (i in 1:5){ts.plot(rrdf[,i]); abline(h=0)}
par(mfcol=c(3,3)); sapply(1:5,function(i){ts.plot(rrdf[,i]); abline(h=0)})


# descriptive statistics  
round(apply(rrdf,2,mean,na.rm=T),4)
round(apply(rrdf,2,median,na.rm=T),4)
round(apply(rrdf,2,min,na.rm=T),4)
round(apply(rrdf,2,max,na.rm=T),4)

ds.summ(rrdf,3)

# correlation
round(cor(rrdf,use="complete.obs"),3)

#create seasonality dummy Variables
names(xrdf)
xrdf$mth<-as.numeric(substring(xrdf$ym,6,7))
xrdf$jan<-ifelse(xrdf$mth==1,1,0)
xrdf$feb<-ifelse(xrdf$mth==2,1,0)
xrdf$mar<-ifelse(xrdf$mth==3,1,0)
xrdf$apr<-ifelse(xrdf$mth==4,1,0)
xrdf$may<-ifelse(xrdf$mth==5,1,0)
xrdf$jun<-ifelse(xrdf$mth==6,1,0)
xrdf$jul<-ifelse(xrdf$mth==7,1,0)
xrdf$aug<-ifelse(xrdf$mth==8,1,0)
xrdf$sep<-ifelse(xrdf$mth==9,1,0)
xrdf$oct<-ifelse(xrdf$mth==10,1,0)
xrdf$nov<-ifelse(xrdf$mth==11,1,0)
xrdf$dec<-ifelse(xrdf$mth==12,1,0)
tail(xrdf)

# Pivot tables
ptEUrr<-tapply(xrdf$rreur,xrdf$mth,mean,na.rm=T)
ptBPrr<-tapply(xrdf$rrbp,xrdf$mth,mean,na.rm=T)
ptCYrr<-tapply(xrdf$rrcy,xrdf$mth,mean,na.rm=T)
ptJYrr<-tapply(xrdf$rrjy,xrdf$mth,mean,na.rm=T)
ptIRrr<-tapply(xrdf$rrir,xrdf$mth,mean,na.rm=T)
ptCurrRRbyMnth<-data.frame(mthNo=1:12,eu=ptEUrr,bp=ptBPrr,cy=ptCYrr,jy=ptJYrr,ir=ptIRrr)
round(ptCurrRRbyMnth,4)

# Digress : creating custom functions
addTwoNo<-function(x,y){
  z<-x+y
  return(z) }
addTwoNo(100,400)
addTwoNo(100,400:500)

zFv<-function(pv,r,n){
  fv<-pv*(1+r)^n
  return(fv)  }
zFv(10000,.06,10)
zFv(10000,.06,1:100)