## ----echo=FALSE,include=FALSE--------------------------------------------
knitr::opts_chunk$set(echo=FALSE,results='hide',message=FALSE,error=TRUE,warning=FALSE,cache=FALSE,dev='tikz')

## ----analyses------------------------------------------------------------

library(xtable)
library(Hmisc)

latexSN <- function(x){
    x <- format(x)
    x <- sedit(x, c("e+00", "e-0*", "e-*", "e+0*", "e+*"), c("",
        "$\\\\times 10^{-*}$", "$\\\\times 10^{-*}$", "$\\\\times 10^{*}$",
        "$\\\\times 10^{*}$}"))
    x
}

drs <- list.files()
RDanalysis <- max(drs[grep('RDanalysis',drs)])

if (!exists("analysis_objects")) analysis_objects <- load(RDanalysis)

## ----loadMariaDat,echo=FALSE, include=FALSE------------------------------
library(dplyr)
library(ggplot2)
library(robustbase)
library(pbs)
library(perm)


datM <- read.csv('extdata/death_file_JAMA.csv')


datM$hurYear <- datM$year==2017
datM$year <- as.factor(datM$year)
rownames(datM) <- paste0(month.abb[datM$month_num],substr(datM$year,3,4))

month_lengths  <-
    c(31, 28.25, 31, 30, 31, 30,
      31, 31, 30, 31, 30, 31)

datM$dailyDeath <- datM$deaths_original/month_lengths[datM$month_num]
datM$adjDeath <- datM$dailyDeath*mean(month_lengths)

## are we adjusting deaths for month length??
## choose one of the following:
#dat$deaths <- dat$adjDeath
datM$deaths <- datM$deaths_original


## ----modelMariaDeaths,include=FALSE--------------------------------------
### model monthly deaths prior to 2017
mod  <-   lmrob(deaths~year+
                     pbs(month_num, knots=seq(2,11,by=3)),
                 data=datM,subset=!hurYear)

### predict monthly
fitCurve <- function(x,year='avg'){
  nd <- data.frame(month_num=x,year=ifelse(year=='avg','2010',year))
  out <- predict(mod,nd)
  if(year=='avg') out <- out+sum(coef(mod)[2:7])/8
  out
}


datM$yhat <- NA
datM$yhat[!datM$hurYear] <- predict(mod)
datM$yhat[datM$hurYear] <- fitCurve(datM$month_num[datM$hurYear],'avg')

datM$resid <- datM$deaths-datM$yhat

datM$Z <- datM$month_num>8


## ----bandwidth,include=FALSE,eval=FALSE----------------------------------
## ## are we limiting the window of analysis?
## ## set eval=FALSE if not.
## ## modify the following for a different window:
## window <- 6:11
## datM <- subset(datM,month_num%in%window)
## 

## ----mariaFigs,eval=TRUE,include=FALSE,cache=TRUE------------------------
pd <- rbind(datM,datM)
pd$Deaths <- c(datM$deaths,datM$resid)
pd$type <- factor(c(rep('Death Counts',nrow(datM)),rep('Residual Death Counts',nrow(datM))),
                  levels=c('Death Counts','Residual Death Counts'))
pd$hurYear <- factor(ifelse(pd$hurYear,'2017','2010-2016'))

ggplot(pd,aes(month_num,Deaths,group=year,color=hurYear,size=hurYear,alpha=hurYear))+
  geom_line(size=1)+geom_point()+
  scale_x_continuous(NULL,breaks=1:12,labels=month.abb,minor_breaks=NULL)+ylab(NULL)+
  scale_color_manual(name='Year',values=c('grey','black'))+
  geom_vline(xintercept=9,linetype='dotted')+
  scale_size_discrete(range=c(1,2),guide=FALSE)+scale_alpha_discrete(range=c(.9,1),guide=FALSE)+
  facet_wrap(~type,nrow=1,scales='free_y')+
  geom_hline(data=data.frame(yint=0,type='Residual Death Counts'),aes(yintercept=yint),
    size=2,color='black',linetype='dashed')+
  stat_function(data=data.frame(x=datM$month_num,type='Death Counts',y=datM$deaths,year='avg',hurYear=TRUE),
    aes(x=x,y=y), fun=fitCurve,size=2,color='black',linetype='twodash')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.pos='top')

ggsave('figure/maria.png',width=6.4,height=3,dpi=300)

## ----estimates,include=FALSE---------------------------------------------
N1 <- sum(datM$hurYear&datM$Z)
N0 <- sum(datM$hurYear&!datM$Z)
N <- N1+N0
cardZ <- choose(N,N1)

## adapted from http://rsnippets.blogspot.com/2012/04/generating-all-subsets-of-set.html
all.subsets <- function(set,size) {
  n <- length(set)
  bin <- expand.grid(plyr::rlply(n, c(F, T)))
  if(!missing(size)) bin <- bin[rowSums(bin)==size,]
  bin
}

subs <- all.subsets(1:N,N1)
stopifnot(nrow(subs)==cardZ)

rr <- datM$resid[datM$hurYear]
z <- datM$Z[datM$hurYear]

testStat <- function(Z,RR)
  sum(RR[Z])

ts0 <- apply(subs,1,testStat,RR=rr)
stopifnot(length(unique(ts0))==cardZ) ## no Test stat value appears more than once; simplifying

pfun <- function(tau){
  RR <- rr-z*tau
  ts <- testStat(z,RR)
  TS <- apply(subs,1,testStat,RR=RR)
  nless <- sum(TS<ts)
  nmore <- cardZ-nless-1
  pHigh <- (nmore+.5)/cardZ
  pLow <- (nless+.5)/cardZ
  c(nless=nless,nmore=nmore,p=2*min(pHigh,pLow),diff=abs(mean(TS)-ts))
}

pval <- pfun(0)

tau <- 0
rej <- TRUE
diff <- NULL
while(rej){
  tau <- tau+1
  p <- pfun(tau)
  diff <- rbind(diff,c(tau,p['diff']))
  rej <- p['p']<0.05
}
lb <- tau
p.lower <- p
while(!rej){
  tau <- tau+1
  p <- pfun(tau)
  diff <- rbind(diff,c(tau,p['diff']))
  rej <- p['p']<0.05
}
ub <- tau
p.upper <- p

hl <- diff[which.min(diff[,2]),1]


## ----echo=FALSE,include=FALSE--------------------------------------------
stopifnot(SHmain[['bal.pval']]>.2)
stopifnot(mccrary1<.001)
stopifnot(max(c(frandsen1[,'p'],frandsen2[,'p']))<0.001)

## ----loadSim, results="hide", include=FALSE------------------------------
source('R/simulations.r')
source('R/displaySim.r')
## NOT RUN:
## outcomeSim <- totalOutcomeSim()
## save(outcomeSim,file='dataResults/outcomeSim.RData')
sims <- sort(grep('outcomeSim',list.files(),value=TRUE),decreasing=TRUE)
#load(paste0(dataResults_location, sims[1]))

## ----dgms,fig.height=2,fig.width=6,cache=TRUE,eval=TRUE------------------
#par(mfrow=c(1,3))
dgms()

