## run four analyses for our method:
## "main": BW=0.5, keep all data
## donut: BW=0.5, throw out R=0
## data driven BW
## IV

## to be run from main directory

library(xtable)
library(rdd)
library(robustbase)

source('R/functions.r')
source('R/ddsandwich.R')

logit=function(x) log(x*.01/(1-x*.01))
                                #dat=read.csv('LindoDat.csv')

if(!is.element('dat',ls())){
    require(foreign)
    if(!'data'%in%list.files() | !'data_for_analysis.dta'%in%list.files('data/')){
        ## download the data
        temp <- tempfile()
        download.file('https://www.aeaweb.org/aej/app/data/2008-0202_data.zip',temp)
        unzip(temp,'AEJApp2008-0202_data/data_for_analysis.dta',
              junkpaths=TRUE,exdir='data')
    }
  dat=read.dta('data/data_for_analysis.dta')
  dat=subset(dat,left_school!=1)
  dat$dist_from_cut <- round(dat$dist_from_cut,2)
  dat$hsgrade_pct[dat$hsgrade_pct==100]=99.5
  dat$lhsgrade_pct=logit(dat$hsgrade_pct)
  dat$age <- dat$age_at_entry>=19
}                                    #dat=read.csv('LindoDat.csv')


dat$R <- dat$dist_from_cut
dat$Z <- dat$gpalscutoff

## pdf('graphics/figure2_1.pdf')
## smoothplot(dat$dist_from_cut,dat$nextGPA,xlab='First-Year GPA (Distance from Cutoff)',ylab='Avg Subsequent GPA')
## dev.off()

## pdf('graphics/hs_gpa.pdf')
## smoothplot(dat$dist_from_cut,logit(dat$hsgrade_pct),xlab='First Year GPA (Distance from Cutoff)',ylab='Avg logit(hsgrade_pct)')
## dev.off()

dat$hsgrade_pct[dat$hsgrade_pct==100] <- 99.5
dat$lhsgrade_pct <- logit(dat$hsgrade_pct)

#################
## main analysis
#################
## The sh method uses `lmrob`, which in turn requires a random
## seed.  For confidence interval and estimation routines it's
## helpful to use the same seed throughout, as this means the
## S-estimation initializers will always be sampling the same
## subsets of the sample.
set.seed(201705)
lmrob_seed <- .Random.seed


print('1')
## test BW=0.5
balance0.5 <- newBal(dat,0.5, reduced.covars=F)
test0.5 <- test(dat,0.5,outcome='nextGPA')
CI0.5 <- CI(dat,0.5,outcome='nextGPA')

#################
### donut
#################
## donut <- subset(dat,dist_from_cut!=0)
## balanceDonut <- newBal(donut,0.5)
## testDonut <- test(donut,0.5,outcome='nextGPA')
## CIdonut <- CI(donut,0.5,outcome='nextGPA')

###############
### data-driven BW
##############
print(2)
BW <- bwMult(dat, newbal.control=list(method='ancovaHC',reduced.covars=FALSE))
balanceBW <- newBal(dat,BW, reduced.covars=FALSE)
testBW <- test(dat,BW,outcome='nextGPA')
CIBW <- CI(dat,BW,outcome='nextGPA')


###########
### IV
###########
print(3)
datIV <- dat
testCI <- IV(datIV)
#balanceIV <- newBal(testCI$

ciChar <- function(ci,est=FALSE){
    ci.out <- paste('(',round(ci[1],2),',',round(ci[2],2),')',sep='')
    if(est) ci.out <- c(ci.out,as.character(ci[3]))
    ci.out
}

round2 <- function(x) round(x,2)

nfunc <- function(bw) sum(abs(dat$R)<bw,na.rm=TRUE)

resultsTab <-
    rbind(
        main=c(round2(CI0.5['HL']),ciChar(round2(c(CI0.5['CI1'],CI0.5['CI2']))),bw=0.5,n=nfunc(0.5)),
        data_driven=c(round2(CIBW['HL']),ciChar(round2(c(CIBW['CI1'],CIBW['CI2']))),bw=BW,n=nfunc(BW)),
    IV=c(round2(testCI['HL']),ciChar(round2(c(testCI['CI1'],testCI['CI2']))),bw=0.5,n=nfunc(0.5)))

print(xtable(resultsTab),
      file="tab-results.tex", floating=F)



psRocio <- vapply(seq(0.01,0.2,0.01), function(b) newBal(dat,BW=b,method='cft'),1)
bRocio <- max(seq(0.01,0.2,0.01)[psRocio>=0.15],na.rm=TRUE)
#datR <- bw(bRocio,datDN=datSurg)

cftTest <- test(dat,BW=bRocio,method='cft',outcome='nextGPA')
cftCI <- CI(dat,BW=bRocio,method='cft',outcome='nextGPA')

conv <- RDestimate(nextGPA~dist_from_cut,data=dat,cutpoint=-0.005,kernel='rectangular')



altTable <- rbind(
    Local_Permutation=c(round2(cftCI['HL']),ciChar(round2(c(cftCI['CI1'],cftCI['CI2']))),bw=bRocio,n=nfunc(bRocio)),
    Limitless=c(round2(CI0.5['HL']),ciChar(round2(c(CI0.5['CI1'],CI0.5['CI2']))),bw=0.5,n=nfunc(0.5)),
    Local_OLS=c(round2(-conv$est[1]),ciChar(sort(-conv$ci[1,])),bw=round2(conv$bw[1]),n=nfunc(conv$bw[1])))



print(xtable(altTable),
      file="tab-alt.tex", floating=F)



IKbalanceTest <- newBal(dat,1.25)

mccrary1 <- DCdensity(dat$R,-0.005, bin=0.01,plot=FALSE)


save(list=ls(),file='RDanalysis.RData')



