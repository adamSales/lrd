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
print('1')
## test BW=0.5
balance0.5 <- newBal(dat,0.5, method= 'ancovaHC', reduced.covars=F)
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
balanceBW <- newBal(dat,BW, method='ancovaHC',reduced.covars=FALSE)
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



xtable(altTable,caption='Null Hypothesis p-values, 95\\% confidence intervals, and point estimates for our main analysis, compared with the analysis in Cattaneo, et al. (2014), and the conventional local-linear estimate with the Imbens and Kalyanaraman (2012) bandwidth.',label='alt')


IKbalanceTest <- newBal(dat,1.25)

mccrary1 <- DCdensity(dat$R,-0.005, bin=0.01,plot=FALSE)


save(list=ls(),file='RDanalysis.RData')



#################################################################################
### Adam added this: the code to create the simulation results from version 3.
### The actual version 3 simulation results are in the file: data/simResultsV3.RData
### Adam verified that those results are what is reported in the paper.
### Further, the "functions.r" file from 6/29 is the same (in all relevant places)
### as a "functions.r~" saves shortly after the simulation finished running.
### Note that though the submitted paper reported that we had run 5000 runs of the simulation,
### we had actually only run 500.
###################################################################################

### run simulation
#results <- fullOutcomeSim(500,pval=TRUE)
### alternatively, load simulation results
load('data/simResultsV3.RData')


### generate tables
### we did this by hand;
### this code generates the same numbers, in the same configuration, but without the formatting

### Table 3: Empirical Size of Hypothesis Tests
sizeTab <- NULL
ns <- c(50,500,5000)
for(n in ns){
    row <- NULL
    for(meth in c('cft','sh','ik')){
        for(bw in c(0.3,0.5,0.75)){
            row <- c(row, mean(results[[paste(n,TRUE,3,bw,0,sep='_')]][,meth]<=0.05,na.rm=TRUE))
        }
    }
    sizeTab <- rbind(sizeTab,row)
}

sizeTab <- round(sizeTab,2)



### Table 4: Power
powTab <-NULL
for(n in ns){
    row <- NULL
    for(meth in c('cft','sh','ik')){
        for(bw in c(0.3,0.5)){
            row <- c(row, mean(results[[paste(n,TRUE,3,bw,1,sep='_')]][,meth]<=0.05,na.rm=TRUE))
        }
    }
    powTab <- rbind(powTab,row)
}

powTab <- round(powTab,2)
