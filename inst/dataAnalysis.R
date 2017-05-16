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

ciChar <- function(ci,est=FALSE){
    ci <- round(ci,2)
    ci.out <- paste('(',round(ci[1],2),',',round(ci[2],2),')',sep='')
    if(est) ci.out <- c(ci.out,as.character(ci[3]))
    ci.out
}

round2 <- function(x) round(x,2)

nfunc <- function(bw) sum(abs(dat$R)<bw,na.rm=TRUE)

Wfunc <- function(W)
    paste0('[',round2(W[1]), ',',round2(W[2]),')')


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
SHmain <- sh(subset(dat,R!=0),BW=0.5,outcome='nextGPA',Dvar='probation_year1')

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
SHdataDriven <- sh(dat=subset(dat,R!=0),outcome='nextGPA')

##############3
### quadratic in R
###############3
print(3)
SHquad <- sh(dat=subset(dat,R!=0),BW=0.5,outcome='nextGPA',rhs='~Z+poly(R,2)')

###########
### ITT
##########
print(4)
SHitt <- sh(dat=subset(dat,R!=0),BW=0.5,outcome='nextGPA', Dvar=NULL)




resultsTab <-
    do.call('rbind', lapply(list(main=SHmain,data_driven=SHdataDriven,quad=SHquad,ITT=SHitt),
                            function(res) c(round2(res$CI[3]),
                                            ciChar(res$CI[1:2]),
                                            W=Wfunc(res$W),
                                            n=res$n)))


print(xtable(resultsTab),
      file="tab-results.tex", floating=F)

CFT <- cft(subset(dat,R!=0),BW=NULL,outcome='nextGPA')
IK <- ik(subset(dat,R!=0),outcome='nextGPA')

altTab <-
    do.call('rbind', lapply(list(Limitless=SHitt,`Local Permutation`=CFT,`Local OLS`=IK),
                            function(res) c(round2(res$CI[3]),
                                            ciChar(res$CI[1:2]),
                                            W=Wfunc(res$W),
                                            n=res$n)))

## To do: compare robustness weights plots for the next 2 models
modHL <- lmrob(nextGPA~Z+R+offset(CI0.5['HL']*Z),
      data=dat,subset=(abs(R)<.05),
      method='MM',
      control=lmrob.control(seed=lmrob_seed,
                            k.max=500, maxit.scale=500)
      )
modM <- lmrob(nextGPA~Z+R,
      data=dat,subset=(abs(R)<.05),
      method='MM',
      control=lmrob.control(seed=lmrob_seed,
                            k.max=500, maxit.scale=500)
      ) 




print(xtable(altTab),
      file="tab-alt.tex", floating=F)


## To do: compare robustness weights plots for the next 2 models
modHL <- lmrob(nextGPA~Z+R+offset(SHmain$CI[3]*Z),
      data=dat,subset=(abs(R)<.05),
      method='MM',
      control=lmrob.control(seed=lmrob_seed,
                            k.max=500, maxit.scale=500)
      )
modM <- lmrob(nextGPA~Z+R,
      data=dat,subset=(abs(R)<.05),
      method='MM',
      control=lmrob.control(seed=lmrob_seed,
                            k.max=500, maxit.scale=500)
      )






mccrary1 <- DCdensity(dat$R,-0.005, bin=0.01,plot=FALSE)


ncomp <- with(dat,sum(gpalscutoff& !probation_year1))
ntot <- nrow(dat)

save(list=ls(),file='RDanalysis.RData')



