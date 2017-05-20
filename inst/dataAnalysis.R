#' run four analyses for our method:
#' main: BW=0.5, throw out R=0
#' nodo: BW=0.5, no exclusion to satisfy McCrary
#' data-driven: adaptive choice of BW
#' IV

#' to be run from main directory

library(xtable)
library(rdd)
library(robustbase)
library(ggplot2)
requireNamespace('lrd')

logit=function(x) log(x*.01/(1-x*.01))


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
    if (system.file(package="lrd")!="") {
        extdata_dir <- system.file("extdata", package="lrd")
    } else extdata_dir <- 'extdata'
    LSO_dta_location <- lrd::fetchLSOdata(extdata_dir)
  dat=foreign::read.dta(LSO_dta_location)
  dat=subset(dat,left_school!=1)
  dat$dist_from_cut <- round(dat$dist_from_cut,2)
  dat$hsgrade_pct[dat$hsgrade_pct==100]=99.5
  dat$lhsgrade_pct=logit(dat$hsgrade_pct)
  dat$age <- dat$age_at_entry>=19
}


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


#' ## main analysis ##

#' The sh method uses `lmrob`, which in turn requires a random
#' seed.  For confidence interval and estimation routines it's
#' helpful to use the same seed throughout, as this means the
#' S-estimation initializers will always be sampling the same
#' subsets of the sample.
set.seed(201705)
lmrob_seed <- .Random.seed


print('1')
#' test BW=0.5
SHmain <- lrd::sh(subset(dat,R!=0),BW=0.5,outcome='nextGPA',Dvar='probation_year1')


#' ## No-donut variant ##

SHnodo <- lrd::sh(dat, BW=0.5, outcome='nextGPA',Dvar='probation_year1')


#' ## data-driven (adaptive) BW ##

print(2)
SHdataDriven <- lrd::sh(dat=subset(dat,R!=0),outcome='nextGPA')


#' ## quadratic in R ## 

print(3)
SHquad <- lrd::sh(dat=subset(dat,R!=0),BW=0.5,outcome='nextGPA',rhs='~Z+poly(R,2)')

###########
#' ## ITT
##########
print(4)
SHitt <- lrd::sh(dat=subset(dat,R!=0),BW=0.5,outcome='nextGPA', Dvar=NULL)




resultsTab <-
    do.call('rbind', lapply(list(main=SHmain,data_driven=SHdataDriven,quad=SHquad,ITT=SHitt),
                            function(res) c(round2(res$CI[3]),
                                            ciChar(res$CI[1:2]),
                                            W=Wfunc(res$W),
                                            n=res$n)))


print(xtable(resultsTab),
      file="tab-results.tex", floating=F)

CFT <- lrd::cft(subset(dat,R!=0),BW=NULL,outcome='nextGPA')
IK <- lrd::ik(subset(dat,R!=0),outcome='nextGPA')

altTab <-
    do.call('rbind', lapply(list(Limitless=SHitt,`Local Permutation`=CFT,`Local OLS`=IK),
                            function(res) c(round2(res$CI[3]),
                                            ciChar(res$CI[1:2]),
                                            W=Wfunc(res$W),
                                            n=res$n)))





print(xtable(altTab),
      file="tab-alt.tex", floating=F)


#' To do: compare robustness weights plots for the next 2 models
lmrob_main <- lmrob(nextGPA~Z+R,
              offset=(SHmain$CI[3]*probation_year1),
              data=dat,subset=(R!=0 & abs(R)<.5),
              method='MM',
              control=lmrob.control(seed=lmrob_seed,
                            k.max=500, maxit.scale=500)
              )

#' Robustness weights are mostly near 1, never below .25. 
robwts_main <- weights(lmrob_main, type="robustness")
summary(robwts_main)

#' Not too much pattern to the robustness weights --
#' although the lowest values do occur at slightly above
#' the cutpoint, where we'd see savvy students whose
#' rose above the cut due to savvyness. 
ggp_main <- ggplot(data.frame(R=lmrob_main$model$R,
                              robweights=robwts_main),
                   aes(x=R,y=robweights))
ggp_main + geom_point(alpha=.1) + stat_smooth()

#' What were the R values for the lowest-weight points?

subset(ggp_main$data, robweights<.3)


#' When we fit without omitting R=0 students, here is
#' the best fitting version of the model.
lmrob_nodo <- lmrob(nextGPA~Z+R,
              offset=(SHnodo$CI[3]*probation_year1),
              data=dat,subset=(abs(R)<.5),
              method='MM',
              control=lmrob.control(seed=lmrob_seed,
                                    k.max=500, maxit.scale=500)
      )

robwts_nodo <- weights(lmrob_nodo, type="robustness")

#' Do we the observations at R=0 stand out?
#' With no donut, robustness weights have a slight tendency
#' to be lower among observations at R=0.
by(robwts_nodo,
   lmrob_nodo$model$R==0, summary)
t.test(wt~atcut, data.frame(wt=robwts_nodo,
                            atcut=(lmrob_nodo$model$R==0)),
       var.equal=F, alternative="g")


#' The plot is similar to that of the main analysis,
#' with some low robustness weight observations as R=0
#' but also plenty of ordinary weight observations there.
ggp_nodo <- ggplot(data.frame(R=lmrob_nodo$model$R,
                              robweights=robwts_nodo),
                   aes(x=R,y=robweights))
ggp_nodo + geom_point(alpha=.1) + stat_smooth()

#' What were the R values for the lowest-weight points?

subset(ggp_nodo$data, robweights<.3)


mccrary1 <- rdd::DCdensity(dat$R,-0.005, bin=0.01,plot=FALSE)
mccraryDougnut <- rdd::DCdensity(dat$R[dat$R!=0],-0.005, bin=0.01,plot=FALSE)

ncomp <- with(dat,sum(gpalscutoff& !probation_year1))
ntot <- nrow(dat)

save(list=ls(),file='RDanalysis.RData')



