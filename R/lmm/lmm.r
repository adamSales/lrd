library(ggplot2)
library(dplyr)
library(R2jags)
library(foreign)

dat <- read.dta('extdata/data_for_analysis.dta')

datO <- dat
dat <- subset(dat,dist_from_cut!=0)

gpa <- dat$hsgrade_pct
gpa[gpa==100] <- 99
gpa <- gpa/100
dat$lhsgrade_pct <- plogis(gpa)

jagsDat <- with(dat,
                list(N= nrow(dat),
                     Z=gpalscutoff,
                     gpa=lhsgrade_pct,
                     credits=totcredits_year1,
                     male=male,
                     bpl=bpl_north_america,
                     english=english,
                     campus=ifelse(loc_campus1==1,1,
                         ifelse(loc_campus2==1,2,3))))

params <- c('balGPA','balcredits','balMale','balBpl','balEnglish','balCampus1','balCampus2','gammaGPA0','gammaGPA1','gammacredits0','gammacredits1','gammaMale0','gammaMale1','gammaBpl0','gammaBpl1','gammaEnglish0','gammaEnglish1','gammaCampus01','gammaCampus11','gammaCampus02','gammaCampus12','mu0','tau0','mu1','tau1','pi','tauGPA','taucredits')

### NOT RUN: (tried it on whole dataset to see if it worked)
# mod <- jags.parallel(jagsDat,parameters=params,model='R/lmm/lmmBalance.bug')


jagsDat2 <- with(subset(dat,abs(dist_from_cut)<0.18),
                 list(Z=gpalscutoff,
                     gpa=lhsgrade_pct/sd(lhsgrade_pct),
                     credits=totcredits_year1/sd(totcredits_year1),
                     male=male,
                     bpl=bpl_north_america,
                     english=english,
                     campus=ifelse(loc_campus1==1,1,
                         ifelse(loc_campus2==1,2,3))))
jagsDat2$N <- length(jagsDat2$Z)

## NOT RUN: (tried on a smaller dataset)
# mod <- jags.parallel(jagsDat2,parameters=params,model='R/lmm/lmmBalance.bug')

## p-values
library(RItools)
pvals <- vapply(seq(0.01,0.5,by=0.01),function(bw)
    xBalance(gpalscutoff~lhsgrade_pct+totcredits_year1+male+bpl_north_america+english+loc_campus1+loc_campus2,data=subset(dat,abs(dist_from_cut)<=bw),report='chisquare.test')$overall$p.value,1)
plot(seq(0.01,0.5,by=0.01),pvals)
cbind(seq(0.01,0.5,by=0.01),round(pvals,3))


balTestLMM <- function(bw){
    jagsDat2 <- with(subset(dat,abs(dist_from_cut)<=bw),
                 list(Z=gpalscutoff,
                     gpa=lhsgrade_pct/sd(lhsgrade_pct),
                     credits=totcredits_year1/sd(totcredits_year1),
                     male=male,
                     bpl=bpl_north_america,
                     english=english,
                     campus=ifelse(loc_campus1==1,1,
                         ifelse(loc_campus2==1,2,3))))
    jagsDat2$N <- length(jagsDat2$Z)

    mod <- jags.parallel(jagsDat2,parameters=c('balGPA','balcredits','balMale','balBpl','balEnglish','balCampus1','balCampus2','pi'),model='R/lmmBalance.bug',n.iter=3000,n.burnin=2000,n.chains=3) ## pi added after first go-round
    mod$BUGSoutput$summary
}

bws <- seq(0.02,0.5,by=0.05)
baltests <- lapply(bws,balTestLMM)
save(baltests,bws,file='inst/lmmBalance.RData')

## best bandwidth: 0.17

### outcome analysis
## basically this:
summary(mod <- lm(nextGPA~gpalscutoff*dist_from_cut+bpl_north_america+lhsgrade_pct+totcredits_year1+male+english+loc_campus1+loc_campus2,data=dat,subset=abs(dist_from_cut)<=0.17))

dat%>%filter(abs(dist_from_cut)<=.17)%>%group_by(dist_from_cut)%>%summarize(y=mean(nextGPA,na.rm=TRUE),n=n(),Z=dist_from_cut[1]<0)%>%
    ggplot(aes(dist_from_cut,y,size=n,color=Z))+geom_point()+geom_smooth( mapping=aes(x=dist_from_cut,y=nextGPA,group=gpalscutoff,color=gpalscutoff==1),data=subset(dat,abs(dist_from_cut)<=0.17),method='lm',inherit.aes=FALSE)

## bayesian style
outDat <- with(subset(dat,abs(dist_from_cut)<=0.17),
               list(
                   nextGPA=nextGPA,
                   R=dist_from_cut,
                   Z=gpalscutoff,
                   gpa=lhsgrade_pct/sd(lhsgrade_pct),
                   credits=totcredits_year1/sd(totcredits_year1),
                   male=male,
                   bpl=bpl_north_america,
                   english=english,
                   campus1=loc_campus1,
                   campus2=loc_campus2))
outDat$N <- length(outDat$R)

outMod <- jags.parallel(outDat,parameters=c('b','tau','ate'),model='R/lmm/lmmOutcome.bug',n.chains=3)

library(rstanarm)
outModStan <-
    stan_glm(nextGPA~gpalscutoff*dist_from_cut+I(lhsgrade_pct/sd(lhsgrade_pct))+I(totcredits_year1/sd(totcredits_year1))+male+bpl_north_america+english+loc_campus1+loc_campus2,
            data=subset(dat,abs(dist_from_cut)<=0.17),prior=normal(0,1),family=gaussian)

outModStan2 <- stan_glm(nextGPA~gpalscutoff,prior=normal(scale=1),data=subset(dat,abs(dist_from_cut)<=0.17),family=gaussian)
