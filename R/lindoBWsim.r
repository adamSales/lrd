library(foreign)
dat <- read.dta('extdata/data_for_analysis.dta')

dat$dist_from_cut <- round(dat$dist_from_cut,2)
dat$hsgrade_pct[dat$hsgrade_pct==100]=99.5
dat$lhsgrade_pct=logit(dat$hsgrade_pct)
                                        #dat$age <- dat$age_at_entry>=19
dat$R <- dat$dist_from_cut
dat$Z <- dat$gpalscutoff
dat$Y <- dat$nextGPA



dat <- na.omit(dat[,c('Y','R','Z','lhsgrade_pct','totcredits_year1','male','loc_campus1','loc_campus2',
           'bpl_north_america','english','age_at_entry')])


## speed things up:
dat <- subset(dat,abs(R)<1)


### estimate relationships:
mods <- list()
for(x in c('lhsgrade_pct','totcredits_year1','male','loc_campus1','loc_campus2',
           'bpl_north_america','english','age_at_entry')){

    if(length(unique(dat[[x]]))==2){
        dat[[x]] <- dat[[x]]==dat[[x]][1]
        fam <- binomial
    } else fam <- gaussian

    mods[[x]] <- gam(as.formula(paste(x,'~s(R)')),data=dat,family=fam)
}




mods$Y <- gam(Y~Z+s(R)+s(lhsgrade_pct)+s(totcredits_year1)+s(age_at_entry)+male+loc_campus1+loc_campus2+
                  bpl_north_america+english,data=dat)

makeDat <- function(){

    newDat <- data.frame(R=dat$R,Z=FALSE)
    modsNew <- mods
    for(ccc in setdiff(names(mods),'Y')){
        modsNew[[ccc]]$fitted.values <- predict(mods[[ccc]],data=newDat,type='response')
        newDat[[ccc]] <- #if(endsWith(ccc,'bin')) rbinom(nrow(newDat),1,modsNew[[ccc]]$fitted.values) else
            simulate(modsNew[[ccc]])[[1]]
    }
    ymod <- modsNew[['Y']]
    ymod$fitted.values <- predict(ymod,data=newDat,type='response')
    newDat$Y <- simulate(ymod)[[1]]

    newDat$Z <- newDat$R<0

    newDat <- newDat[sample(1:nrow(newDat),1000),]
    newDat
}

anal <- function(nd){
    c(sh=unlist(sh(nd,outcome='Y',Dvar=NULL)),
      ik=unlist(ik(nd,outcome='Y')))
}

res <- replicate(500,anal(makeDat()))
