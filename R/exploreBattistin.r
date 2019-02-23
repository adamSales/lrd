library(foreign)

datO <- read.dta('data/20071180_data/datapaper_ab.dta')
datO$Z <- datO$esse_m>=0 ##

library(dplyr)
library(ggplot2)
library(tidyr)

ggplot(datO%>%group_by(anno,esse_m,Z)%>%summarize(n=sum(!is.na(lnc)),mf=mean(lnc,na.rm=TRUE)),
       aes(esse_m,mf,size=n,group=Z))+
    geom_point()+geom_smooth()+facet_wrap(~anno)

ggplot(datO,aes(esse_m,lnc,group=Z))+geom_point()+geom_smooth()+facet_wrap(~anno)

dat <- datO%>%filter(anno==2000)%>%select(Y=lnc,R=esse_m,Z,year=anno,wealth=w,educ=educ_m,region,age=eta_m,houseValue=valabit,famSize=ncomp)%>%mutate(wealth=log(wealth-min(wealth,na.rm=TRUE)+1),educC=as.numeric(educ))

ggplot(gather(dat%>%filter(year==2000),key='variable',value='value',-R,-educ,-region,-Z,-year),aes(R,value,group=Z))+
    geom_point()+geom_smooth()+facet_wrap(~variable,scales='free_y')

dat <- na.omit(dat)

library(gam)




mods <- list()
for(ccc in c('wealth','educC','age','houseValue','famSize'))
    mods[[ccc]] <- gam(as.formula(paste(ccc,'~s(R)+Z')),data=dat)

mods$Y <- gam(Y~Z+s(R)+s(wealth)+s(educC)+s(age)+s(houseValue)+s(famSize),data=dat)

R <- dat$R

plotDat <- newDat <- data.frame(R=sort(R),Z=FALSE)
for(ccc in names(mods)){
    plotDat[[ccc]] <- predict(mods[[ccc]],newdata=newDat,type='response')
    mods[[ccc]]$fitted.values <- plotDat[[ccc]]
    newDat[[ccc]] <- simulate(mods[[ccc]])[[1]]
}

for(ccc in names(mods))
    newDat[[ccc]] <- scale(newDat[[ccc]])


plotDat <- plotDat%>%group_by(R)%>%mutate(Y=mean(Y))%>%ungroup()

ggplot(gather(plotDat,'variable','value',-R,-Z),aes(R,value,group=Z))+geom_line()+geom_vline(xintercept=0,linetype='dotted')+facet_wrap(~variable,scales='free_y')

save(R,mods,file='curvedSimMods.RData')

source('R/functions.r')

balMult <- function(dat,BW,method='sh',reduced.covars=TRUE,rhs=NULL,bonferonni=TRUE){
    ps <- NULL
    balTest <- switch(method, "sh"=balOneSH,"cft"=cftTest,"ik"=ikTest)

    xvars <- c('wealth','educC','age','houseValue','famSize')

    if(method=='ik') return(ikMultBal(dat,BW,xvars))

    for(varb in xvars){
        ps <- c(ps,balTest(dat=dat,BW=BW,varb=varb,rhs=rhs))

    }
                                        #if(bonferonni) ps <- ps*length(ps)
    ps <- p.adjust(ps,'hochberg')
    min(1,min(ps))
}


makeDat <- function(){
    newDat <- data.frame(R=sort(R),Z=FALSE)
    for(ccc in names(mods)){
        newDat[[ccc]] <- plotDat[[ccc]]+with(mods[[ccc]],rnorm(nrow(newDat),0,sqrt(var(residuals))))
    }

    for(ccc in names(mods))
        newDat[[ccc]] <- scale(newDat[[ccc]])

    newDat$Z <- newDat$R<0

    newDat
}

anal <- function(nd){
    c(sh=unlist(sh(nd,outcome='Y',Dvar=NULL,bws=1:40)),
      ik=unlist(ik(nd,outcome='Y')))
}

res <- replicate(500,anal(makeDat()))

