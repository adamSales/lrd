
##---------------------------
## Library directives needed for various functions below.  (As 
## dependencies are identified, associate them w/ functions, as in 
## the Roxygen documentation preceding `sh` below.)
## library(nnet)
## library(RItools)
## library(rdd)
## library(robustbase)
## library(parallelsugar)
##---------------------------



#' Create simulated data set
#'
#'  With `curve=3` and `tdist=TRUE` we get the simulation as reported in arxiv v3
#'
#' @param n size (no. obs) in simulation
#' @param curve Amount by which magnitude of slope increases as R passes below -.5
#' @param tdist Generate disturbances from t dist'n on 3 d.f.? (As opposed to std Normal, the default)
#' @param tau Magnitude of simulated treatment effect
#'
#' @return  Data frame w/ running variable R, covariate x, yc, Z, Y
#' @export
#'
makeData <- function(n,curve,tdist=FALSE,tau=0){
    R <- runif(n,-1,1)

    x <- R+ifelse(R< -0.5,curve*R-sign(R)*.5*curve,0)
    if(tdist) x <- x+rt(n,3)
    else x <- x+rnorm(n)

    yc <- .5*R+ ifelse(R< -0.5,curve*R-sign(R)*.5*curve,0)
    yc <- yc+if(tdist) rt(n,3) else rnorm(n)

    Z <- R>0

    Y <- yc+Z*tau

    data.frame(R=R,x=x,yc=yc,Z=Z,Y=Y)
}

mallik <- function(pvals,bws){
    if(sum(is.na(pvals))>0){
        bws <- bws[!is.na(pvals)]
        pvals <- pvals[!is.na(pvals)]
    }
    mallik2=function(d,pvals){
	sum(pvals[1:d])-d*.25
    }
    bws[which.max(vapply(1:length(bws),mallik2,1,pvals=pvals))]
}

bw <- function(data,covname='x',method='sh',mallik=FALSE){

    short <- function(w) test(data,w,outcome=covname,method=method)
    if(method=='ik') return(IKbandwidth(dat$R,dat$Y,kernel='rectangular'))

    bws <- seq(0,1,0.01)
    pvals <- vapply(bws,short,1)
    alpha <- ifelse(method=='cft',0.15,0.15)
    if(all(is.na(pvals)) | all(pvals<alpha,na.rm=TRUE)) return(0)
 #   if(mallik) return(mallik(ps,bws))
    max(bws[pvals>alpha],na.rm=TRUE)
}

bwMult <- function(dat,alpha=0.15){
    short <- function(w) try(newBal(dat,w))

    bws <- seq(.3,2,0.01)
    pvals <- vapply(bws,short,1)
    if(all(is.na(pvals)) | all(pvals<alpha,na.rm=TRUE)) return(0)
    max(bws[pvals>alpha],na.rm=TRUE)
}

test <- function(data, BW,tau=0,outcome='Y',method='sh'){
    if(method=='ik') return(ik(data,BW)['pval'])
    if(sum(data$R<0 & -BW<data$R)<2 || sum(data$R>0 & data$R<BW)<2) return(NA)
    dat <- data[abs(data$R)<BW,]
    if('D'%in%names(dat)) dat$ytilde <- dat[[outcome]]-tau*dat$D
    else dat$ytilde <- dat[[outcome]]-tau*dat$Z
    meth <- eval(parse(text=method),envir=-1)
    meth(dat)
}

#' Robust balance test following robust covariate adjustment 
#' 
#' The test is the test associated with the Z coefficient after fitting
#' a model from the robustbase suite that uses Z and R as independent variables.
#' Covariance adjustment is via robust logistic regression if 
#' ytilde is binary, robust linear regression otherwise. In the latter case,
#' since we're using `robustbase::lmrob` with the `MM` method, the SE used 
#' in the test is of the Huber-White type, allowing for heteroskedasticity 
#' and accounting for  propagation of errors from the preliminary fitting
#' done by the robust regression routine. 
#' 
#' This is the method recommended in version 2 of Limitless 
#' Regression Discontinuity
#'
#' @param dat Data set with columns \code{ytilde}, \code{Z}, \code{R}
#'
#' @return scalar, the p-value associated w/ coefficient on Z
#' @imports robustbase 
#' @export
sh <- function(dat){
  ctl <- lmrob.control(k.max=500,maxit.scale=500)
    if(length(unique(dat$ytilde))>2)
        mod <- lmrob(ytilde~Z+R,data=dat,method='MM',control=ctl)
    else mod <- glmrob(ytilde~Z+R,data=dat,family=binomial)
    summary(mod)$coef[2,4]
}

cft <- function(dat){
    return(with(dat,wilcox.test(ytilde~Z)$p.value))
}

orig <- function(dat,multBal=FALSE){
    logistic <- FALSE
    ctl <- lmrob.control(k.max=500,maxit.scale=500)#,setting= "KS2014")
    if(isTRUE(all.equal(sort(unique(dat$ytilde)),c(0,1)))) logistic <- TRUE
    if(logistic) mod <- glmrob(ytilde~R,family=binomial(logit),data=dat,control=ctl,method='MM')
    else mod <- lmrob(ytilde~R,data=dat,control=ctl,method='MM')
    dat$Ydt <- residuals(mod) #* weights(mod, type="robustness")
    if(multBal) return(dat$Ydt)

    xBalance(Z~Ydt,data=dat,report='chisquare.test')$overall$p.value
}

CI <- function(data,BW,grid=seq(-1,1,0.01),alpha=0.05,method='sh',outcome='Y'){
    if(method=='ik')
        if(missing(BW)) return(ik(dat)[1:3]) else return(ik(dat,BW)[1:3])
    shortFunction <- function(tau) test(data,BW,tau,outcome=outcome)
    pvals <- vapply(grid,shortFunction,1)
    if(all(is.na(pvals))) return(rep(NA,3))
    c(CI1=min(grid[pvals>alpha]),CI2=max(grid[pvals>alpha]),HL=grid[which.max(pvals)])
}


estimate <- function(dat,method,justTest=FALSE,outcome,running,treat,bwmethod=method){

    # DRY fail
    if(!missing(outcome)) dat$Y <- dat[[outcome]]
    if(!missing(running)) dat$R <- dat[[running]]
    if(!missing(treat)) dat$Z <- dat[[treat]]

    BW <- bw(dat,method=bwmethod)

    pval <- test(dat,BW,method=method)

    ci <- CI(dat,BW,method=method)

    c(ci,pval,BW)
}

ik <- function(dat,BW){
    if(missing(BW))  mod <- try(RDestimate(Y~R,kernel='rectangular',data=dat))
    else  mod <- try(RDestimate(Y~R,kernel='rectangular',data=dat,bw=BW))
    if(class(mod)=='try-error') return(rep(NA,5))
    c(CI1=mod$ci[1,1],CI2=mod$ci[1,2],HL=mod$est[1],pval=mod$p[1],bw=mod$bw[1])
}

processRun <- function(n,curve,tdist,tau=0.3){
    dat <- makeData(n,curve,tdist,tau=tau)
    rbind(
        SH=estimate(dat,'sh'),
        CFT=estimate(dat,'cft'),
        IK=ik(dat),
        shOrig=estimate(dat,'sh',bwmethod='orig')
        )
}




oneCellProc <- function(B,n,curve,tdist,tau=0.3){
    run <- function(i) processRun(n,curve,tdist,tau)

    res <- lapply(1:B,run,mc.cores=parallel::detectCores()-2)
#    res <- do.call('rbind',res)
    res
}


fullSimProc <- function(B=500){
    results <- list()
    for(tdist in c(FALSE,TRUE))
        for(n in c(25,250,2500))
            for(curve in c(1,3)){
                print(paste(n,tdist,curve))
                results[[paste(n,tdist,curve,sep='_')]] <- oneCellProc(B,n,curve,tdist)
        }
    results
}

summ <- function(res,stat){
    vapply(res,stat,numeric(4))
}

pointEst <- function(run) run[,3]

width <- function(run) run[,2]-run[,1]

bandwidth <- function(run) run[,5]

p.value <- function(run) run[,4]

contain <- function(run) run[,1]<0.3 & run[,2]>0.3

summarizeSim <- function(results,stat,PLOT=FALSE){

    out <- NULL
    for(i in 1:length(results)){
        SUM <- summ(results[[i]],stat)
        if(PLOT)
        if(as.character(match.call()[[3]])=='p.value')
                    out <- rbind(out,apply(SUM,1,function(a) mean(a<0.05,na.rm=TRUE)))
        else out <- rbind(out,apply(SUM,1,mean,na.rm=TRUE))
    }

    rownames(out) <- names(results)

    out
}

plotSim <- function(results,runNum){

    res <- do.call('rbind',results[[runNum]])
    res <- data.frame(res,meth=rownames(res))
    boxplot(HL~meth,data=res,main=names(results)[runNum])
    abline(h=0.3,lty=2)
}

outcomeSimOne <- function(n,curve,tdist,BW,tau=0,pval=TRUE){
    dat <- makeData(n=n,curve=curve,tdist=tdist,tau=tau)

    if(pval) func <- function(method) test(data=dat,BW=BW,method=method)
    else func <- function(method) CI(dat,BW,method=method)

    c(vapply(c('sh','cft','ik'),func,numeric(ifelse(pval,1,3))),
      n,curve,tdist,BW,tau)
}


outcomeSimOneCell <- function(B,n,curve,tdist,tau=0,pval=TRUE,BW){
    results <- NULL
#    for(BW in c(0.25,0.5,1)){
        func <- function(i) outcomeSimOne(n,curve,tdist,BW,tau=tau,pval=pval)
        res <- do.call('rbind',lapply(1:B,func))#clusterApply(cl,1:B,func))
        results <- rbind(results,res)
 #   }
    results
}


fullOutcomeSim <- function(B,pval=TRUE){
    results <- list()
    tdist <- TRUE
    curve <- 3
    #for(tdist in c(FALSE,TRUE))
        for(n in c(50,500,5000))
    #        for(curve in c(1,3,5))
                for(tau in c(0,1))
                    for(BW in c(0.3,0.5,0.75,1)){
                            print(paste(n,tdist,curve))
                            results[[paste(n,tdist,curve,BW,tau,sep='_')]] <-
                                outcomeSimOneCell(B=B,n=n,curve=curve,tdist=tdist,BW=BW,tau=tau)
                        }
    save(results,file='outcomeSimFinal.RData')
    results
}


printResults <- function(subs){
        for(tdist in c(FALSE,TRUE))
            for(n in c(50,500,5000))
                for(curve in c(1,3,5))
                    for(tau in c(0,0.5,1))
                        for(BW in c(0.3,0.5,0.75,1)){
                            SUBS <- eval(parse(text=subs))
                            if(SUBS){
                                res <- results[[paste(n,tdist,curve,BW,tau,sep='_')]]
                                print(paste(n,tdist,curve,BW,tau,sep='_'))
                                print(apply(res,2,function(col) mean(col<0.05)))}

                        }
    }

### this is specific to LSO data

multBal <- function(dat,BW){

    dat <- dat[abs(dat$R)<BW,]

    balDat <- data.frame(Z=dat$Z)

    balDat$hsgpa <- benDT(dat,'lhsgrade_pct')
    balDat$age <-  benDT(dat,'age')
    balDat$credits <-  benDT(dat,'totcredits_year1')
    balDat$male <-  benDT(dat,'male')
    balDat$camp1 <-  benDT(dat,'loc_campus1')
    balDat$camp2 <- benDT(dat,'loc_campus1')
    balDat$north <- benDT(dat,'bpl_north_america')
    balDat$eng <-  benDT(dat,'english')

    balDat <<- balDat

    xBalance(Z~.,data=balDat,report='chisquare.test')$overall$p.value
}

IV <- function(dat){
  #  dat$Z <- ifelse(dat$totcredits_year1==4,
  #                  dat$R<(dat$gpacutoff*5-1)/4-dat$gpacutoff,dat$gpalscutoff)

    dat$Z <- dat$R< ifelse(dat$totcredits_year1<=4,0.2,0)


    dat$D <- dat$probation_year1

    c(test(dat,0.5,outcome='nextGPA'),CI(dat,0.5,outcome='nextGPA'))
}

benDT <- function(dat,varb){
    dat$Z <- as.numeric(dat$Z)
    dat$ytilde <- dat[[varb]]
    if(length(unique(dat[[varb]]))==2)
        mod <- glmrob(ytilde~Z+R,data=dat,family=binomial)#,setting = "KS2014")
    else mod <- lmrob(ytilde~Z+R,data=dat,control=ctl,setting = "KS2014")

    dat$ytilde - predict(mod, newdata=transform(dat, Z=0),type='response')
}

ben <- function(dat){
  ctl <- lmrob.control(k.max=500,maxit.scale=500)#,setting= "KS2014")
    dat$Z <- as.numeric(dat$Z)
    mod <- lmrob(ytilde~Z+R,data=dat,method='MM',control=ctl)

    dat$Ydt <- dat$ytilde - predict(mod, newdata=transform(dat, Z=0))

    xBalance(Z~Ydt,data=dat,report='chisquare.test')$overall$p.value
}


outcomeSimTable <- function(results,power){

    runs <- strsplit(names(results),'_')
    if(power) keep <- which(vapply(runs,function(r) r[5]=='1' & r[4]%in% c('0.3','0.5'),TRUE))
    else keep <- which(vapply(runs,function(r) r[5]=='0',TRUE))

    tab <- NULL
    for(i in keep){
        tab <- rbind(tab,
                     c(apply(results[[i]],2,function(col) mean(col<0.05,na.rm=TRUE))[1:3],
                       bw=results[[i]][1,7],n=results[[i]][1,4]))

    }
    #colnames(tab) <- c('Limitless','Local Rand.','Local OLS','bw','n')

    #print(round(tab,2))
    newTab <- NULL
    for(meth in c('cft','sh','ik')){
            small <- tab[,meth]
            for(b in unique(tab[,'bw']))
                newTab <- cbind(newTab,small[tab[,'bw']==b])
        }
    rownames(newTab) <- paste('n=',unique(tab[,'n']))
    colnames(newTab) <- NULL

    al <- ifelse(power,
                 'r|cc|cc|cc',
                 'r|cccc|cccc|cccc')
    rrr <- ifelse(power,2,4)
    addtorow <- list()
    addtorow$pos <- list(0, 0)
    addtorow$command <- c(paste('&\\multicolumn{',rrr,'}{c}{Local Rand.}&\\multicolumn{',rrr,'}{c}{Limitless}&\\multicolumn{',rrr,'}{c}{Local OLS}\\\\\n'),
        paste(do.call('paste',as.list(paste('&bw=',rep(unique(tab[,'bw']),3)))),'\\\\\n'))
    print(xtable(newTab,align=al), add.to.row = addtorow, include.colnames = FALSE)
    round(newTab,2)


}

newBal <- function(dat,BW,method='sh'){
    ps <- NULL
    for(varb in c('lhsgrade_pct','totcredits_year1','male','loc_campus1','loc_campus2','bpl_north_america','english','age')){
        print(varb)
        ps <- c(ps,test(dat,BW,outcome=varb,method=method))

    }
    min(ps*length(ps))
}
