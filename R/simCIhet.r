library(knitr)
library(kableExtra)

tryNA <- function(expr,num=1){
    out <- try(expr,silent=TRUE)
    if(inherits(out,'try-error')) return(rep(NA,num))
    out
}

####################################
#### Table 3 simulation (linear)
##############################
#' Create simulated data set
#'
#'  With `curve=3` and `tdist=TRUE` we get the simulation as reported in arxiv v3
#'
#' @param n size (no. obs) in simulation
#' @param curve Amount by which magnitude of slope increases as R passes below -.5
#' @param tdist Generate disturbances from t dist'n on 3 d.f.? (As opposed to std Normal,
#' the default)
#' @param tau Magnitude of simulated treatment effect
#'
#' @return  Data frame w/ running variable R, covariate x, yc, Z, Y
#' @export
#'

makeData <- function(n,tdist=FALSE,frc=0,tauMean=0,tauErr='none',plt=FALSE){
   ## O(n^-.5) contamination fraction OK -> try 1/sqrt(n) contamination
    R <- c(runif(n*(1-frc),-.5,.5),runif(ceiling(n*frc/2),-.75,-.5),runif(ceiling(n*frc/2),.5,.75))
    R <- sample(R,n)
    yc <- .75*R
    yc <- ifelse(abs(R)>0.5,4.5*R-sign(R)*15/8,yc)
    if(plt) yhat <- yc
    err <- if(tdist) rt(n,3) else rnorm(n,0,.75)
    if(tdist) err <- err/sd(err)*0.75
    yc <- yc+err

    Z <- R>0

  tau <- if(tauErr=='t') rt(n,3) else if(tauErr=='norm') rnorm(n,0,.75) else 0

  if(tauErr=='t') tau <- tau/sd(tau)*0.75

  tau <- tau+tauMean

    Y <- yc+Z*tau

    if(plt){
        plot(R,Y)
        lines(sort(R),yhat[order(R)])
    }
    data.frame(R=R,yc=yc,Z=Z,Y=Y)
}




#' Bandwidth, p-value, point estimate from SH method
#'
#' Analyze a simulated data set with the SH method
#'
#' @param dat data frame with variables R, x, Y, Z
#'
#' @return vector with elements
#' \itemize{
#'  \item bandwidth
#'  \item p-value
#'  \item HL point estimate
#' }
#' @export
#'
#'
shbw <- function(ddd,BW=NULL){
    if(missing(BW) | is.null(BW)) BW <- bw(ddd)
    res <- CIsh(dat=ddd,BW=BW)
    p <- testSH(dat=ddd,BW=BW)
    setNames(c(BW,p,res['CI1'],res['CI2'],res['est']),c('BW','p','CIl','CIh','est'))
}


#' Bandwidth p-value, point estimate from IK method
#'
#' Analyze a simulated dataset with the IK method (implemented via rdd package)
#'
#' @import rdd
#'
#' @param dat data frame with variables R, x, Y, Z
#'
#' @return vector with elements
#' \itemize{
#'  \item bandwidth
#'  \item p-value
#'  \item HL point estimate
#' }
#' @export
#'
ikSim <- function(dat,BW=NULL){
    if(missing(BW) | is.null(BW)){
        mod <- RDestimate(Y~R,data=dat,kernel='rectangular')
        return(setNames(with(mod,c(bw[1],p[1],ci[1,1],ci[1,2],est[1])),c('BW','p','CIl','CIh','est')))
    }
    mod <- lm(Y~R*Z,data=dat,subset=abs(R)<=BW)
    est <- coef(mod)['ZTRUE']
    se <- sqrt(vcovHC(mod,'HC1')['ZTRUE','ZTRUE']) ## what RDestimate uses
    setNames(c(BW,2*pnorm(-abs(est/se)),est+c(-1,1)*1.96*se,est),c('BW','p','CIl','CIh','est'))
}

cftSim <- function(dat,BW=NULL){
    if(missing(BW) | is.null(BW)) BW <- bw(dat,method='cft')
    res=cftTest(dat,BW,'Y',justP=FALSE)
    setNames(c(BW=BW,res$p.value,res$conf.int[1],res$conf.int[2],res$estimate),c('BW','p','CIl','CIh','est'))
}



totalOutcomeOne <- function(n,tdist,frc,tauErr,tauMean){
    dat <- makeData(n=n,tdist=tdist,frc=frc,tauErr=tauErr,tauMean=tauMean)
    BW <- max(abs(dat$R))
    c(sh=tryNA(shbw(dat,BW),5),
      ik=tryNA(ikSim(dat,BW),5),
      cft=tryNA(cftSim(dat,BW),5))
}

#' Run the level/power simulation from Table 3
#'
#' @import rdd robustbase
#'
#' @param nreps Number of simulation replications
#'
#' @param cluster For multicore computation with the `parallel` package, an initialized SOCK cluster
#'
#' @return list of output for each simulation run
#' @export
#'
totalOutcomeSim <- function(nreps=5000,cluster=NULL){
    res <- list()
    #B <- 5000

    appFunc <- if(is.null(cluster)) sapply else function(X,FUN) parSapply(cl=cluster,X=X,FUN=FUN)

    for(n in c(50,250,2500)){
      for(tdist in c(TRUE,FALSE)){
        for(tauErr in c('none','some')){
          for(tauMean in c(0,0.2)){
            if(tauErr=='some') tauErr <- ifelse(tdist,'t','norm')
            message(paste(n,tdist,tauErr,tauMean))
            res[[paste0(n,'_','err',ifelse(tdist,'t','norm'),'_','tau',tauMean,'W',tauErr)]] <-
              appFunc(1:nreps,function(i) totalOutcomeOne(n,tdist,frc=0,tauErr=tauErr,tauMean=tauMean))
          }
        }
      }
    }
    res
}






###############
### table 4 simulation (polynomial)
###############
mu3 <- function(x){
    ifelse(x<0,1.27*x-.5*7.18*x^2+0.7*20.21*x^3+1.1*21.54*x^4+1.5*7.33*x^5,
    .84*x-0.1*3.00*x^2-0.3*7.99*x^3-0.1*9.01*x^4+3.56*x^5)
}

### from Wasserman "all of nonparametric stats" p.99 (just the sine function really)
mu4 <- function(x)
  sin(3*x)

makeDataShapes <- function(n,shape,tdist=FALSE,tau=0,plt=FALSE){
    curve <- 3
    R <- runif(n,-1,1)

    yc <- .5*R
    if(shape=='antiSym')
        yc <- ifelse(abs(R)>0.5,3*R+sign(R)*(0.5-3)*0.5,yc)
    if(shape=='oneSide')
        yc <- yc+ ifelse(R> 0.5,3*R+(0.5-3)*0.5,yc)
    if(shape=='sym')
        yc <- ifelse(abs(R)> 0.5,sign(R)*3*R+(sign(R)*0.5-3)*0.5,yc)
    if(shape=='cct')
        yc <- mu3(R)
    if(shape=='poly3')
      yc <- 1.75*R^3
    if(shape=='wass')
      yc <- mu4(R)

    if(plt) plot(R,yc)

    yc <- yc+if(tdist) rt(n,3) else rnorm(n)

    Z <- R>0

    Y <- yc+Z*tau

    data.frame(R=R,Z=Z,Y=Y)
}



ikPoly <- function(dat,deg){
    mod <- lm(Y~poly(R,deg)*Z,data=dat)
    Z.pos <- which(grepl('Z',names(coef(mod))) & !grepl(':',names(coef(mod))))
    setNames(c(p=summary(mod)$coef[Z.pos,4],est=coef(mod)[Z.pos]),
             paste0(c('ik.p.','ik.est.'),deg))
}

shPoly <- function(dat,deg){
    rhs <- paste0('~poly(R,',deg,')+Z')
    setNames(c(p=testSH(dat,1,rhs=rhs),est=HLsh(dat,1,rhs=rhs)),
            paste0(c('sh.p.','sh.est.'),deg))
}

llPoly <- function(dat){
    mod <- RDestimate(Y~R,data=dat)
    setNames(c(mod$p[1],mod$est[1]),c('ll.p','ll.est'))
}


polySim <- function(n,degs=1:5,shape='lin',tdist=TRUE,tau=0){
    dat <- makeDataShapes( n=n,shape=shape,tdist=tdist,tau=tau)

    func <- function(deg){
        c(tryNA(shPoly(dat,deg),2),tryNA(ikPoly(dat,deg),2))
    }

    c(do.call('c',lapply(degs,func)),tryNA(llPoly(dat),2))
}


polyDisp <- function(sim){
    if(nrow(sim)<ncol(sim)) sim <- t(sim)


    simp <- sim[,grep('p',colnames(sim))]
    simEst <- sim[,grep('est',colnames(sim))]
    print('level of test')
    print(apply(simp,2,function(x) mean(x<0.05,na.rm=TRUE)))
    print('mean of est')
    print(apply(simEst,2,mean,na.rm=TRUE))
    print('SD of est')
    print(apply(simEst,2,sd,na.rm=TRUE))
    print('RMSE')
    print(apply(simEst,2,function(x) mean(x^2,na.rm=TRUE)))

}


#' Run the polynomial simulation from Table 4
#'
#' @import robustbase
#'
#' @param nreps Number of simulation replications
#'
#' @return list of output for each simulation run
#' @export
#'
totalPolySim <- function(nreps=5000,cluster=NULL){
  appFunc <- if(is.null(cluster)) sapply else function(X,FUN) parSapply(cl=cluster,X=X,FUN=FUN)
  res <- list()
    #B=5000
    n=500
    tau=0
    degs <- 1:5

    for(shape in c('lin','antiSym','wass')){
        for(tdist in c(TRUE)){#,FALSE)){
            res[[paste0(shape,'_',ifelse(tdist,'t','norm'))]] <-
                appFunc(1:nreps,function(i) polySim(n,degs,shape,tdist,tau))
        }
    }
    res
}



