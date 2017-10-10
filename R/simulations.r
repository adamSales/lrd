tryNA <- function(...,num=1){
    out <- try(...)
    if(inherits(out,'try-error')) return(rep(NA,num))
    out
}

####################################
#### Simulation Functions
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
makeData <- function(n,curve,tdist=FALSE,tau=0){
    R <- runif(n,-0.5,0.5)

    yc <- .5*R
    yc <- yc+if(tdist) rt(n,3) else rnorm(n)

    Z <- R>0

    Y <- yc+Z*tau

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
    c(BW=BW,p=testSH(ddd,BW))
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
    if(missing(BW)|is.null(BW)){
        mod <- ikTest(dat, justP=FALSE)
        BW <- mod$bw
        p <- mod$p[1]
    }  else p <- ikTest(dat,BW,justP=TRUE)
    c(BW=BW,p=p)
}

cftSim <- function(dat,BW=NULL){
    if(missing(BW) | is.null(BW)) BW <- bw(dat,method='cft')
    p=cftTest(dat,BW,'Y',justP=TRUE)
    c(BW=BW,p=p)
}


totalOutcomeOne <- function(n,tdist,tau){
    dat <- makeData(n=n,curve=3,tdist=tdist,tau=tau)

    c(sh25=tryNA(shbw(dat,0.25),2),sh5=tryNA(shbw(dat,0.5),2),
      ik25=tryNA(ikSim(dat,0.25),2),ik5=tryNA(ikSim(dat,0.5),2),
      cft25=tryNA(cftSim(dat,0.25),2),cft5=tryNA(cftSim(dat,0.5),2))
}


#' Run the level/power simulation from Table 3
#'
#' @import rdd robustbase
#'
#' @param nreps Number of simulation replications
#'
#' @return list of output for each simulation run
#' @export
#'
totalOutcomeSim <- function(nreps=5000){
    res <- list()
    #B <- 5000


        for(n in c(50,250,2500)){
            for(tau in c(0,0.2)){
                for(tdist in c(TRUE,FALSE)){
                    res[[paste(n,tau,ifelse(tdist,'t','norm'),sep='_')]] <-
                       sapply(1:nreps,function(i) totalOutcomeOne(n,tdist,tau))
                }
            }
        }
    res
}





###############
### polynomial sim
###############
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

    if(plt) plot(R,yc)

    yc <- yc+if(tdist) rt(n,3) else rnorm(n)

    Z <- R>0

    Y <- yc+Z*tau

    data.frame(R=R,Z=Z,Y=Y)
}



ikPoly <- function(dat,deg){
    mod <- lm(Y~poly(R,deg)*Z,data=dat)
    Z.pos <- which(grepl('Z',names(coef(mod))) & !grepl(':',names(coef(mod))))
    c(p=summary(mod)$coef[Z.pos,4],est=coef(mod)[Z.pos])
}

shPoly <- function(dat,deg){
    rhs <- paste0('~poly(R,',deg,')+Z')
    c(p=testSH(dat,1,rhs=rhs),est=HLsh(dat,1,rhs=rhs))
}

polySim <- function(n,degs=1:5,shape='lin',tdist=TRUE,tau=0){
    dat <- makeDataShapes( n=n,shape=shape,tdist=tdist,tau=tau)

    func <- function(deg){
        c(tryNA(shPoly(dat,deg),2),tryNA(ikPoly(dat,deg),2))
    }

    do.call('c',lapply(degs,func))
}


polyDisp <- function(sim){
    if(nrow(sim)<ncol(sim)) sim <- t(sim)

    colnames(sim) <-
        paste0(rep(c('p','est'),ncol(sim)/2),
               rep(seq(ncol(sim)/4),each=4),
               rep(c('sh','sh','ik','ik'),ncol(sim)/4))

    simp <- sim[,grep('p',colnames(sim))]
    simEst <- sim[,grep('est',colnames(sim))]
    print('level of test')
    print(apply(simp,2,function(x) mean(x<0.05,na.rm=TRUE)))
    print('mean of est')
    print(apply(simEst,2,mean,na.rm=TRUE))
    print('SD of est')
    print(apply(simEst,2,sd,na.rm=TRUE))
}


#' Run the polynomial simulation from Table 4 (LRD & OLS, not Local Linear)
#'
#' @import robustbase
#'
#' @param nreps Number of simulation replications
#'
#' @return list of output for each simulation run
#' @export
#'
totalPolySim <- function(nreps=5000){
    res <- list()
    #B=5000
    n=500
    tau=0
    degs <- 1:5

    for(shape in c('lin','antiSym','oneSide')){
        for(tdist in c(TRUE,FALSE)){
            res[[paste0(shape,'_',ifelse(tdist,'t','norm'))]] <-
                sapply(1:nreps,function(i) polySim(n,degs,shape,tdist,tau))
        }
    }
    res
}

polyIKone <- function(n,shape,tdist,tau){
    dat <- makeDataShapes(n=n,shape=shape,tdist=tdist,tau=tau)
    mod <- RDestimate(Y~R,data=dat)
    c(mod$p[1],mod$est[1])
}


#' Run the polynomial simulation from Table 4 (just Local Linear)
#'
#' @import rdd
#'
#' @param nreps Number of simulation replications
#'
#' @return list of output for each simulation run
#' @export
#'
totalPolySimIK <- function(nreps=5000){
    res <- list()
    #B=5000
    n=500
    tau=0
    degs <- 1:5

    for(shape in c('lin','antiSym','oneSide')){
        for(tdist in c(TRUE,FALSE)){
            cat(shape,' ',tdist,'\n')
            print(Sys.time())
            res[[paste0(shape,'_',ifelse(tdist,'t','norm'))]] <-
                sapply(1:nreps,function(i) polyIKone(n,shape,tdist,tau))
        }
    }
    res
}
