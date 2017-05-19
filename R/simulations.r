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
    R <- runif(n,-1,1)

    x <- R+ifelse(abs(R) > 0.5,curve*R-sign(R)*.5*curve,0)
    if(tdist) x <- x+rt(n,3)
    else x <- x+rnorm(n)

    yc <- .5*R+ifelse(abs(R) > 0.5,curve*R-sign(R)*.5*curve,0)
    yc <- yc+if(tdist) rt(n,3) else rnorm(n)

    Z <- R>0

    Y <- yc+Z*tau

    data.frame(R=R,x=x,yc=yc,Z=Z,Y=Y)
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
    c(BW=BW,p=testSH(ddd,BW),est=HLsh(ddd,BW))
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
    mod <- if(missing(BW)|is.null(BW)) ikTest(dat, justP=FALSE) else ikTest(dat,BW,justP=FALSE)
    c(BW=mod$bw[1],p=mod$p[1],est=mod$est[1])
}

cftSim <- function(dat,BW=NULL){
    if(missing(BW) | is.null(BW)) BW <- bw(dat,method='cft')
    mod=cftTest(dat,BW,'Y',justP=FALSE)
    c(BW=BW,p=mod$p.value,est=mod$estimate)
}


totalOutcomeOne <- function(n,tdist,tau){
    dat <- makeData(n=n,curve=3,tdist=tdist,tau=tau)

    c(sh25=tryNA(shbw(dat,0.25)),sh5=tryNA(shbw(dat,0.5)),shQ=tryNA(shbw(dat)),
      ik25=tryNA(ikSim(dat,0.25)),ik5=tryNA(ikSim(dat,0.5)),shQ=tryNA(ikSim(dat)),
      cft25=tryNA(cftSim(dat,0.25)),cft5=tryNA(cftSim(dat,0.5)),cftQ=tryNA(cftSim(dat)))
}

totalOutcomeSim <- function(nreps=5000){
    res <- list()
    #B <- 5000


        for(n in c(100,500,5000)){
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



totalPolySim <- function(){
    res <- list()
    B=5000
    n=500
    tau=0
    degs <- 1:5

    for(shape in c('lin','antiSym','oneSide')){
        for(tdist in c(TRUE,FALSE)){
            res[[paste0(shape,'_',ifelse(tdist,'t','norm'))]] <-
                sapply(1:B,function(i) polySim(n,degs,shape,tdist,tau))
        }
    }
    res
}

polyIKone <- function(n,shape,tdist,tau){
    dat <- makeDataShapes(n=n,shape=shape,tdist=tdist,tau=tau)
    mod <- RDestimate(Y~R,data=dat)
    c(mod$p[1],mod$est[1])
}

totalPolySimIK <- function(){
    res <- list()
    B=5000
    n=500
    tau=0
    degs <- 1:5

    for(shape in c('lin','antiSym','oneSide')){
        for(tdist in c(TRUE,FALSE)){
            cat(shape,' ',tdist,'\n')
            print(Sys.time())
            res[[paste0(shape,'_',ifelse(tdist,'t','norm'))]] <-
                sapply(1:B,function(i) polyIKone(n,shape,tdist,tau))
        }
    }
    res
}
