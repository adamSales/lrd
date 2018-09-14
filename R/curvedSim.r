## take model 3 from CCT

mu3 <- function(x){
    ifelse(x<0,1.27*x-.5*7.18*x^2+0.7*20.21*x^3+1.1*21.54*x^4+1.5*7.33*x^5,
    .84*x-0.1*3.00*x^2-0.3*7.99*x^3-0.1*9.01*x^4+3.56*x^5)
}

mu4 <- function(x,crv){
    .5*x+crv*sin(x*pi)
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
makeData <- function(n,crv,tdist=FALSE,tau=0){
    R <- runif(n,-1,1)

    x <- crv*sin(R*pi)+if(tdist) rt(n,3) else rnorm(n)

    yc <- .5*x+.5*R

    yc <- yc+if(tdist) rt(n,3) else rnorm(n)

    Z <- R>0

    Y <- yc+Z*tau

    data.frame(R=R,yc=yc,Z=Z,Y=Y,x=x)
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
    if(missing(BW) | is.null(BW)) BW <- bw(ddd,alphax=.15)
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
        BW <- mod$bw[1]
        p <- mod$p[1]
    }  else p <- ikTest(dat,BW,justP=TRUE)
    c(BW=BW,p=p)
}


ikSim <- function(dat,BW=NULL){
    mod <- RDestimate(Y~R,data=dat,bw=BW)
    BW <- mod$bw[1]
    p <- mod$p[1]

    c(BW=BW,p=p)
}


cftSim <- function(dat,BW=NULL){
    if(missing(BW) | is.null(BW)) BW <- bw(dat,method='cft')
    p=cftTest(dat,BW,'Y',justP=TRUE)
    c(BW=BW,p=p)
}


totalOutcomeOne <- function(n,tdist,tau){
    dat <- makeData(n=n,curve=3,tdist=tdist,tau=tau)

    c(sh=tryNA(shbw(dat,0.5),2),
      ik=tryNA(ikSim(dat,0.5),2),
      cft=tryNA(cftSim(dat,0.5),2))
}


curvedSimOne <- function(n,tdist,crv){

    dat <- makeData(n=n,crv=crv,tdist=tdist)
    c(sh=tryNA(shbw(dat),2),
      ik=tryNA(ikSim(dat),2),
      cct=tryNA(with(rdrobust(dat$Y,dat$R),c(BW=bws[2,1],p=pv[3])),2))

}

system.time(s1 <- mclapply(1:500,function(i) curvedSimOne(500,F,1),mc.cores=3))
s11 <- do.call('rbind',s1)

boxplot(s11[,'sh.BW'],s11[,'ik.BW'],s11[,'cct.BW'])

mean(s11[,'sh.p']<0.05,na.rm=T)
mean(s11[,'ik.p']<0.05)
mean(s11[,'cct.p']<0.05)
