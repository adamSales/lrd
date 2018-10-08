library(knitr)
library(kableExtra)

tryNA <- function(expr,num=1){
    out <- try(expr,silent=TRUE)
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
    yc <- yc+if(tdist) rt(n,3)/sqrt(3)*.8 else rnorm(n,0,.8)

    Z <- R>0

    Y <- yc+Z*tau

    data.frame(R=R,yc=yc,Z=Z,Y=Y)
}

makeData2 <- function(n,tdist=FALSE,frc=0,tauErr='none',plt=FALSE){
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


totalOutcomeOne <- function(n,tdist,tau){
    dat <- makeData(n=n,curve=3,tdist=tdist,tau=tau)

    c(sh=tryNA(shbw(dat,0.5),2),
      ik=tryNA(ikSim(dat,0.5),2),
      cft=tryNA(cftSim(dat,0.5),2))
}

totalOutcomeOne2 <- function(n,tdist){
    dat <- makeData2(n=n,tdist=tdist)
    BW <- max(abs(dat$R))
    c(sh=tryNA(shbw(dat,BW),2),
      ik=tryNA(ikSim(dat,BW),2),
      cft=tryNA(cftSim(dat,BW),2))
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
            for(tau in c(0,0.2)){
                for(tdist in c(TRUE,FALSE)){
                    res[[paste(n,tau,ifelse(tdist,'t','norm'),sep='_')]] <-
                       appFunc(1:nreps,function(i) totalOutcomeOne(n,tdist,tau))
                }
            }
        }
    res
}

totalOutcomeSim2 <- function(nreps=5000,cluster=NULL){
    res <- list()
    #B <- 5000

    appFunc <- if(is.null(cluster)) sapply else function(X,FUN) parSapply(cl=cluster,X=X,FUN=FUN)

        for(n in c(50,250,2500)){
            for(tdist in c(TRUE,FALSE)){
                res[[paste(n,0,ifelse(tdist,'t','norm'),sep='_')]] <-
                    appFunc(1:nreps,function(i) totalOutcomeOne2(n,tdist))
            }
        }
    res
}


### make table with bias, coverage, CI width

eachCase <- function(case,eff=0)
    sapply(c('sh','ik','cft'),
           function(method) c(bias=case[paste0(method,'.est')]-eff,
                              cover=case[paste0(method,'.CIl')]<=eff &
                                  case[paste0(method,'.CIh')]>=eff,
                              width=case[paste0(method,'.CIh')]-
                                  case[paste0(method,'.CIl')])
        )

levTabCI <- function(res,eff=0){
    out <- NULL
     for(n in c(50,250,2500))
         for(err in c('norm','t')){
             run <- res[[paste(n,eff,err,sep='_')]]
            row <- NULL
            for(meth in c('cft','sh','ik'))
                row <- c(row,
                         if(is.list(run))
                             mean(sapply(run,function(x) x[paste0(meth,'.p')])<0.05,na.rm=TRUE)
                         else mean(run[paste0(meth,'.p'),]<0.05))

             out <- rbind(out,row)
             rownames(out)[nrow(out)] <- paste(err,n)
         }
    colnames(out) <- c('cft','sh','ik')
    out
}


displayCIsim <- function(res,eff=0){
    ## omit cases where one of the methods (ours?) didn't converge, gave NA
    res <- lapply(res,function(x) if(is.list(x)) do.call('rbind',x[sapply(x,length)==15]) else t(x))



    tab0 <- lapply(res[grep(paste0('_',eff,'_'),names(res),fixed=TRUE)],function(x)
        apply(sapply(1:nrow(x),function(i) eachCase(x[i,],eff=eff),simplify='array'),
              1:2,mean))


    cat('
\\begin{table}
\\footnotesize
\\begin{tabular}{cc|ccc|ccc|ccc}
\\hline

&& \\multicolumn{ 3 }{c}{Permutation}&\\multicolumn{ 3 }{c}{\`\`Limitless\'\'}&\\multicolumn{ 3 }{c}{Local OLS}\\\\
$n$& Error &', paste(rep(c('Bias','Coverage','Width'),3),collapse='&'),'\\\\
\\hline \n')
    for(n in c(50,250,2500))
        for(err in c('norm','t')){
            row <- NULL
            for(meth in c('cft','sh','ik'))
                row <- c(row,tab0[[paste(n,eff,err,sep='_')]][,meth])
            if(err=='norm'){
                cat('\\hline \n')
                cat('\\multirow{2}{*}{',n,'} & $\\mathcal{N}(0,1)$ &')
            } else cat(' & $t_3$ &')
            cat(paste(round(row,2),collapse='&'),'\\\\ \n')
        }
cat('\\hline
\\end{tabular}\n')
#\\caption{',caption,'}
#\\label{',label,'}',sep='')
cat('\\end{table}\n')
}






###############
### polynomial sim
###############
mu3 <- function(x){
    ifelse(x<0,1.27*x-.5*7.18*x^2+0.7*20.21*x^3+1.1*21.54*x^4+1.5*7.33*x^5,
    .84*x-0.1*3.00*x^2-0.3*7.99*x^3-0.1*9.01*x^4+3.56*x^5)
}

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



polySimShape <- function(nreps=5000,shape,cluster=NULL){

    appFunc <- if(is.null(cluster)) sapply else function(X,FUN) parSapply(cl=cluster,X=X,FUN=FUN)

    res <- list()
                                        #B=5000
    n=500
    tau=0
    degs <- 1:5

    for(tdist in c(TRUE,FALSE)){
        res[[paste0(shape,'_',ifelse(tdist,'t','norm'))]] <-
            appFunc(1:nreps,function(i) polySim(n,degs,shape,tdist,tau))
    }

    res
}
