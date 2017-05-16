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


makeDataSort <- function(n,tdist=FALSE,tau=0,sortPer=0.1){
    R <- runif(n,-1,1)
    R <- round(R,2)

    yc <- R+if(tdist) rt(n,3) else rnorm(n)

    sorter <- sample(1:n,n*sortPer,replace=FALSE)

    Robs <- R
    Robs[sorter] <- ifelse(R[sorter]<0,0,R[sorter])

    Z <- Robs< 0

    Y <- yc+Z*tau

    data.frame(R=Robs,Y=Y,Z=Z)
}



#' Bandwidth, p-value, point estimate from SH method
#'
#' Analyze a simulated data set with the SH method
#'
#' @param dat data frame with variables R, x, Y, Z
#'
#' @return vector with elements
#'  \item bandwidth
#'  \item p-value
#'  \item HL point estimate
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
#'  \item bandwidth
#'  \item p-value
#'  \item HL point estimate
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

Outcomesimone <- function(n,curve,tdist,BW,tau=0,pval=TRUE){
    dat <- makeData(n=n,curve=curve,tdist=tdist,tau=tau)

    if(pval) func <- function(test) test(dat=dat,BW=BW)
    else func <- function(method) CI(dat,BW,method=method)

    setNames(c(vapply(list(testSH,ikTest,cftTest),func,numeric(ifelse(pval,1,3))),
      n,curve,tdist,BW,tau),c('sh','ik','cft','n','curve','tdist','BW','tau'))
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

outcomeSimFlexBW <- function(n,curve,tdist,tau,pval=TRUE){
    dat <- makeData(n=n,curve=curve,tdist=tdist,tau=tau)

    func <- function(test) test(dat)

   c( vapply(list(shbw,ikSim),
                                        #,cftTest),
                      func,numeric(3)),
               n,curve,tdist,tau)#,c('sh','ik',
                                    #'cft',
                                  #  'n','curve','tdist','tau'))
}

totalOutcomeOne <- function(n,tdist,tau){
    dat <- makeData(n=n,curve=3,tdist=tdist,tau=tau)

    c(sh25=tryNA(shbw(dat,0.25)),sh5=tryNA(shbw(dat,0.5)),shQ=tryNA(shbw(dat)),
      ik25=tryNA(ikSim(dat,0.25)),ik5=tryNA(ikSim(dat,0.5)),shQ=tryNA(ikSim(dat)),
      cft25=tryNA(cftSim(dat,0.25)),cft5=tryNA(cftSim(dat,0.5)),cftQ=tryNA(cftSim(dat)))
}

totalOutcomeSim <- function(){
    res <- list()
    B <- 5000


        for(n in c(100,500,5000)){
            for(tau in c(0,0.2)){
                for(tdist in c(TRUE,FALSE)){
                    res[[paste(n,tau,ifelse(tdist,'t','norm'),sep='_')]] <-
                       sapply(1:B,function(i) totalOutcomeOne(n,tdist,tau))
                }
            }
        }
    res
}


outcomeSimFlexBWbig <- function(n,curve,tdist,tau){
    dat <- makeData(n=n,curve=curve,tdist=tdist,tau=tau)

    func <- function(test){
        out <- test(dat)
        if(inherits(out,'try-error')) return(rep(NA,3))
        out
    }

    c(vapply(list(sh1=shbw,sh3=shbwCube,ik1=ikSim,ik2=ikSimQuad,ik3=ikSimCube),
             func,numeric(3)),
      n,curve,tdist,tau)#,c('sh','ik',
    #'cft',
                                        #  'n','curve','tdist','tau'))
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


sortSim <- function(B){
    out <- vapply(1:B,
           function(i){
               dat <- makeDataSort(500)
               c(testSH(dat,1),ikTest(dat,1))
           }
          ,numeric(2))
    cat('SH level: ',mean(out[,1]<0.05),'   IK level:',mean(out[,2]<0.05),'\n')
    out
}




###############
### polynomial sim
###############
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

polySim2 <- function(n,degs=1:5,shape='lin',tdist=TRUE,tau=0){
    dat <- makeDataShapes( n=n,shape=shape,tdist=tdist,tau=tau)

    func <- function(deg){
        c(tryNA(shPoly(dat,deg),2),tryNA(ikPoly(dat,deg),2))
    }

    c(do.call('c',lapply(degs,func)),ikSim(dat)[2:3])
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
