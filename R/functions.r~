


#################
### SH functions
#################

#' Wrapper function for limitless regression discontinuity testing and estimation
#'
#' In a regression discontinuity design, this function tests for an effect using \code{testSH},
#' and inverts the test to estimate a confidence interval and Hodges-Lehmann point estimate.
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
#' @param dat Data set with columns \code{Z}, \code{R}, an outcome variable, and (optionally)
#' an indicator for treatment received, in the case of partial compliance.
#' @param BW An optional bandwidth \code{b>0}. The test uses data for subjects with |R|<b. When
#' \code{BW} is omitted, the function uses sequential covariate balance tests to choose a
#' data-driven bandwidth.
#' @param outcome A string specifying the name of the outcome variable
#' @param Dvar An optional string specifying the name of the treatment-received variable in the
#' case of partial compliance.
#' @param alpha The level of the confidence interval
#' @param rhs A string specifying the \eqn{\mu_\beta(\cdot)} model relating the outcome to
#' \code{R}. Defaults to \code{"R+Z"}
#'
#' @return a list consisting of
#'  \item p.value The p-value testing no effect
#'  \item CI a vector of confidence limits and the HL treatment effect
#'  \item BW the RDD bandwidth
#'  \item bal.pval a p-value testing for covariate balance
#'  \item n the number of subjects in the window of analysis
#'  \item W the range of R values in the window of analysis
#' @imports robustbase
#' @export

sh <- function(dat,BW,outcome,Dvar='probation_year1',alpha=0.05,rhs=NULL){
    if(missing(BW)) BW <- bwMult(dat)
    if(!is.null(Dvar)) dat$D <- dat[[Dvar]]
    bal.pval <- balMult(dat,BW,method='sh',reduced.covars=FALSE,rhs=rhs)
    p.value <- testSH(dat=dat,BW=BW,outcome=outcome,rhs=rhs)
    est <- HLsh(dat=dat,BW=BW,outcome=outcome,rhs=rhs)
    CI <- CIsh(dat=dat,BW=BW,outcome=outcome,est=est,alpha=alpha,rhs=rhs)
    list(p.value=p.value,CI=CI,BW=BW,bal.pval=bal.pval,W=c(min(abs(dat$R)),BW),n=sum(abs(dat$R)<BW))
}

#' Univariate covariance-adjusted balance test, using Huber-White SE
#'
#' The test is the test associated with the Z coefficient after fitting
#' a linear or logistic-linear model with Z and R as independent variables.
#' The Huber-White SE is the one provided by the sandwich package.
#' #'
#'
#' @param dat Data set with columns \code{Z}, \code{R} and a covariate
#' @param BW The bandwidth within which to test balance.
#' @param varb The character name of the variable to test balance on.
#' @param rhs  A string specifying the \eqn{\mu_\beta(\cdot)} model relating the outcome to
#' \code{R}. Defaults to \code{"R+Z"}
#'
#' @return scalar, the p-value associated w/ coefficient on Z
#' @imports sandwich
#' @export
balOneSH <- function(dat,BW,varb,rhs=NULL){

    if(sum(dat$R<0 & -BW<dat$R)<2 || sum(dat$R>0 & dat$R<BW)<2) return(NA)
    dat <- dat[abs(dat$R)<BW,]
    dat$ytilde <- dat[[varb]]

    form <- if(is.null(rhs)) as.formula('ytilde~Z+R') else
                   as.formula(paste0('ytilde',rhs))

    if(length(unique(dat$ytilde))>2)
        mod <- lm(form,dat=dat)
    else mod <- glm(form,dat=dat,family=binomial)
    vcv <- sandwich::sandwich(mod)
    Z.pos.c <- pmatch("Z", names(coef(mod)))
    Z.pos.v <- pmatch("Z", colnames(vcv))
    tstat <- coef(mod)[Z.pos.c]/sqrt(vcv[Z.pos.v, Z.pos.v])
    2 * pt(-abs(tstat), df=mod$df.residual, lower.tail=T)
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
#' @param dat Data set with columns \code{Z}, \code{R}, and an outcome variable
#' @param BW A bandwidth \code{b>0}. The test uses data for subjects with |R|<b
#' @param tau A hypothetical treatment effect. Defaults to 0
#' @param outcome A string specifying the name of the outcome variable
#' @param return.coef If TRUE return the Z-coefficient from the model; if FALSE return
#' the p-value
#' @param rhs A string specifying the \eqn{\mu_\beta(\cdot)} model relating the outcome to
#' \code{R}. Defaults to \code{"R+Z"}
#'
#' @return scalar, if return.coef the coefficient on Z; otherwise the p-value associated
#' w/ coefficient on Z
#' @imports robustbase
#' @export
testSH <- function(dat,BW,tau=0,outcome='Y',return.coef=FALSE,rhs=NULL){
    if(sum(dat$R<0 & -BW<dat$R)<2 || sum(dat$R>0 & dat$R<BW)<2) return(NA)
    dat <- dat[abs(dat$R)<BW,]

    if('D'%in%names(dat)) dat$ytilde <- dat[[outcome]]-tau*dat$D
    else dat$ytilde <- dat[[outcome]]-tau*dat$Z

    ctl <- if (exists('lmrob_seed')) {
        lmrob.control(seed=lmrob_seed, k.max=500, maxit.scale=500)
    } else lmrob.control(          k.max=500, maxit.scale=500)

    form <- if(is.null(rhs)) as.formula('ytilde~Z+R') else
                   as.formula(paste0('ytilde',rhs))

    mod <- lmrob(form,data=dat,method='MM',control=ctl)

    newcov <- try(sandwich(mod))
    mod$cov <- if (inherits(newcov, "try-error")) NA else newcov
    Z.pos = pmatch("Z", names(coef(mod)))
    if(return.coef) return(coef(mod)[Z.pos])
    coef(summary(mod))[Z.pos,4]

}


#' Hodges-Lehmann estimate of a treatment effect for limitless regression discontinuity
#'
#' Inverts the test in \code{testSH()} to find the Hodges-Lehmann point estimate
#'
#'
#' @param dat Data set with columns \code{Z}, \code{R}, and an outcome variable
#' @param BW A bandwidth \code{b>0}. The test uses data for subjects with |R|<b
#' @param outcome A string specifying the name of the outcome variable
#' @param rhs A string specifying the \eqn{\mu_\beta(\cdot)} model relating the outcome to
#' \code{R}. Defaults to \code{"R+Z"}
#'
#' @return scalar, the Hodges-Lehmann estimate
#' @imports robustbase
#' @export

HLsh <- function(dat,BW,outcome='Y',rhs=rhs){

    shortFunction <- function(tau)
        testSH(dat=dat,BW=BW,tau=tau,outcome=outcome,return.coef=TRUE,rhs=rhs)
    out <- try(uniroot(shortFunction,interval=c(-1,1),extendInt='yes')$root)
    ifelse(inherits(out, 'try-error'),NA,out)
}


#' Confidence Interval and Hodges-Lehmann estimate of a treatment effect for limitless
#' regression discontinuity
#'
#' Inverts the test in \code{testSH()} to find the Confidence Interval and Hodges-Lehmann
#' point estimate
#'
#'
#' @param dat Data set with columns \code{Z}, \code{R}, and an outcome variable
#' @param BW A bandwidth \code{b>0}. The test uses data for subjects with |R|<b
#' @param outcome A string specifying the name of the outcome variable
#' @param est A point estimate generated by \code{HLsh}. If not provided it will be estimated
#' automatically.
#' @param alpha the level of the confidence interval.
#' @param rhs A string specifying the \eqn{\mu_\beta(\cdot)} model relating the outcome to \code{R}. Defaults to \code{"R+Z"}
#'
#' @return a vector including
#'  \item CI1 and
#'  \item CI2, the lower and upper bounds of the confidence interval
#'  \item est, the HL point estiate
#' @imports robustbase
#' @export
CIsh <- function(dat,BW,outcome='Y',est,alpha=0.05,rhs=rhs){

    shortFunction <- function(tau) testSH(dat=dat,BW=BW,tau=tau,outcome=outcome,rhs=rhs)-alpha
    if(missing(est)) est <- HLsh(dat=dat,BW=BW,method=method,outcome=outcome)
    CI1 <- uniroot(shortFunction,interval=c(-1,est),extendInt='upX')$root
    CI2 <- uniroot(shortFunction,interval=c(est,1),extendInt='downX')$root
    c(CI1=CI1,CI2=CI2,est=est)
}




###############
### IK method
#############


#' Wrapper for Imbens-Kalyanaraman style RDD analysis
#'
#' Uses OLS to estimate the RDD Local Average Treatment Effect (LATE) as in
#' Imbens, G. and Kalyanaraman, K. (2012). Uses a "rectangular" kernal.
#' Tests balance on a set of pre-treatment covariates using \code{balMult} and either the
#' provided bandwidth or the bandwidth calculated with the IK procedure.
#'
#'
#' @param dat Data set with columns \code{Z}, \code{R}, and an outcome variable
#' @param BW (Optional) A bandwidth \code{b>0}. If not provided it will be estimated from the
#' data
#' @param outcome A string specifying the name of the outcome variable
#'
#' @return a list consisting of
#'  \item p.value The p-value testing no effect
#'  \item CI a vector of confidence limits and the HL treatment effect
#'  \item BW the RDD bandwidth
#'  \item bal.pval a p-value testing for covariate balance
#'  \item n the number of subjects in the window of analysis
#'  \item W the range of R values in the window of analysis
#' @imports rdd
#' @export

ik <- function(dat,BW=NULL,outcome){
    mod <- ikTest(dat,BW,varb=outcome,justP=FALSE)
    BW <- mod$bw[1]
    bal.pval=balMult(dat=dat,BW=BW,method='sh',reduced.covars=FALSE)
    list(p.value=mod$p[1],CI=c(mod$ci[1,],est=mod$est[1]),bal.pval=bal.pval,
         W=c(min(abs(dat$R)),BW),n=sum(abs(dat$R)<BW))
}


#' Imbens-Kalyanaraman style RDD analysis
#'
#' Uses OLS to estimate the RDD Local Average Treatment Effect (LATE) as in
#' Imbens, G. and Kalyanaraman, K. (2012). Uses a "rectangular" kernal.
#' Tests balance on a set of pre-treatment covariates using \code{balMult} and either the
#' provided bandwidth or the bandwidth calculated with the IK procedure.
#'
#'
#' @param dat Data set with columns \code{Z}, \code{R}, and an outcome variable
#' @param BW (Optional) A bandwidth \code{b>0}. If not provided it will be
#' estimated from the data
#' @param varb A string specifying the name of the outcome or covariate variable
#' @param rhs Ignored
#' @param justP if TRUE only returns the p-value
#'
#' @return if justP=TRUE, returns the p-value for an effect. if justP=FALSE,
#' returns the fitted RDD model (an object of class "RD").
#' @imports rdd
#' @export

ikTest <- function(dat,BW=NULL,varb,rhs=NULL,justP=TRUE){
    if(!missing(varb)) dat$Y <- -dat[[varb]]
    if(missing(BW) | is.null(BW))
        mod <- try(RDestimate(Y~R,kernel='rectangular',data=dat,cutpoint=-0.005))
    else  mod <- try(RDestimate(Y~R,kernel='rectangular',data=dat,bw=BW,cutpoint=-0.005))
    if(class(mod)=='try-error') return(rep(NA,5))
    if(justP) return(mod$p[1])
    mod
}

ikMultBal <- function(dat,BW,xvars,int=FALSE){
    require(systemfit)
    xeq <- if(int) lapply(xvars,function(xx) as.formula(paste0(xx,'~R+Z+R:Z'))) else
    lapply(xvars,function(xx) as.formula(paste0(xx,'~R+Z')))
    names(xeq) <- gsub('_','',xvars)

    sur <- systemfit(xeq,data=subset(dat,abs(R)<BW))

    rest <- paste(names(xeq),'Z',sep='_')
    rest <- paste(rest,'=0')

    linearHypothesis(sur,rest,test='Chisq')$Pr[2]
}




################
### CFT Method
##############

cft <- function(dat,BW,outcome,alpha=0.05,rhs=NULL){
    if(missing(BW) |is.null(BW))
        BW <- bwMult(dat,balMult.control=list(method='cft',reduced.covars=FALSE))

    if(sum(dat$R<0 & -BW<dat$R)<2 || sum(dat$R>0 & dat$R<BW)<2) return(NA)

    bal.pval=balMult(dat=dat,BW=BW,method='cft',reduced.covars=FALSE)

    estAll <- cftTest(dat=dat,BW=BW,varb=outcome,alpha=alpha,justP=FALSE)

    list(estAll$p.value,CI=c(estAll$conf.int,est=estAll$estimate),bal.pval=bal.pval,
         n=sum(abs(dat$R)<BW),W=c(min(abs(dat$R)),BW))
}

cftTest <- function(dat,BW,varb,alpha=0.05,rhs=NULL,justP=TRUE){
    if(sum(dat$R<0 & -BW<dat$R)<2 || sum(dat$R>0 & dat$R<BW)<2) return(NA)
    dat <- dat[abs(dat$R)<BW,]

    if(!missing(varb)) dat$Y <- -dat[[varb]]

    wilcox <- with(dat,wilcox.test(Y~Z,conf.int=!justP,conf.level=1-alpha))
    if(justP) return(wilcox$p.value)
    return(wilcox[c('p.value','conf.int','estimate')])
}


##############################
### data-driven bandwidths
############################




balMult <- function(dat,BW,method='sh',reduced.covars=TRUE,rhs=NULL){
    ps <- NULL
    balTest <- switch(method, "sh"=balOneSH,"cft"=cftTest,"ik"=ikTest)

    xvars <- c('lhsgrade_pct','totcredits_year1','male','loc_campus1','loc_campus2',
               'bpl_north_america','english')
    xvars <- if (reduced.covars) c(xvars, 'age') else c(xvars, 'age_at_entry')

    if(method=='ik') return(ikMultBal(dat,BW,xvars))

    for(varb in xvars){
        ps <- c(ps,balTest(dat=dat,BW=BW,varb=varb,rhs=rhs))

    }
    min(1,min(ps*length(ps)))
}



#' Choose a bandwidth using either Limitless or Permutations method with one covariate, or the IK method
#'
#' @param dat a data.frame. must contain variables `R` (the running variable) and `Z`
#' (treatment) and, if method='ik', `Y` (outcome)
#' @param covname a character string, the name of the covariate to test balance on
#' (unless method='ik')
#' @param method a character string. either 'sh' for Limitless, 'ik' for IK, or 'cft' for
#' Permutations
#'
#' @return bandwidth choice (scalar)
#'
bw <- function(dat,covname='x',method='sh',alphax=0.15,plt=FALSE){

    if(method=='ik') return(IKbandwidth(dat$R,dat$Y,kernel='rectangular'))
    else test <- switch(method,"sh"= balOneSH, "cft"=cft)
    short <- function(w) test(dat=dat,BW=w,varb=covname)# - alphax
    #uniroot(short,interval=c(min(max(dat$R),-min(dat$R))-0.1,min(max(dat$R),-min(dat$R))),
    #                  extendInt='downX')$root
    bws <- seq(0.02,min(max(dat$R),-min(dat$R)),0.01)
    ps <- vapply(bws,short,1)
    if(plt) plot(ps)
    bws[max(which(ps>alphax))]
}



#' Choose a bandwidth using one of several methods
#'
#' Method options include Limitless, a permutation-testing method, or the IK method
#'
#' @param dat a data.frame. must contain variables `R` (the running variable) and `Z`
#' (treatment) and, if method='ik', `Y` (outcome) along with these covariates: 'lhsgrade_pct',
#' 'totcredits_year1','male','loc_campus1','loc_campus2','bpl_north_america','english',
#' and 'age'
#' @param alpha the level of the balance test
#' @param newbal.control list of optional arguments to `newBal`
#' @param bsw numeric, a sequence of bandwidths to try (in increasing order)
#'
#' @return bandwidth choice (scalar)
#'
bwMult <- function(dat,alpha=0.15,balMult.control=list(method='sh',reduced.covars=FALSE), bws = seq(0,2,0.01)){
    stopifnot(is.list(balMult.control),
              !any(c('dat', 'BW') %in% names(balMult.control)),
              is.numeric(bws) && all(bws >=0),
              length(bws)==1 || !is.unsorted(bws))
    short <- function(w) {
        nbargs <- c(list(dat=dat, BW=w), balMult.control)
        try(do.call(balMult, nbargs))
    }

    p <- 0
    i <- length(bws)+1
    while(p<alpha){
        i <- i-1
        if(i==0) return(i)
        bw <- bws[i]
        pval <- short(bw)
        p <- ifelse(is.numeric(pval)&is.finite(pval),pval,0)
    }
    bws[i]
}


####################################
#### Simulation Functions
##############################3
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

    x <- R+ifelse(R< -0.5,curve*R-sign(R)*.5*curve,0)
    if(tdist) x <- x+rt(n,3)
    else x <- x+rnorm(n)

    yc <- .5*R+ ifelse(R< -0.5,curve*R-sign(R)*.5*curve,0)
    yc <- yc+if(tdist) rt(n,3) else rnorm(n)

    Z <- R>0

    Y <- yc+Z*tau

    data.frame(R=R,x=x,yc=yc,Z=Z,Y=Y)
}

shbw <- function(dat){
    BW <- bw(dat)
    c(BW,testSH(dat,BW,rhs='~poly(R,3)+Z'),HLsh(ddd,BW,rhs='~poly(R,3)+Z'))
}

ikSim <- function(dat){
    mod <- ikTest(dat, justP=FALSE)
    c(mod$bw[1],mod$p[1],mod$est[1])
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
