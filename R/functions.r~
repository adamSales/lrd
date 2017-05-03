


#################
### SH functions
#################

sh <- function(dat,BW,outcome,Dvar='probation_year1',alpha=0.05,rhs=NULL){
    if(missing(BW)) BW <- bwMult(dat)
    if(!is.null(Dvar)) dat$D <- dat[[Dvar]]
    bal.pval <- balMult(dat,BW,method='sh',reduced.covars=FALSE,rhs=rhs)
    p.value <- testSH(dat=dat,BW=BW,outcome=outcome,rhs=rhs)
    est <- HLsh(dat=dat,BW=BW,outcome=outcome,rhs=rhs)
    CI <- CIsh(dat=dat,BW=BW,outcome=outcome,est=est,alpha=alpha,rhs=rhs)
    list(p.value=p.value,CI=CI,BW=BW,bal.pval=bal.pval,n=nrow(dat),W=range(abs(dat$R)))
}


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

HLsh <- function(dat,BW,outcome='Y',rhs=rhs){

    shortFunction <- function(tau)
        testSH(dat=dat,BW=BW,tau=tau,outcome=outcome,return.coef=TRUE,rhs=rhs)
    uniroot(shortFunction,interval=c(-1,1),extendInt='yes')$root
}

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


ik <- function(dat,BW=NULL,outcome){
    mod <- ikTest(dat,BW,varb=outcome,justP=FALSE)
    BW <- mod$bw[1]
    bal.pval=balMult(dat=dat,BW=BW,method='ik',reduced.covars=FALSE)
    list(p.value=mod$p[1],CI=-c(mod$ci[1,],mod$est[1]),bal.pval=bal.pval,
         W=c(min(abs(dat$R)),BW))
}

ikTest <- function(dat,BW,varb,rhs=NULL,justP=TRUE){
    if(!missing(varb)) dat$Y <- dat[[varb]]
    if(missing(BW) | is.null(BW))  mod <- try(RDestimate(Y~R,kernel='rectangular',data=dat))
    else  mod <- try(RDestimate(Y~R,kernel='rectangular',data=dat,bw=BW))
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

    list(estAll$p.value,CI=c(estAll$conf.int,estAll$estimate),bal.pval=bal.pval,
         n=sum(abs(dat$R)<BW),W=c(min(abs(dat$R)),BW))
}

cftTest <- function(dat,BW,varb,alpha=0.05,rhs=NULL,justP=TRUE){
    if(sum(dat$R<0 & -BW<dat$R)<2 || sum(dat$R>0 & dat$R<BW)<2) return(NA)
    dat <- dat[abs(dat$R)<BW,]

    dat$Y <- dat[[varb]]

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
    min(ps*length(ps))
}



#' Choose a bandwidth using either Limitless or Permutations method with one covariate, or the IK method
#'
#' @param dat a data.frame. must contain variables `R` (the running variable) and `Z` (treatment) and, if method='ik', `Y` (outcome)
#' @param covname a character string, the name of the covariate to test balance on (unless method='ik')
#' @param method a character string. either 'sh' for Limitless, 'ik' for IK, or 'cft' for Permutations
#'
#' @return bandwidth choice (scalar)
#'
bw <- function(dat,covname='x',method='sh',alphax=0.15){

    if(method=='ik') return(IKbandwidth(dat$R,dat$Y,kernel='rectangular'))
    else test <- switch(method,"sh"= testSH, "cft"=cft)
    short <- function(w) test(dat=dat,BW=w,varb=covname)

    bws <- seq(0,1,0.01)
    pvals <- vapply(bws,short,1)

    if(all(is.na(pvals)) | all(pvals<alpha,na.rm=TRUE)) return(0)
     max(bws[pvals>alpha],na.rm=TRUE)
}


#' Choose a bandwidth using one of several methods
#'
#' Method options include Limitless, a permutation-testing method, or the IK method
#'
#' @param dat a data.frame. must contain variables `R` (the running variable) and `Z` (treatment) and, if method='ik', `Y` (outcome) along with these covariates: 'lhsgrade_pct','totcredits_year1','male','loc_campus1','loc_campus2','bpl_north_america','english', and 'age'
#' @param alpha the level of the balance test
#' @param newbal.control list of optional arguments to `newBal`
#' @param bsw numeric, a sequence of bandwidths to try (in increasing order)
#'
#' @return bandwidth choice (scalar)
#'
bwMult <- function(dat,alpha=0.15,balMult.control=list(method='sh',reduced.covars=FALSE), bws = seq(.3,2,0.01)){
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
