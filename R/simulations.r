
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
