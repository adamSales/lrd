source('../../R/functions.r')


makeDat <- function(){
    R <- rep(seq(-.51,.51,.01),10)

    Y <- .5*R

    Y[abs(R)>.5] <- 1.5*R[abs(R)>.5]

    Y <- Y+rnorm(length(Y),0,sqrt(0.01))

    Z <- R>0
    data.frame(R,Y,Z)
}

anal <- function(ddd){
    ols <- lm(Y~R+Z,data=ddd)
    c(confint(ols)['ZTRUE',],summary(ols)$coef['ZTRUE',c(1,4)],
      CIsh(ddd,BW=0.52,outcome='Y'),
      testSH(ddd,BW=0.52,outcome='Y'))
}



res <- replicate(5000,anal(makeDat()))
save(res,file='newSimRes.RData')
