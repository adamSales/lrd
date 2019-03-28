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

res <- t(res)

olsRes <- res[,1:4]
shRes <- res[,5:8]

summ <- rbind(ols=c(mean(olsRes[,3]),mean(olsRes[,4]<=0.05),mean(olsRes[,1]*olsRes[,2]<=0)),
              sh=c(mean(shRes[,3]),mean(shRes[,4]<=0.05),mean(shRes[,1]*shRes[,2]<=0)))
colnames(summ) <- c('bias','type1error','95ciCoverage')

write.csv(summ,file='newSimResultsSummary.csv')
