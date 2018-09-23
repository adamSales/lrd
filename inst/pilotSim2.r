### parallel computing on Adam's windows machine:
library(parallel)
cl <- makeCluster(10)

source('R/simCI.r')

nrep <- 5000

clusterEvalQ(cl,{
             library('robustbase')
             library('rdd')
             library('RItools')
             library('sandwich')
             library('nnet')
             source('R/functions.r')
             source('R/simCI.r')
             })

st <- system.time(outcomeSimCI2 <- totalOutcomeSim2(nrep,cl))

stopCluster(cl)

save(outcomeSimCI2, st, file='outcomeSimCI2.RData')
