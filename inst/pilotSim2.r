### parallel computing on Adam's windows machine:
library(parallel)
cl <- makeCluster(10)

library('robustbase')
library('rdd')
library('RItools')
library('sandwich')
library('nnet')
source('R/functions.r')
source('R/simCIhet.r')

nrep <- 5000

clusterEvalQ(cl,{
             library('robustbase')
             library('rdd')
             library('RItools')
             library('sandwich')
             library('nnet')
             source('R/functions.r')
             source('R/simCIhet.r')
             })

st <- system.time(res <- totalOutcomeSim(nrep,cl))

stopCluster(cl)

save(list=ls(), file=paste0('outcomeSimCIhetTrtEff',Sys.Date(),'.RData'))
