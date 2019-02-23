source('R/simCI.r')

### parallel computing on Adam's windows machine:
library(parallel)
cl <- makeCluster(10)


nrep <- 5000

clusterEvalQ(cl,{
             library('robustbase')
             library('rdd')
             library('sandwich')
             library('nnet')
             source('R/functions.r')
             source('R/simCI.r')
             })

st <- system.time(PolySim3 <- polySimShape(nrep,'poly3',cl))

stopCluster(cl)

save(PolySim3,st,file='polySim3.RData')
