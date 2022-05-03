---
title: "LRD paper Appendix D, Tables from simulations presented in lrd paper"
author: "Adam C Sales & Ben B Hansen"
date: "29 April, 2022"
output: html_document
---



General dependencies.

```r
library('knitr')
library('kableExtra')
source('R/simCIhet.r')
source('R/functions.r')
source('R/ddsandwich.R')
source('R/displaySim.r')
```



Initialization. Note that `nreps=0` corresponds to no simulations,
just print results from previously saved simulations.
In order to re-run the simulations, the `nreps`
variable should have been set to a positive integer before initiating this script.

##Level/Power Simulation
The following code (optionally, if `nreps>0`) runs the simulation
reported in Table 3 of "Limitless Regression Discontinuity"

To run the simulations in parallel, using the `parallel` package in
`R`,
register a cluster, called `cl` with the desired number of nodes, with
code similar to the following:

```r
library(parallel)
cl <- makeCluster(5)
```


```r
if (!exists('nreps') ) nreps <- 0
nreps
```

```
## [1] 5000
```

```r
if (nreps) {
library('robustbase')
library('rdd')
library('RItools')
library('sandwich')
library('nnet')

clust <- FALSE
if(require('parallel')) if(exists('cl')) if(inherits(cl,"cluster")) clust <- TRUE

if(clust){
  clusterEvalQ(cl,{
             library('robustbase')
             library('rdd')
             library('RItools')
             library('sandwich')
             library('nnet')
             source('R/functions.r')
             source('R/simCIhet.r')
	     source('R/ddsandwich.R')
             })
} else cl <- NULL

set.seed(201609)
st <- system.time(outcomeSim <- totalOutcomeSim(nreps,cl))
save(outcomeSim, file=paste0('./outcomeSim',Sys.Date(),'.RData'))
cat(paste0(date(), ', nreps=', nreps, '\n'),
    paste(c(names(st),'\n', collapse=T)),
    st,
    file='fullOutcomeSim-runtime.txt', append=TRUE)
} else{ ### load the most recent simulation
  sims <- sort(grep('outcomeSim',list.files('.'),value=TRUE),decreasing=TRUE)

  load(paste0('./',sims[1]))
}
```

This code creates Table 3:

```r
capture.output({
  displayCIsimHet(outcomeSim,tau=0,
    caption=paste('Empirical bias and 95\\% confidence interval coverage (\\%) and width for the analyses of',prettyNum(ncol(outcomeSim[[1]]),big.mark=','),'simulated datasets using either permutation tests, limitless or local OLS methods. The average treatment effect was zero in all conditions; in six conditions the effect was uniquely zero, and in three it was distributed as $t_3$.'),
    label='tab:level')
  },file="./tab-levelSimulation.tex")
```
Here are the results, for all cases run:


```r
allRes <- dispAllSimp(outcomeSim)
rownames(allRes) <- NULL
save(allRes,file=paste0('./levelSimResults',Sys.Date(),'.RData'))
```

###Full Results: Bias

```r
kable(subset(allRes,meas=='Bias',select=-meas),caption=paste('Empirical bias for the analyses of ',ncol(outcomeSim[[1]]),'simulated datasets using either permutation tests, limitless or local OLS methods. Results for all conditions'),digits=2,format='html')%>%
  kable_styling(full_width=FALSE)%>%
#  group_rows("n=50",1,24)%>%group_rows("n=250",25,48)%>%group_rows("n=2500",49,72)%>%
  collapse_rows(columns = 1:4)#, valign = "center")
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Empirical bias for the analyses of  5000 simulated datasets using either permutation tests, limitless or local OLS methods. Results for all conditions</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> n </th>
   <th style="text-align:left;"> error </th>
   <th style="text-align:left;"> ATE </th>
   <th style="text-align:left;"> TE randomness </th>
   <th style="text-align:right;"> Limitless </th>
   <th style="text-align:right;"> Local-OLS </th>
   <th style="text-align:right;"> Permutation </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> -0.01 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 34 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 43 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 55 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 58 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 61 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 64 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 67 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 70 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.38 </td>
  </tr>
</tbody>
</table>

###95% CI Coverage

```r
kable(subset(allRes,meas=='Coverage',select=-meas),caption=paste('Empirical 95% confidence interval coverage for the analyses of ',ncol(outcomeSim[[1]]),'simulated datasets using either permutation tests, limitless or local OLS methods. Results for all conditions'),digits=2,format='html')%>%
  kable_styling(full_width=FALSE)%>%
#  group_rows("n=50",1,24)%>%group_rows("n=250",25,48)%>%group_rows("n=2500",49,72)%>%
  collapse_rows(columns = 1:4)#, valign = "center")
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Empirical 95% confidence interval coverage for the analyses of  5000 simulated datasets using either permutation tests, limitless or local OLS methods. Results for all conditions</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> n </th>
   <th style="text-align:left;"> error </th>
   <th style="text-align:left;"> ATE </th>
   <th style="text-align:left;"> TE randomness </th>
   <th style="text-align:right;"> Limitless </th>
   <th style="text-align:right;"> Local-OLS </th>
   <th style="text-align:right;"> Permutation </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.92 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.66 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.63 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.63 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.73 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.75 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.03 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.04 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 38 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.03 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 41 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.03 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 53 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 59 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 62 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 65 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 71 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
</tbody>
</table>

###95% CI Width

```r
kable(subset(allRes,meas=='Width',select=-meas),caption=paste('Empirical 95% confidence interval width for the analyses of ',ncol(outcomeSim[[1]]),'simulated datasets using either permutation tests, limitless or local OLS methods. Results for all conditions'),digits=2,format='html')%>%
  kable_styling(full_width=FALSE)%>%
#  group_rows("n=50",1,24)%>%group_rows("n=250",25,48)%>%group_rows("n=2500",49,72)%>%
  collapse_rows(columns = 1:4)#, valign = "center")
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Empirical 95% confidence interval width for the analyses of  5000 simulated datasets using either permutation tests, limitless or local OLS methods. Results for all conditions</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> n </th>
   <th style="text-align:left;"> error </th>
   <th style="text-align:left;"> ATE </th>
   <th style="text-align:left;"> TE randomness </th>
   <th style="text-align:right;"> Limitless </th>
   <th style="text-align:right;"> Local-OLS </th>
   <th style="text-align:right;"> Permutation </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 1.42 </td>
   <td style="text-align:right;"> 1.67 </td>
   <td style="text-align:right;"> 0.74 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 1.41 </td>
   <td style="text-align:right;"> 1.67 </td>
   <td style="text-align:right;"> 0.74 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 1.82 </td>
   <td style="text-align:right;"> 2.04 </td>
   <td style="text-align:right;"> 0.95 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 1.80 </td>
   <td style="text-align:right;"> 2.04 </td>
   <td style="text-align:right;"> 0.95 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 1.74 </td>
   <td style="text-align:right;"> 1.68 </td>
   <td style="text-align:right;"> 0.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 1.75 </td>
   <td style="text-align:right;"> 1.69 </td>
   <td style="text-align:right;"> 0.91 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 2.12 </td>
   <td style="text-align:right;"> 2.06 </td>
   <td style="text-align:right;"> 1.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 2.11 </td>
   <td style="text-align:right;"> 2.05 </td>
   <td style="text-align:right;"> 1.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.57 </td>
   <td style="text-align:right;"> 0.74 </td>
   <td style="text-align:right;"> 0.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.57 </td>
   <td style="text-align:right;"> 0.74 </td>
   <td style="text-align:right;"> 0.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 33 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.73 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 36 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.73 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 39 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.76 </td>
   <td style="text-align:right;"> 0.74 </td>
   <td style="text-align:right;"> 0.39 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 42 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.77 </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 0.39 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 45 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48 </td>
   <td style="text-align:left;"> 250 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.17 </td>
   <td style="text-align:right;"> 0.23 </td>
   <td style="text-align:right;"> 0.09 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.17 </td>
   <td style="text-align:right;"> 0.23 </td>
   <td style="text-align:right;"> 0.09 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 57 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.22 </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> 0.11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.22 </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> 0.11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 63 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 0.12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 0.12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> 0.15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 72 </td>
   <td style="text-align:left;"> 2500 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> 0.15 </td>
  </tr>
</tbody>
</table>



##Polynomial Simulation


```r
if (!exists('nreps') ) nreps <- 0
nreps
```

```
## [1] 5000
```

```r
if (nreps) {


set.seed(201609)
st2 <- system.time(totalPoly <- totalPolySim(nreps,cl))
save(totalPoly,file=paste0("./totalPolySim",Sys.Date(),".RData"))
cat(paste0(date(), ', nreps=', nreps, '\n'),
    paste(c(names(st),'\n', collapse=T)),
    st,
    file='totalPolySim-runtime.txt', append=TRUE)
} else{
  psims <- sort(grep('totalPolySim',list.files('.'),value=TRUE),decreasing=TRUE)

  load(paste0('./',psims[1]))
}
```

The following gives the results in Table 4 of the paper, in addition
to the break-down of RMSE into bias and variance, and analogous
results for normally-distributed errors.


```r
tab.paper <- prntTab(totalPoly,5,full=FALSE,md=FALSE)
capture.output(
polyLatex5(tab.paper,full=FALSE,caption=paste0('Results from ',prettyNum(ncol(totalPoly[[1]]),big.mark=','),' simulations of polynomial specifications for RDD analysis, using limitless, OLS, or local linear regression. Data-generating models (DGM) were as depicted in Figure~\\ref{fig:dgms}, with $t_{3}$ errors; sample size for all runs was 500; there was no treatment effect.'),label='tab:poly'),
    file="./tab-polynomialSimulation.tex")

tab <- prntTab(totalPoly,5,full=TRUE,md=FALSE)
#rownames(tab) <- rep(c('level','RMSE','bias','sd'),6)
colnames(tab) <- gsub('(sh|ik)\\.est.','deg=',colnames(tab))#c(rep(paste0('deg=',1:4),2),'')
colnames(tab)[ncol(tab)] <- 'n/a'
kable(tab,format='html',caption='Full results for polynomial simulation',digits=2)%>%
    kable_styling()%>% column_spec( 6,border_right=TRUE)%>%column_spec(11,border_right=TRUE)%>%
        add_header_above(c(" " = 1, "Limitless" = 5, "OLS" = 5, "Loc. Lin." = 1))%>%
            #group_rows("$t_3$ Error",1,12)%>%group_rows("N(0,1) Error",13,24)%>%
            group_rows("linear",1,3)%>%group_rows('antiSym',4,6)%>%group_rows('sine',7,9)#%>%
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Full results for polynomial simulation</caption>
 <thead>
<tr>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="1"></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="5"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Limitless</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="5"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">OLS</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Loc. Lin.</div></th>
</tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> deg=1 </th>
   <th style="text-align:right;"> deg=2 </th>
   <th style="text-align:right;"> deg=3 </th>
   <th style="text-align:right;"> deg=4 </th>
   <th style="text-align:right;"> deg=5 </th>
   <th style="text-align:right;"> deg=1 </th>
   <th style="text-align:right;"> deg=2 </th>
   <th style="text-align:right;"> deg=3 </th>
   <th style="text-align:right;"> deg=4 </th>
   <th style="text-align:right;"> deg=5 </th>
   <th style="text-align:right;"> n/a </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="3"><td colspan="12" style="border-bottom: 1px solid;"><strong>linear</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> bias </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.01 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.01 </td>
   <td style="text-align:right;"> 0.01 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> RMSE </td>
   <td style="text-align:right;"> 0.23 </td>
   <td style="text-align:right;"> 0.23 </td>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;"> 0.31 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.37 </td>
   <td style="text-align:right;"> 0.31 </td>
   <td style="text-align:right;"> 0.47 </td>
   <td style="text-align:right;"> 0.62 </td>
   <td style="text-align:right;"> 0.79 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.97 </td>
   <td style="text-align:right;"> 0.48 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> level </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.06 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.05 </td>
   <td style="text-align:right;"> 0.07 </td>
  </tr>
  <tr grouplength="3"><td colspan="12" style="border-bottom: 1px solid;"><strong>antiSym</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> bias </td>
   <td style="text-align:right;"> -0.63 </td>
   <td style="text-align:right;"> -0.63 </td>
   <td style="text-align:right;"> -0.02 </td>
   <td style="text-align:right;"> -0.02 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.13 </td>
   <td style="text-align:right;"> -0.63 </td>
   <td style="text-align:right;"> 0.16 </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> -0.09 </td>
   <td style="text-align:right;border-right:1px solid;"> -0.09 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> RMSE </td>
   <td style="text-align:right;"> 0.67 </td>
   <td style="text-align:right;"> 0.67 </td>
   <td style="text-align:right;"> 0.31 </td>
   <td style="text-align:right;"> 0.31 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.39 </td>
   <td style="text-align:right;"> 0.70 </td>
   <td style="text-align:right;"> 0.51 </td>
   <td style="text-align:right;"> 0.65 </td>
   <td style="text-align:right;"> 0.81 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.98 </td>
   <td style="text-align:right;"> 0.51 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> level </td>
   <td style="text-align:right;"> 0.79 </td>
   <td style="text-align:right;"> 0.79 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.06 </td>
   <td style="text-align:right;"> 0.54 </td>
   <td style="text-align:right;"> 0.07 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.05 </td>
   <td style="text-align:right;"> 0.07 </td>
  </tr>
  <tr grouplength="3"><td colspan="12" style="border-bottom: 1px solid;"><strong>sine</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> bias </td>
   <td style="text-align:right;"> 1.16 </td>
   <td style="text-align:right;"> 1.16 </td>
   <td style="text-align:right;"> 0.17 </td>
   <td style="text-align:right;"> 0.17 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.01 </td>
   <td style="text-align:right;"> 1.16 </td>
   <td style="text-align:right;"> -0.11 </td>
   <td style="text-align:right;"> -0.09 </td>
   <td style="text-align:right;"> -0.01 </td>
   <td style="text-align:right;border-right:1px solid;"> -0.01 </td>
   <td style="text-align:right;"> 0.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> RMSE </td>
   <td style="text-align:right;"> 1.19 </td>
   <td style="text-align:right;"> 1.19 </td>
   <td style="text-align:right;"> 0.35 </td>
   <td style="text-align:right;"> 0.35 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.36 </td>
   <td style="text-align:right;"> 1.20 </td>
   <td style="text-align:right;"> 0.47 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 0.78 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.95 </td>
   <td style="text-align:right;"> 0.53 </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> level </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.05 </td>
   <td style="text-align:right;"> 0.96 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.05 </td>
   <td style="text-align:right;"> 0.08 </td>
  </tr>
</tbody>
</table>

```r
                #group_rows("linear",13,16)%>%group_rows('antiSym',17,20)%>%group_rows('oneSide',21,24)
```


Session information

```r
sessionInfo()
```

```
## R version 4.1.2 (2021-11-01)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 20.04.3 LTS
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3
## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/liblapack.so.3
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] nnet_7.3-16       RItools_0.1-18    SparseM_1.81      rdd_0.57         
##  [5] Formula_1.2-4     AER_1.2-9         survival_3.2-13   car_3.0-12       
##  [9] carData_3.0-5     lmtest_0.9-40     zoo_1.8-9         sandwich_3.0-1   
## [13] robustbase_0.95-0 kableExtra_1.3.4  knitr_1.33       
## 
## loaded via a namespace (and not attached):
##  [1] xfun_0.24         splines_4.1.2     lattice_0.20-45   colorspace_2.0-2 
##  [5] vctrs_0.3.8       htmltools_0.5.1.1 viridisLite_0.4.0 utf8_1.2.1       
##  [9] rlang_0.4.11      pillar_1.6.1      glue_1.4.2        lifecycle_1.0.0  
## [13] stringr_1.4.0     munsell_0.5.0     rvest_1.0.0       svd_0.5.1        
## [17] evaluate_0.14     fansi_0.5.0       DEoptimR_1.0-11   highr_0.9        
## [21] xtable_1.8-4      scales_1.1.1      webshot_0.5.3     abind_1.4-5      
## [25] systemfonts_1.0.4 digest_0.6.27     stringi_1.6.2     grid_4.1.2       
## [29] tools_4.1.2       magrittr_2.0.1    tibble_3.1.2      pkgconfig_2.0.3  
## [33] crayon_1.4.1      ellipsis_0.3.2    Matrix_1.3-4      xml2_1.3.2       
## [37] rmarkdown_2.9     svglite_2.1.0     httr_1.4.2        rstudioapi_0.13  
## [41] R6_2.5.0          compiler_4.1.2
```
