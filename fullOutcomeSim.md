---
title: "Tables from simulations presented in lrd paper"
author: "lrd authors"
date: "27 February, 2019"
output: html_document
---



General dependencies.

```r
library('knitr')
#library('lrd')
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
## [1] 0
```

```r
if (nreps) {
library('robustbase')
library('rdd')
library('RItools')
library('sandwich')
library('nnet')

if(require('parallel')&exists('cl')&inherits(cl,"cluster")){
  clusterEvalQ(cl,{
             library('robustbase')
             library('rdd')
             library('RItools')
             library('sandwich')
             library('nnet')
             source('R/functions.r')
             source('R/simCIhet.r')
             })
} else cl <- NULL

set.seed(201609)
st <- system.time(outcomeSim <- totalOutcomeSim(nrep,cl))
save(outcomeSim, file=paste0('inst/outcomeSim',Sys.Date(),'.RData'))
cat(paste0(date(), ', nreps=', nreps, '\n'),
    paste(c(names(st),'\n', collapse=T)),
    st,
    file='dataResults/fullOutcomeSim-runtime.txt', append=TRUE)
} else{ ### load the most recent simulation
  sims <- sort(grep('outcomeSim',list.files('./inst/'),value=TRUE),decreasing=TRUE)

  load(paste0('inst/',sims[1]))
}
```

This code creates Table 3:

```r
capture.output({
  displayCIsimHet(outcomeSim,tau=0,
    caption=paste('Empirical bias and 95\\% confidence interval coverage and width for the analyses of ',ncol(outcomeSim[[1]]),'simulated datasets using either permutation tests, limitless or local OLS methods. The average treatment effect was zero in all conditions; in six conditions the effect was uniquely zero, and in three it was distributed as $t_3$.'),
    label='tab:level')
  },file="inst/r1/tab-levelSimulation.tex")
```
Here are the results, for all cases run:


```r
allRes <- dispAllSimp(outcomeSim)
rownames(allRes) <- NULL
save(allRes,file=paste0('inst/levelSimResults',Sys.Date(),'.RData'))
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
   <td style="text-align:left;vertical-align: middle !important;" rowspan="8"> 50 </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> -0.01 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 19 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> -0.01 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 22 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="8"> 250 </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 28 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 31 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 34 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 37 </td>
   
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 40 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 43 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 46 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="8"> 2500 </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 55 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 58 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 61 </td>
   
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 64 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 67 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 70 </td>
   
   
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
   <td style="text-align:left;vertical-align: middle !important;" rowspan="8"> 50 </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.66 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.64 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.64 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.76 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.75 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="8"> 250 </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 29 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 32 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.03 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 35 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.04 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 38 </td>
   
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.03 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 41 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.04 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 44 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="8"> 2500 </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.96 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 53 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 56 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 59 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 62 </td>
   
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 65 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 71 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.95 </td>
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
   <td style="text-align:left;vertical-align: middle !important;" rowspan="8"> 50 </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 1.41 </td>
   <td style="text-align:right;"> 1.66 </td>
   <td style="text-align:right;"> 0.74 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 1.40 </td>
   <td style="text-align:right;"> 1.66 </td>
   <td style="text-align:right;"> 0.74 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 1.80 </td>
   <td style="text-align:right;"> 2.04 </td>
   <td style="text-align:right;"> 0.95 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 1.82 </td>
   <td style="text-align:right;"> 2.05 </td>
   <td style="text-align:right;"> 0.95 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 1.75 </td>
   <td style="text-align:right;"> 1.69 </td>
   <td style="text-align:right;"> 0.91 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 1.74 </td>
   <td style="text-align:right;"> 1.68 </td>
   <td style="text-align:right;"> 0.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 21 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 2.11 </td>
   <td style="text-align:right;"> 2.05 </td>
   <td style="text-align:right;"> 1.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 24 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 2.11 </td>
   <td style="text-align:right;"> 2.05 </td>
   <td style="text-align:right;"> 1.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="8"> 250 </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.57 </td>
   <td style="text-align:right;"> 0.74 </td>
   <td style="text-align:right;"> 0.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 30 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.57 </td>
   <td style="text-align:right;"> 0.74 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 33 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.73 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 36 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.73 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 39 </td>
   
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.77 </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 0.39 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 42 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.77 </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 0.39 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 45 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 48 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="8"> 2500 </td>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> t </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.17 </td>
   <td style="text-align:right;"> 0.23 </td>
   <td style="text-align:right;"> 0.09 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.17 </td>
   <td style="text-align:right;"> 0.23 </td>
   <td style="text-align:right;"> 0.09 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 57 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.22 </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> 0.11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 60 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> t </td>
   <td style="text-align:right;"> 0.22 </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> 0.11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 63 </td>
   
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> norm </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 0.12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   
   
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> none </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 0.12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 69 </td>
   
   
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> norm </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> 0.15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 72 </td>
   
   
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
if (nreps) {


set.seed(201609)
st2 <- system.time(totalPoly <- totalPolySim(nreps,cl))
save(totalPoly,file=paste0("inst/totalPolySim",Sys.Date(),".RData"))
cat(paste0(date(), ', nreps=', nreps, '\n'),
    paste(c(names(st),'\n', collapse=T)),
    st,
    file='dataResults/totalPolySim-runtime.txt', append=TRUE)
} else{
  psims <- sort(grep('totalPolySim',list.files('./inst/'),value=TRUE),decreasing=TRUE)

  load(paste0('inst/',psims[1]))
}
```

The following gives the results in Table 4 of the paper, in addition
to the break-down of RMSE into bias and variance, and analogous
results for normally-distributed errors.


```r
tab.paper <- prntTab(totalPoly,full=FALSE,md=FALSE)
capture.output(
polyLatex(tab.paper,full=FALSE,caption=paste0('Results from ',ncol(totalPoly[[1]]),' simulations of polynomial specifications for RDD analysis, using MM-estimation, OLS or local linear regression. Data generating models were as depicted in Figure~\\ref{fig:dgms}, with $t_{3}$ errors; sample size for all runs was 500, and there was no treatment effect.'),label='tab:poly'),
    file="tab-polynomialSimulation.tex")
tab <- prntTab(totalPoly,full=TRUE,md=TRUE)
rownames(tab) <- rep(c('level','RMSE','bias','sd'),6)
colnames(tab) <- c(rep(paste0('deg=',1:4),2),'')
kable(tab,format='html',caption='Full results for polynomial simulation',digits=2)%>%
    kable_styling()%>% column_spec( 5,border_right=TRUE)%>%column_spec(9,border_right=TRUE)%>%
        add_header_above(c(" " = 1, "Limitless" = 4, "OLS" = 4, "Loc. Lin." = 1))%>%
            group_rows("$t_3$ Error",1,12)%>%group_rows("N(0,1) Error",13,24)%>%
            group_rows("linear",1,4)%>%group_rows('antiSym',5,8)%>%group_rows('oneSide',9,12)%>%
                group_rows("linear",13,16)%>%group_rows('antiSym',17,20)%>%group_rows('oneSide',21,24)
```


Session information

```r
sessionInfo()
```

```
## R version 3.4.4 (2018-03-15)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7601) Service Pack 1
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] kableExtra_1.0.1 knitr_1.21      
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.13      rstudioapi_0.6    xml2_1.1.1       
##  [4] magrittr_1.5      hms_0.3           rvest_0.3.2      
##  [7] munsell_0.4.3     viridisLite_0.2.0 colorspace_1.2-6 
## [10] R6_2.1.2          rlang_0.1.2       highr_0.6        
## [13] plyr_1.8.4        stringr_1.2.0     httr_1.2.1       
## [16] tools_3.4.4       webshot_0.5.1     xfun_0.5         
## [19] selectr_0.3-1     htmltools_0.3.5   digest_0.6.10    
## [22] rprojroot_1.2     tibble_1.4.2      readr_1.1.1      
## [25] glue_1.1.1        evaluate_0.10     rmarkdown_1.7    
## [28] stringi_1.1.1     compiler_3.4.4    pillar_1.1.0     
## [31] scales_0.4.1      backports_1.1.2   XML_3.98-1.5
```
