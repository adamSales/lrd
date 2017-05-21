---
title: "Tables from simulations presented in lrd paper"
author: "lrd authors"
date: "21 May, 2017"
output: html_document
---



General dependencies.

```r
#print(getwd())
library('knitr')
if(!require('lrd')){
 source("lrd/R/functions.r")
 source("lrd/R/simulations.r")
 source("lrd/R/displaySim.r")
}
```

```
## Loading required package: lrd
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
## logical.return = TRUE, : there is no package called 'lrd'
```



Initialization. Note that `nreps=0` corresponds to no simulations,
just print results from previously saved simulations.
In order to re-run the simulations, the `nreps`
variable should have been set to a positive integer before initiating this script.


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
source("lrd/R/ddsandwich.R")
set.seed(201609)
st <- system.time(outcomeSim <- totalOutcomeSim(nreps))
save(outcomeSim, file="dataResults/outcomeSim.RData")
cat(paste0(date(), ', nreps=', nreps, '\n'),
    paste(c(names(st),'\n', collapse=T)),
    st,
    file='dataResults/fullOutcomeSim-runtime.txt', append=TRUE)
} else load('dataResults/outcomeSim.RData')
```


```r
levTab <- levels(outcomeSim)
powTab <- power(outcomeSim)
capture.output({

cat('
\\begin{table}
\\footnotesize
\\begin{tabular}{cc|ccccccc|cccccc}
\\hline
&&& \\multicolumn{6}{c}{Treatment Effect$=0$}&\\multicolumn{6}{c}{Treatment Effect$=0.2$}\\\\
&&& \\multicolumn{ 2 }{c}{Permutation}&\\multicolumn{ 2 }{c}{\`\`Limitless\'\'}&\\multicolumn{ 2 }{c}{Local OLS}&\\multicolumn{ 2 }{c}{Permutation}&\\multicolumn{ 2 }{c}{\`\`Limitless\'\'}&\\multicolumn{ 2 }{c}{Local OLS}\\\\
$n$& Error &$b=$ &', paste(rep('0.25&0.5',ncol(levTab)),collapse='&'),'\\\\
\\hline \n')
for(i in 1:nrow(levTab)){
    spec <- strsplit(rownames(levTab)[i],' ')[[1]]
    if(spec[1]=='norm'){
        cat('\\hline \n')
        cat('\\multirow{2}{*}{',round(as.numeric(spec[2])/2),'} & $\\mathcal{N}(0,1)$ &&')
    } else cat(' & $t_3$ &&')
    cat(paste(round(c(levTab[i,],powTab[i,]),2),collapse='&'),'\\\\ \n')
}
cat('\\hline
\\end{tabular}
\\caption{Proportion of ',length(outcomeSim[[1]]),' simulations resulting in a p-value below $\\alpha=0.05$ using either permutation tests, limitless or local OLS methods. The left column gives sample sizes with $b=0.5$; the sample size when $b=0.25$ is roughly half the listed $n$ value. When the treatment effect is zero (left) these are empirical estimates of size; otherwise (right), they are estimates of power.}
\\label{tab:level}',sep='')
cat('\\end{table}\n')
},file="lrd/inst/tab-levelSimulation.tex")

kable(levTab,caption = 'Empirical size for hypothesis tests',digits = 2)
```



|          | cft 25| cft 5| sh 25| sh 5| ik 25| ik 5|
|:---------|------:|-----:|-----:|----:|-----:|----:|
|norm 100  |   0.06|  0.13|  0.09| 0.07|  0.09| 0.07|
|t 100     |   0.05|  0.10|  0.09| 0.07|  0.09| 0.07|
|norm 500  |   0.11|  0.48|  0.06| 0.05|  0.06| 0.05|
|t 500     |   0.09|  0.36|  0.05| 0.05|  0.05| 0.06|
|norm 5000 |   0.57|  1.00|  0.05| 0.05|  0.05| 0.05|
|t 5000    |   0.43|  1.00|  0.05| 0.06|  0.05| 0.05|

```r
kable(powTab,caption = 'Empirical power for hypothesis tests, treatment effect =0.2',digits = 2)
```



|          | cft 25| cft 5| sh 25| sh 5| ik 25| ik 5|
|:---------|------:|-----:|-----:|----:|-----:|----:|
|norm 100  |   0.11|  0.33|  0.10| 0.08|  0.11| 0.08|
|t 100     |   0.09|  0.23|  0.09| 0.08|  0.09| 0.07|
|norm 500  |   0.41|  0.93|  0.10| 0.12|  0.09| 0.12|
|t 500     |   0.29|  0.79|  0.08| 0.10|  0.06| 0.08|
|norm 5000 |   1.00|  1.00|  0.41| 0.67|  0.36| 0.66|
|t 5000    |   0.99|  1.00|  0.30| 0.51|  0.16| 0.29|


The polynomial sim was run in two parts: first for robust regression
and OLS, and next for local linear regression with the IK bandwidth.


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
source("lrd/R/ddsandwich.R")
set.seed(201609)
st2 <- system.time(totalPoly <- totalOutcomeSim(nreps))
st3 <- system.time(ikp <- totalPolySimIK(nreps))
save(totalPoly,file="dataResults/totalPolySim.RData")
save(ikp,file="dataResults/ikp.RData")
cat(paste0(date(), ', nreps=', nreps, '\n'),
    paste(c(names(st),'\n', collapse=T)),
    st,
    file='dataResults/totalPolySim-runtime.txt', append=TRUE)
} else{
    load('dataResults/totalPolySim.RData')
    load('dataResults/ikp.RData')
    }
```

The following gives the code for Table 4 in the paper:


```r
capture.output(
prntTab(totalPoly,ikp,full=FALSE,caption=paste0('Results from ',ncol(totalPoly[[1]]),' simulations of polynomial specifications for RDD analysis, using robust regression and OLS, using all the data, and local linear regression with a triangular kernel and the \\citet{imbens2012optimal} bandwidth. The sample size for all runs was 500, the error distribution was $t_3$, and there was no treatment effect. The data generating models are those depicted in Figure \\ref{fig:dgms}.'),label='tab:poly'),
file="lrd/inst/tab-polynomialSimulation.tex")
kable(prntTab(totalPoly,ikp,full=TRUE,md=TRUE),
      caption='Full results for polynomial simulation',digits=2)
```



|                       | Rob, deg= 1| Rob, deg= 2| Rob, deg= 3| Rob, deg= 4| OLS, deg= 1| OLS, deg= 2| OLS, deg= 3| OLS, deg= 4| Loc.Lin|
|:----------------------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-------:|
|lin t err level        |        0.37|        0.39|        0.06|        0.06|        0.40|        0.27|        0.07|        0.06|    0.08|
|lin t err RMSE         |        0.36|        0.36|        0.25|        0.25|        0.36|        1.07|        2.88|       13.15|    0.29|
|lin t err bias         |       -0.31|       -0.31|       -0.01|       -0.01|       -0.31|        0.85|        0.88|       -4.38|    0.00|
|lin t err sd           |        0.18|        0.18|        0.25|        0.25|        0.18|        0.65|        2.74|       12.41|    0.29|
|antiSym t err level    |        0.91|        0.91|        0.05|        0.05|        0.93|        0.78|        0.10|        0.11|    0.07|
|antiSym t err RMSE     |        0.64|        0.64|        0.24|        0.24|        0.65|        1.84|        3.24|       15.42|    0.30|
|antiSym t err bias     |       -0.61|       -0.61|       -0.02|       -0.02|       -0.62|        1.73|        1.75|       -8.88|    0.01|
|antiSym t err sd       |        0.17|        0.17|        0.24|        0.24|        0.18|        0.64|        2.73|       12.61|    0.30|
|oneSide t err level    |        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|    0.07|
|oneSide t err RMSE     |        0.18|        0.18|        0.25|        0.24|        0.18|        0.64|        2.76|       12.73|    0.28|
|oneSide t err bias     |        0.00|        0.00|        0.00|        0.00|        0.00|        0.01|        0.00|       -0.40|    0.01|
|oneSide t err sd       |        0.18|        0.18|        0.25|        0.24|        0.18|        0.64|        2.76|       12.72|    0.28|
|lin norm err level     |        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|        0.06|    0.07|
|lin norm err RMSE      |        0.23|        0.23|        0.30|        0.30|        0.31|        1.08|        4.72|       21.82|    0.49|
|lin norm err bias      |        0.00|        0.00|        0.00|        0.00|        0.00|        0.01|        0.02|       -0.37|    0.00|
|lin norm err sd        |        0.23|        0.23|        0.30|        0.30|        0.31|        1.08|        4.72|       21.82|    0.49|
|antiSym norm err level |        0.80|        0.80|        0.05|        0.05|        0.55|        0.37|        0.06|        0.08|    0.08|
|antiSym norm err RMSE  |        0.64|        0.64|        0.30|        0.30|        0.70|        2.04|        4.94|       23.77|    0.51|
|antiSym norm err bias  |       -0.61|       -0.61|       -0.03|       -0.03|       -0.63|        1.73|        1.80|       -8.89|   -0.01|
|antiSym norm err sd    |        0.20|        0.20|        0.30|        0.30|        0.31|        1.08|        4.61|       22.05|    0.51|
|oneSide norm err level |        0.27|        0.28|        0.06|        0.05|        0.18|        0.13|        0.06|        0.05|    0.07|
|oneSide norm err RMSE  |        0.38|        0.38|        0.30|        0.30|        0.44|        1.38|        4.86|       22.37|    0.49|
|oneSide norm err bias  |       -0.31|       -0.31|       -0.01|       -0.01|       -0.31|        0.85|        0.92|       -4.09|   -0.02|
|oneSide norm err sd    |        0.23|        0.23|        0.30|        0.30|        0.31|        1.08|        4.77|       21.99|    0.49|


Session information

```r
sessionInfo()
```

```
## R version 3.3.1 (2016-06-21)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.12.3 (Sierra)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] knitr_1.15.1
## 
## loaded via a namespace (and not attached):
##  [1] backports_1.0.5 magrittr_1.5    rprojroot_1.2   rsconnect_0.5  
##  [5] htmltools_0.3.5 tools_3.3.1     yaml_2.1.13     Rcpp_0.12.8    
##  [9] stringi_1.1.1   rmarkdown_1.5   highr_0.6       stringr_1.1.0  
## [13] digest_0.6.10   evaluate_0.10
```
