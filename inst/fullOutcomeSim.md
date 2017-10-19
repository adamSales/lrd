---
title: "Tables from simulations presented in lrd paper"
author: "lrd authors"
date: "16 October, 2017"
output: html_document
---



General dependencies.

```r
library('knitr')
if(!require('lrd')){
 source("lrd/R/functions.r")
 source("lrd/R/simulations.r")
 source("lrd/R/displaySim.r")
}
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
## [1] 5000
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
st <- system.time(outcomeSim <- lrd:::totalOutcomeSim(nreps))
save(outcomeSim, file="dataResults/outcomeSim.RData")
cat(paste0(date(), ', nreps=', nreps, '\n'),
    paste(c(names(st),'\n', collapse=T)),
    st,
    file='dataResults/fullOutcomeSim-runtime.txt', append=TRUE)
} else load('dataResults/outcomeSim.RData')
```


```r
levTab <- lrd:::levels(outcomeSim)
powTab <-lrd:::power(outcomeSim)
capture.output({

cat('
\\begin{table}
\\footnotesize
\\begin{tabular}{cc|ccccccc}
\\hline

&&& \\multicolumn{ 2 }{c}{Permutation}&\\multicolumn{ 2 }{c}{\`\`Limitless\'\'}&\\multicolumn{ 2 }{c}{Local OLS}\\\\
$n$& Error &&', paste(rep('Level&Power',ncol(levTab)),collapse='&'),'\\\\
\\hline \n')
for(i in 1:nrow(levTab)){
    spec <- strsplit(rownames(levTab)[i],' ')[[1]]
    if(spec[1]=='norm'){
        cat('\\hline \n')
        cat('\\multirow{2}{*}{',round(as.numeric(spec[2])),'} & $\\mathcal{N}(0,1)$ &&')
    } else cat(' & $t_3$ &&')
    cat(paste(paste(round(levTab[i,]*100),round(powTab[i,]*100),sep='&'),collapse='&'),'\\\\ \n')
}
cat('\\hline
\\end{tabular}
\\caption{Proportion of ',ncol(outcomeSim[[1]]),' simulations resulting in a p-value below $\\alpha=0.05$ using either permutation tests, limitless or local OLS methods. When the treatment effect $\tau$ is zero (left) these are empirical estimates of size; otherwise (right), they are estimates of power with a treatment effect of 0.2.}
\\label{tab:level}',sep='')
cat('\\end{table}\n')
},file="tab-levelSimulation.tex")

kable(levTab,caption = 'Empirical size for hypothesis tests',digits = 2)
```



|          |  cft|   sh|   ik|
|:---------|----:|----:|----:|
|norm 50   | 0.14| 0.07| 0.07|
|t 50      | 0.10| 0.07| 0.06|
|norm 250  | 0.48| 0.05| 0.05|
|t 250     | 0.34| 0.05| 0.05|
|norm 2500 | 1.00| 0.05| 0.05|
|t 2500    | 1.00| 0.05| 0.05|

```r
kable(powTab,caption = 'Empirical power for hypothesis tests, treatment effect =0.2',digits = 2)
```



|          |  cft|   sh|   ik|
|:---------|----:|----:|----:|
|norm 50   | 0.32| 0.08| 0.08|
|t 50      | 0.22| 0.08| 0.07|
|norm 250  | 0.93| 0.12| 0.12|
|t 250     | 0.79| 0.10| 0.08|
|norm 2500 | 1.00| 0.67| 0.66|
|t 2500    | 1.00| 0.50| 0.29|


The polynomial sim was run in two parts: first for robust regression
and OLS, and next for local linear regression with the IK bandwidth.


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
source("lrd/R/ddsandwich.R")
set.seed(201609)
st2 <- system.time(totalPoly <- lrd:::totalPolySim(nreps))
st3 <- system.time(ikp <- lrd:::totalPolySimIK(nreps))
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

```
## lin   TRUE 
## [1] "2017-10-19 01:46:50 CDT"
## lin   FALSE 
## [1] "2017-10-19 01:49:07 CDT"
## antiSym   TRUE 
## [1] "2017-10-19 01:51:37 CDT"
## antiSym   FALSE 
## [1] "2017-10-19 01:53:55 CDT"
## oneSide   TRUE 
## [1] "2017-10-19 01:56:25 CDT"
## oneSide   FALSE 
## [1] "2017-10-19 01:58:43 CDT"
```

The following gives the results in Table 4 of the paper, in addition
to the break-down of RMSE into bias and variance, and analogous
results for normally-distributed errors.


```r
capture.output(
lrd:::prntTab(totalPoly,ikp,full=FALSE,caption=paste0('Results from ',ncol(totalPoly[[1]]),' simulations of polynomial specifications for RDD analysis, using robust regression and OLS, using all the data, and local linear regression with a triangular kernel and the \\citet{imbens2012optimal} bandwidth. The sample size for all runs was 500, the error distribution was $t_3$, and there was no treatment effect. The data generating models are those depicted in Figure \\ref{fig:dgms}.'),label='tab:poly'),
file="lrd/inst/tab-polynomialSimulation.tex")
kable(prntTab(totalPoly,ikp,full=TRUE,md=TRUE),
      caption='Full results for polynomial simulation',digits=2)
```



|                       | Rob, deg= 1| Rob, deg= 2| Rob, deg= 3| Rob, deg= 4| OLS, deg= 1| OLS, deg= 2| OLS, deg= 3| OLS, deg= 4| Loc.Lin|
|:----------------------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-------:|
|lin t err level        |        0.37|        0.38|        0.05|        0.05|        0.40|        0.29|        0.07|        0.06|    0.08|
|lin t err RMSE         |        0.36|        0.36|        0.25|        0.25|        0.36|        1.08|        2.88|       13.12|    0.29|
|lin t err bias         |       -0.31|       -0.31|        0.00|        0.00|       -0.31|        0.88|        0.83|       -4.29|    0.00|
|lin t err sd           |        0.19|        0.18|        0.25|        0.25|        0.18|        0.64|        2.76|       12.40|    0.29|
|antiSym t err level    |        0.92|        0.92|        0.06|        0.06|        0.93|        0.77|        0.10|        0.11|    0.06|
|antiSym t err RMSE     |        0.64|        0.64|        0.25|        0.25|        0.66|        1.83|        3.23|       15.63|    0.29|
|antiSym t err bias     |       -0.62|       -0.62|       -0.02|       -0.02|       -0.63|        1.71|        1.76|       -9.21|    0.01|
|antiSym t err sd       |        0.18|        0.18|        0.25|        0.25|        0.19|        0.64|        2.71|       12.63|    0.29|
|oneSide t err level    |        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|    0.08|
|oneSide t err RMSE     |        0.18|        0.18|        0.24|        0.24|        0.18|        0.64|        2.72|       12.70|    0.28|
|oneSide t err bias     |        0.00|        0.00|        0.00|        0.00|        0.00|        0.00|        0.00|        0.16|    0.00|
|oneSide t err sd       |        0.18|        0.18|        0.24|        0.24|        0.18|        0.64|        2.72|       12.70|    0.28|
|lin norm err level     |        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|    0.06|
|lin norm err RMSE      |        0.22|        0.22|        0.30|        0.30|        0.31|        1.09|        4.75|       21.85|    0.48|
|lin norm err bias      |        0.00|        0.00|        0.01|        0.01|        0.00|        0.02|       -0.08|       -0.24|    0.01|
|lin norm err sd        |        0.22|        0.22|        0.30|        0.30|        0.31|        1.09|        4.75|       21.85|    0.48|
|antiSym norm err level |        0.79|        0.79|        0.06|        0.06|        0.54|        0.37|        0.06|        0.07|    0.07|
|antiSym norm err RMSE  |        0.64|        0.64|        0.30|        0.30|        0.70|        2.10|        5.16|       25.45|    0.49|
|antiSym norm err bias  |       -0.60|       -0.60|       -0.02|       -0.02|       -0.63|        1.73|        1.70|       -9.08|    0.00|
|antiSym norm err sd    |        0.20|        0.20|        0.30|        0.30|        0.31|        1.20|        4.88|       23.77|    0.49|
|oneSide norm err level |        0.29|        0.30|        0.05|        0.05|        0.19|        0.13|        0.06|        0.05|    0.07|
|oneSide norm err RMSE  |        0.39|        0.39|        0.30|        0.30|        0.45|        1.41|        4.88|       22.49|    0.49|
|oneSide norm err bias  |       -0.32|       -0.32|       -0.01|       -0.01|       -0.32|        0.87|        0.93|       -4.52|    0.00|
|oneSide norm err sd    |        0.23|        0.23|        0.30|        0.30|        0.31|        1.11|        4.79|       22.03|    0.49|


Session information

```r
sessionInfo()
```

```
## R version 3.3.1 (2016-06-21)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.12.6 (Sierra)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] nnet_7.3-12       RItools_0.2-0     SparseM_1.77     
##  [4] rdd_0.57          Formula_1.2-1     AER_1.2-4        
##  [7] survival_2.40-1   car_2.1-4         lmtest_0.9-34    
## [10] zoo_1.7-13        sandwich_2.3-4    robustbase_0.92-7
## [13] knitr_1.15.1      lrd_0.0.0.9000   
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.8        DEoptimR_1.0-8     nloptr_1.0.4      
##  [4] highr_0.6          tools_3.3.1        lme4_1.1-12       
##  [7] evaluate_0.10      nlme_3.1-128       lattice_0.20-33   
## [10] mgcv_1.8-15        Matrix_1.2-6       parallel_3.3.1    
## [13] stringr_1.1.0      MatrixModels_0.4-1 grid_3.3.1        
## [16] minqa_1.2.4        magrittr_1.5       MASS_7.3-45       
## [19] splines_3.3.1      rsconnect_0.5      svd_0.4           
## [22] abind_1.4-5        pbkrtest_0.4-6     xtable_1.8-2      
## [25] quantreg_5.29      stringi_1.1.1
```
