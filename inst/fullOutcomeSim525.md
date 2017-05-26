---
title: "Tables from simulations presented in lrd paper"
author: "lrd authors"
date: "25 May, 2017"
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
## [1] 100
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
        cat('\\multirow{2}{*}{',round(as.numeric(spec[2])),'} & $\\mathcal{N}(0,1)$ &&')
    } else cat(' & $t_3$ &&')
    cat(paste(round(c(levTab[i,],powTab[i,])*100,1),collapse='&'),'\\\\ \n')
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
|norm 50   |   0.03|  0.15|  0.07| 0.07|  0.05| 0.05|
|t 50      |   0.05|  0.09|  0.09| 0.10|  0.11| 0.05|
|norm 250  |   0.09|  0.43|  0.04| 0.10|  0.04| 0.07|
|t 250     |   0.05|  0.32|  0.04| 0.06|  0.07| 0.04|
|norm 2500 |   0.56|  1.00|  0.04| 0.04|  0.05| 0.04|
|t 2500    |   0.45|  1.00|  0.05| 0.01|  0.04| 0.05|

```r
kable(powTab,caption = 'Empirical power for hypothesis tests, treatment effect =0.2',digits = 2)
```



|          | cft 25| cft 5| sh 25| sh 5| ik 25| ik 5|
|:---------|------:|-----:|-----:|----:|-----:|----:|
|norm 50   |   0.07|  0.27|  0.12| 0.09|  0.10| 0.06|
|t 50      |   0.08|  0.26|  0.10| 0.09|  0.11| 0.05|
|norm 250  |   0.46|  0.93|  0.10| 0.06|  0.11| 0.09|
|t 250     |   0.32|  0.84|  0.11| 0.09|  0.10| 0.10|
|norm 2500 |   1.00|  1.00|  0.39| 0.67|  0.43| 0.67|
|t 2500    |   1.00|  1.00|  0.34| 0.55|  0.20| 0.28|


The polynomial sim was run in two parts: first for robust regression
and OLS, and next for local linear regression with the IK bandwidth.


```r
if (!exists('nreps') ) nreps <- 0
nreps
```

```
## [1] 100
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
## [1] "2017-05-25 11:18:58 CDT"
## lin   FALSE 
## [1] "2017-05-25 11:19:01 CDT"
## antiSym   TRUE 
## [1] "2017-05-25 11:19:04 CDT"
## antiSym   FALSE 
## [1] "2017-05-25 11:19:07 CDT"
## oneSide   TRUE 
## [1] "2017-05-25 11:19:10 CDT"
## oneSide   FALSE 
## [1] "2017-05-25 11:19:13 CDT"
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
|lin t err level        |        0.38|        0.35|        0.03|        0.03|        0.44|        0.32|        0.02|        0.07|    0.06|
|lin t err RMSE         |        0.35|        0.36|        0.22|        0.22|        0.37|        1.11|        2.77|       12.54|    0.30|
|lin t err bias         |       -0.30|       -0.31|        0.01|        0.01|       -0.32|        0.94|        0.83|       -3.24|    0.01|
|lin t err sd           |        0.18|        0.18|        0.22|        0.22|        0.18|        0.59|        2.66|       12.17|    0.30|
|antiSym t err level    |        0.91|        0.92|        0.04|        0.04|        0.93|        0.83|        0.08|        0.08|    0.08|
|antiSym t err RMSE     |        0.63|        0.63|        0.23|        0.23|        0.66|        1.82|        3.08|       14.54|    0.31|
|antiSym t err bias     |       -0.61|       -0.61|       -0.02|       -0.02|       -0.63|        1.74|        1.84|       -8.43|    0.00|
|antiSym t err sd       |        0.17|        0.17|        0.23|        0.23|        0.19|        0.55|        2.48|       11.91|    0.32|
|oneSide t err level    |        0.07|        0.07|        0.05|        0.05|        0.05|        0.04|        0.03|        0.03|    0.10|
|oneSide t err RMSE     |        0.18|        0.18|        0.23|        0.23|        0.18|        0.65|        2.85|       11.08|    0.29|
|oneSide t err bias     |        0.02|        0.02|        0.02|        0.02|        0.02|        0.03|       -0.40|        1.18|   -0.02|
|oneSide t err sd       |        0.18|        0.18|        0.23|        0.23|        0.18|        0.65|        2.83|       11.07|    0.29|
|lin norm err level     |        0.04|        0.04|        0.04|        0.04|        0.02|        0.02|        0.06|        0.10|    0.05|
|lin norm err RMSE      |        0.21|        0.22|        0.29|        0.29|        0.29|        0.98|        4.70|       23.00|    0.43|
|lin norm err bias      |        0.02|        0.02|        0.02|        0.02|        0.03|       -0.02|        0.45|       -0.10|    0.06|
|lin norm err sd        |        0.21|        0.22|        0.29|        0.29|        0.29|        0.98|        4.70|       23.12|    0.43|
|antiSym norm err level |        0.72|        0.73|        0.07|        0.07|        0.44|        0.43|        0.08|        0.07|    0.11|
|antiSym norm err RMSE  |        0.61|        0.60|        0.30|        0.30|        0.63|        2.15|        5.06|       23.97|    0.51|
|antiSym norm err bias  |       -0.57|       -0.57|        0.02|        0.02|       -0.56|        1.85|        1.82|       -9.56|    0.02|
|antiSym norm err sd    |        0.21|        0.21|        0.30|        0.30|        0.29|        1.11|        4.75|       22.09|    0.51|
|oneSide norm err level |        0.28|        0.33|        0.03|        0.03|        0.20|        0.12|        0.08|        0.03|    0.07|
|oneSide norm err RMSE  |        0.38|        0.39|        0.29|        0.29|        0.44|        1.37|        4.80|       20.14|    0.45|
|oneSide norm err bias  |       -0.32|       -0.33|       -0.02|       -0.02|       -0.35|        0.86|        0.57|       -3.84|   -0.05|
|oneSide norm err sd    |        0.21|        0.22|        0.29|        0.29|        0.28|        1.06|        4.79|       19.87|    0.45|


Session information

```r
sessionInfo()
```

```
## R version 3.3.2 (2016-10-31)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7601) Service Pack 1
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
##  [1] nnet_7.3-12       RItools_0.1-15    SparseM_1.7      
##  [4] rdd_0.57          Formula_1.2-1     AER_1.2-4        
##  [7] survival_2.39-5   car_2.1-3         lmtest_0.9-34    
## [10] zoo_1.7-13        sandwich_2.3-4    robustbase_0.92-7
## [13] knitr_1.14        lrd_0.0.0.9000   
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.7        highr_0.6          compiler_3.3.2    
##  [4] DEoptimR_1.0-8     nloptr_1.0.4       formatR_1.4       
##  [7] tools_3.3.2        lme4_1.1-12        evaluate_0.10     
## [10] nlme_3.1-128       lattice_0.20-34    mgcv_1.8-15       
## [13] Matrix_1.2-7.1     parallel_3.3.2     stringr_1.0.0     
## [16] MatrixModels_0.4-1 grid_3.3.2         minqa_1.2.4       
## [19] magrittr_1.5       MASS_7.3-45        splines_3.3.2     
## [22] svd_0.4            abind_1.4-3        pbkrtest_0.4-6    
## [25] xtable_1.8-2       quantreg_5.29      stringi_1.1.1
```
