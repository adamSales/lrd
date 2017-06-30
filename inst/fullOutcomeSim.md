---
title: "Appendix D, tables from simulations"
author: "lrd authors"
date: "25 May, 2017"
output: html_document
---



General dependencies.

```r
library('knitr')
stopifnot(require('lrd'))
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
set.seed(201609)
st <- system.time(outcomeSim <- lrd::totalOutcomeSim(nreps))
save(outcomeSim, file="dataResults/outcomeSim.RData")
cat(paste0(date(), ', nreps=', nreps, '\n'),
    paste(c(names(st),'\n', collapse=T)),
    st,
    file='dataResults/fullOutcomeSim-runtime.txt', append=TRUE)
} else load('dataResults/outcomeSim.RData')
```


```r
levTab <- lrd::levels(outcomeSim)
powTab <-lrd::power(outcomeSim)
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
},file="dataResults/tab-levelSimulation.tex")

kable(levTab,caption = 'Empirical size for hypothesis tests',digits = 2)
```



|          | cft 25| cft 5| sh 25| sh 5| ik 25| ik 5|
|:---------|------:|-----:|-----:|----:|-----:|----:|
|norm 50   |   0.06|  0.14|  0.09| 0.07|  0.10| 0.07|
|t 50      |   0.06|  0.11|  0.08| 0.06|  0.08| 0.06|
|norm 250  |   0.11|  0.49|  0.05| 0.05|  0.06| 0.05|
|t 250     |   0.09|  0.35|  0.06| 0.05|  0.06| 0.05|
|norm 2500 |   0.58|  1.00|  0.05| 0.05|  0.05| 0.05|
|t 2500    |   0.42|  1.00|  0.05| 0.05|  0.05| 0.05|

```r
kable(powTab,caption = 'Empirical power for hypothesis tests, treatment effect =0.2',digits = 2)
```



|          | cft 25| cft 5| sh 25| sh 5| ik 25| ik 5|
|:---------|------:|-----:|-----:|----:|-----:|----:|
|norm 50   |   0.11|  0.32|  0.09| 0.08|  0.10| 0.08|
|t 50      |   0.09|  0.23|  0.09| 0.07|  0.09| 0.07|
|norm 250  |   0.41|  0.93|  0.09| 0.13|  0.08| 0.12|
|t 250     |   0.29|  0.80|  0.07| 0.10|  0.06| 0.07|
|norm 2500 |   1.00|  1.00|  0.41| 0.68|  0.37| 0.67|
|t 2500    |   1.00|  1.00|  0.29| 0.52|  0.17| 0.30|


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
set.seed(201609)
st2 <- system.time(totalPoly <- lrd::totalPolySim(nreps))
st3 <- system.time(ikp <- lrd::totalPolySimIK(nreps))
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
## [1] "2017-05-26 02:41:25 CDT"
## lin   FALSE 
## [1] "2017-05-26 02:43:20 CDT"
## antiSym   TRUE 
## [1] "2017-05-26 02:45:13 CDT"
## antiSym   FALSE 
## [1] "2017-05-26 02:47:04 CDT"
## oneSide   TRUE 
## [1] "2017-05-26 02:48:54 CDT"
## oneSide   FALSE 
## [1] "2017-05-26 02:50:46 CDT"
```

The following gives the results in Table 4 of the paper, in addition
to the break-down of RMSE into bias and variance, and analogous
results for normally-distributed errors.


```r
capture.output(
lrd::prntTab(totalPoly,ikp,full=FALSE,caption=paste0('Results from ',ncol(totalPoly[[1]]),' simulations of polynomial specifications for RDD analysis, using robust regression and OLS, using all the data, and local linear regression with a triangular kernel and the \\citet{imbens2012optimal} bandwidth. The sample size for all runs was 500, the error distribution was $t_3$, and there was no treatment effect. The data generating models are those depicted in Figure \\ref{fig:dgms}.'),label='tab:poly'),
file="dataResults/tab-polynomialSimulation.tex")
kable(prntTab(totalPoly,ikp,full=TRUE,md=TRUE),
      caption='Full results for polynomial simulation',digits=2)
```



|                       | Rob, deg= 1| Rob, deg= 2| Rob, deg= 3| Rob, deg= 4| OLS, deg= 1| OLS, deg= 2| OLS, deg= 3| OLS, deg= 4| Loc.Lin|
|:----------------------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-------:|
|lin t err level        |        0.05|        0.04|        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|    0.06|
|lin t err RMSE         |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|lin t err bias         |        0.00|        0.00|        0.00|        0.00|       -0.01|       -0.02|       -0.04|        0.02|    0.00|
|lin t err sd           |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|lin t err level        |        0.05|        0.05|        0.06|        0.06|        0.05|        0.05|        0.05|        0.05|    0.07|
|lin t err RMSE         |        0.18|        0.18|        0.24|        0.24|        0.18|        0.62|        2.71|       12.59|    0.28|
|lin t err bias         |        0.00|        0.00|        0.00|        0.00|        0.00|        0.00|        0.01|        0.16|    0.00|
|lin t err sd           |        0.18|        0.18|        0.24|        0.24|        0.18|        0.62|        2.71|       12.59|    0.28|
|lin t err level        |        0.05|        0.04|        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|    0.06|
|lin t err RMSE         |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|lin t err bias         |        0.00|        0.00|        0.00|        0.00|       -0.01|       -0.02|       -0.04|        0.02|    0.00|
|lin t err sd           |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|lin t err level        |        0.78|        0.78|        0.05|        0.05|        0.54|        0.37|        0.07|        0.07|    0.07|
|lin t err RMSE         |        0.63|        0.63|        0.30|        0.30|        0.70|        2.07|        5.17|       24.14|    0.50|
|antiSym t err bias     |       -0.60|       -0.60|       -0.02|       -0.02|       -0.62|        1.74|        1.80|       -9.47|    0.00|
|antiSym t err sd       |        0.20|        0.20|        0.30|        0.30|        0.32|        1.12|        4.85|       22.21|    0.50|
|antiSym t err level    |        0.92|        0.92|        0.06|        0.06|        0.93|        0.77|        0.10|        0.11|    0.07|
|antiSym t err RMSE     |        0.64|        0.64|        0.25|        0.25|        0.65|        1.84|        3.25|       15.39|    0.30|
|antiSym t err bias     |       -0.61|       -0.61|       -0.02|       -0.01|       -0.63|        1.72|        1.77|       -8.63|    0.01|
|antiSym t err sd       |        0.18|        0.18|        0.25|        0.25|        0.18|        0.64|        2.73|       12.74|    0.30|
|antiSym t err level    |        0.05|        0.04|        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|    0.06|
|antiSym t err RMSE     |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|antiSym t err bias     |        0.00|        0.00|        0.00|        0.00|       -0.01|       -0.02|       -0.04|        0.02|    0.00|
|antiSym t err sd       |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|antiSym t err level    |        0.05|        0.05|        0.06|        0.06|        0.05|        0.05|        0.05|        0.05|    0.07|
|antiSym t err RMSE     |        0.18|        0.18|        0.24|        0.24|        0.18|        0.62|        2.71|       12.59|    0.28|
|antiSym t err bias     |        0.00|        0.00|        0.00|        0.00|        0.00|        0.00|        0.01|        0.16|    0.00|
|antiSym t err sd       |        0.18|        0.18|        0.24|        0.24|        0.18|        0.62|        2.71|       12.59|    0.28|
|oneSide t err level    |        0.05|        0.04|        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|    0.06|
|oneSide t err RMSE     |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|oneSide t err bias     |        0.00|        0.00|        0.00|        0.00|       -0.01|       -0.02|       -0.04|        0.02|    0.00|
|oneSide t err sd       |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|oneSide t err level    |        0.78|        0.78|        0.05|        0.05|        0.54|        0.37|        0.07|        0.07|    0.07|
|oneSide t err RMSE     |        0.63|        0.63|        0.30|        0.30|        0.70|        2.07|        5.17|       24.14|    0.50|
|oneSide t err bias     |       -0.60|       -0.60|       -0.02|       -0.02|       -0.62|        1.74|        1.80|       -9.47|    0.00|
|oneSide t err sd       |        0.20|        0.20|        0.30|        0.30|        0.32|        1.12|        4.85|       22.21|    0.50|
|oneSide t err level    |        0.28|        0.28|        0.06|        0.06|        0.19|        0.13|        0.05|        0.05|    0.07|
|oneSide t err RMSE     |        0.39|        0.38|        0.31|        0.31|        0.45|        1.40|        4.76|       22.35|    0.49|
|oneSide t err bias     |       -0.31|       -0.31|       -0.01|       -0.01|       -0.32|        0.85|        0.90|       -4.87|   -0.01|
|oneSide t err sd       |        0.23|        0.23|        0.31|        0.31|        0.31|        1.11|        4.68|       21.81|    0.49|
|oneSide t err level    |        0.38|        0.39|        0.06|        0.07|        0.40|        0.28|        0.06|        0.07|    0.08|
|oneSide t err RMSE     |        0.36|        0.36|        0.25|        0.25|        0.36|        1.08|        2.91|       13.46|    0.29|
|lin norm err bias      |       -0.31|       -0.31|       -0.01|       -0.01|       -0.31|        0.86|        0.90|       -4.18|    0.00|
|lin norm err sd        |        0.19|        0.18|        0.25|        0.25|        0.18|        0.65|        2.77|       12.79|    0.29|
|lin norm err level     |        0.05|        0.04|        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|    0.06|
|lin norm err RMSE      |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|lin norm err bias      |        0.00|        0.00|        0.00|        0.00|       -0.01|       -0.02|       -0.04|        0.02|    0.00|
|lin norm err sd        |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|lin norm err level     |        0.05|        0.05|        0.06|        0.06|        0.05|        0.05|        0.05|        0.05|    0.07|
|lin norm err RMSE      |        0.18|        0.18|        0.24|        0.24|        0.18|        0.62|        2.71|       12.59|    0.28|
|lin norm err bias      |        0.00|        0.00|        0.00|        0.00|        0.00|        0.00|        0.01|        0.16|    0.00|
|lin norm err sd        |        0.18|        0.18|        0.24|        0.24|        0.18|        0.62|        2.71|       12.59|    0.28|
|lin norm err level     |        0.05|        0.04|        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|    0.06|
|lin norm err RMSE      |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|lin norm err bias      |        0.00|        0.00|        0.00|        0.00|       -0.01|       -0.02|       -0.04|        0.02|    0.00|
|lin norm err sd        |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|antiSym norm err level |        0.78|        0.78|        0.05|        0.05|        0.54|        0.37|        0.07|        0.07|    0.07|
|antiSym norm err RMSE  |        0.63|        0.63|        0.30|        0.30|        0.70|        2.07|        5.17|       24.14|    0.50|
|antiSym norm err bias  |       -0.60|       -0.60|       -0.02|       -0.02|       -0.62|        1.74|        1.80|       -9.47|    0.00|
|antiSym norm err sd    |        0.20|        0.20|        0.30|        0.30|        0.32|        1.12|        4.85|       22.21|    0.50|
|antiSym norm err level |        0.92|        0.92|        0.06|        0.06|        0.93|        0.77|        0.10|        0.11|    0.07|
|antiSym norm err RMSE  |        0.64|        0.64|        0.25|        0.25|        0.65|        1.84|        3.25|       15.39|    0.30|
|antiSym norm err bias  |       -0.61|       -0.61|       -0.02|       -0.01|       -0.63|        1.72|        1.77|       -8.63|    0.01|
|antiSym norm err sd    |        0.18|        0.18|        0.25|        0.25|        0.18|        0.64|        2.73|       12.74|    0.30|
|antiSym norm err level |        0.05|        0.04|        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|    0.06|
|antiSym norm err RMSE  |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|antiSym norm err bias  |        0.00|        0.00|        0.00|        0.00|       -0.01|       -0.02|       -0.04|        0.02|    0.00|
|antiSym norm err sd    |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|antiSym norm err level |        0.05|        0.05|        0.06|        0.06|        0.05|        0.05|        0.05|        0.05|    0.07|
|antiSym norm err RMSE  |        0.18|        0.18|        0.24|        0.24|        0.18|        0.62|        2.71|       12.59|    0.28|
|oneSide norm err bias  |        0.00|        0.00|        0.00|        0.00|        0.00|        0.00|        0.01|        0.16|    0.00|
|oneSide norm err sd    |        0.18|        0.18|        0.24|        0.24|        0.18|        0.62|        2.71|       12.59|    0.28|
|oneSide norm err level |        0.05|        0.04|        0.05|        0.05|        0.05|        0.05|        0.05|        0.05|    0.06|
|oneSide norm err RMSE  |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|oneSide norm err bias  |        0.00|        0.00|        0.00|        0.00|       -0.01|       -0.02|       -0.04|        0.02|    0.00|
|oneSide norm err sd    |        0.22|        0.22|        0.30|        0.30|        0.31|        1.08|        4.71|       21.83|    0.47|
|oneSide norm err level |        0.78|        0.78|        0.05|        0.05|        0.54|        0.37|        0.07|        0.07|    0.07|
|oneSide norm err RMSE  |        0.63|        0.63|        0.30|        0.30|        0.70|        2.07|        5.17|       24.14|    0.50|
|oneSide norm err bias  |       -0.60|       -0.60|       -0.02|       -0.02|       -0.62|        1.74|        1.80|       -9.47|    0.00|
|oneSide norm err sd    |        0.20|        0.20|        0.30|        0.30|        0.32|        1.12|        4.85|       22.21|    0.50|
|oneSide norm err level |        0.28|        0.28|        0.06|        0.06|        0.19|        0.13|        0.05|        0.05|    0.07|
|oneSide norm err RMSE  |        0.39|        0.38|        0.31|        0.31|        0.45|        1.40|        4.76|       22.35|    0.49|
|oneSide norm err bias  |       -0.31|       -0.31|       -0.01|       -0.01|       -0.32|        0.85|        0.90|       -4.87|   -0.01|
|oneSide norm err sd    |        0.23|        0.23|        0.31|        0.31|        0.31|        1.11|        4.68|       21.81|    0.49|


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
