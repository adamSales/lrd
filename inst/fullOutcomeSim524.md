---
title: "Tables from simulations presented in lrd paper"
author: "lrd authors"
date: "24 May, 2017"
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
```

```
## Error: 'levels' is not an exported object from 'namespace:lrd'
```

```r
powTab <-lrd:: power(outcomeSim)
```

```
## Error: 'power' is not an exported object from 'namespace:lrd'
```

```r
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
```

```
## Error in ncol(levTab): object 'levTab' not found
```

```r
kable(levTab,caption = 'Empirical size for hypothesis tests',digits = 2)
```

```
## Error in inherits(x, "list"): object 'levTab' not found
```

```r
kable(powTab,caption = 'Empirical power for hypothesis tests, treatment effect =0.2',digits = 2)
```

```
## Error in inherits(x, "list"): object 'powTab' not found
```


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
st2 <- system.time(totalPoly <- lrd::totalOutcomeSim(nreps))
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
## [1] "2017-05-24 21:54:10 CDT"
## lin   FALSE 
## [1] "2017-05-24 21:56:31 CDT"
## antiSym   TRUE 
## [1] "2017-05-24 21:59:03 CDT"
## antiSym   FALSE 
## [1] "2017-05-24 22:01:38 CDT"
## oneSide   TRUE 
## [1] "2017-05-24 22:04:06 CDT"
## oneSide   FALSE 
## [1] "2017-05-24 22:06:36 CDT"
```

The following gives the results in Table 4 of the paper, in addition
to the break-down of RMSE into bias and variance, and analogous
results for normally-distributed errors.


```r
capture.output(
lrd::prntTab(totalPoly,ikp,full=FALSE,caption=paste0('Results from ',ncol(totalPoly[[1]]),' simulations of polynomial specifications for RDD analysis, using robust regression and OLS, using all the data, and local linear regression with a triangular kernel and the \\citet{imbens2012optimal} bandwidth. The sample size for all runs was 500, the error distribution was $t_3$, and there was no treatment effect. The data generating models are those depicted in Figure \\ref{fig:dgms}.'),label='tab:poly'),
file="lrd/inst/tab-polynomialSimulation.tex")
```

```
## Error: 'prntTab' is not an exported object from 'namespace:lrd'
```

```r
kable(prntTab(totalPoly,ikp,full=TRUE,md=TRUE),
      caption='Full results for polynomial simulation',digits=2)
```

```
## Error in inherits(x, "list"): could not find function "prntTab"
```


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
##  [1] Rcpp_0.12.7        compiler_3.3.2     DEoptimR_1.0-8    
##  [4] nloptr_1.0.4       formatR_1.4        tools_3.3.2       
##  [7] lme4_1.1-12        evaluate_0.10      nlme_3.1-128      
## [10] lattice_0.20-34    mgcv_1.8-15        Matrix_1.2-7.1    
## [13] parallel_3.3.2     stringr_1.0.0      MatrixModels_0.4-1
## [16] grid_3.3.2         minqa_1.2.4        magrittr_1.5      
## [19] MASS_7.3-45        splines_3.3.2      svd_0.4           
## [22] abind_1.4-3        pbkrtest_0.4-6     xtable_1.8-2      
## [25] quantreg_5.29      stringi_1.1.1
```
