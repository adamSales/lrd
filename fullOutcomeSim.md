---
title: "Tables from simulations presented in lrd paper"
author: "lrd authors"
date: "10 November, 2017"
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

`paperdir` is the directory where LaTeX code for simulation results tables
is saved. If unspecified, the tables are saved in the working directory.

```r
if (!exists('nreps') ) nreps <- 0
if (!exists('paperdir')) paperdir <- '.'
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
st <- system.time(outcomeSim <- lrd:::totalOutcomeSim(nreps))
save(outcomeSim, file="dataResults/outcomeSim.RData")
cat(paste0(date(), ', nreps=', nreps, '\n'),
    paste(c(names(st),'\n', collapse=T)),
    st,
    file='dataResults/fullOutcomeSim-runtime.txt', append=TRUE)
} else load('dataResults/outcomeSim.RData')
```

```
## Error in readChar(con, 5L, useBytes = TRUE): cannot open the connection
```


```r
levTab <- lrd:::levels(outcomeSim)
```

```
## Error in toMat(os[[paste0(n, "_0_", err)]]): object 'outcomeSim' not found
```

```r
powTab <-lrd:::power(outcomeSim)
```

```
## Error in toMat(os[[paste0(n, "_0.2_", err)]]): object 'outcomeSim' not found
```

```r
capture.output({

cat('
\\begin{table}
%\\footnotesize
\\begin{tabular}{cc|ccccccc}
\\hline

&&& \\multicolumn{ 2 }{c}{Permutation}&\\multicolumn{ 2 }{c}{\`\`Limitless\'\'}
 &\\multicolumn{ 2 }{c}{Local OLS}\\\\
$n$& Error &&', paste(rep('Level&Power',ncol(levTab)),collapse='&'),'\\\\
\\hline \n')
for(i in 1:nrow(levTab)){
    spec <- strsplit(rownames(levTab)[i],' ')[[1]]
    if(spec[1]=='norm'){
        cat('\\hline \n')
        cat('\\multirow{2}{*}{',round(as.numeric(spec[2])),'} & $\\mathcal{N}(0,1)$ &&')
    } else cat(' & $t_3$ &&')
    cat(paste(paste(round(levTab[i,]*100),round(powTab[i,]*100),sep='&'),
              collapse='&'),'\\\\ \n')
}
cat('\\hline
\\end{tabular}
\\caption{Proportion of ',ncol(outcomeSim[[1]]),' simulations resulting in a p-value below 
$\\alpha=0.05$ using either permutation tests, limitless or local OLS methods. 
When the treatment effect $\tau$ is zero (left) these are empirical estimates of size; 
otherwise (right), they are estimates of power with a treatment effect of 0.2.}
\\label{tab:level}',sep='')
cat('\\end{table}\n')
},file=paste0(paperdir,"/tab-levelSimulation.tex"))
```

```
## Warning in file(file, if (append) "a" else "w"): cannot open file './tab-
## levelSimulation.tex': Permission denied
```

```
## Error in file(file, if (append) "a" else "w"): cannot open the connection
```

```r
kable(levTab,caption = 'Empirical size for hypothesis tests',digits = 2)
```

```
## Error in inherits(x, "list"): object 'levTab' not found
```

```r
kable(powTab,
      caption = 'Empirical power for hypothesis tests, treatment effect =0.2',digits = 2)
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
## Error in readChar(con, 5L, useBytes = TRUE): cannot open the connection
```

The following gives the results in Table 4 of the paper, in addition
to the break-down of RMSE into bias and variance, and analogous
results for normally-distributed errors.


```r
capture.output(
lrd:::prntTab(totalPoly,ikp,full=FALSE,
  caption=paste0('Results from ',ncol(totalPoly[[1]]),' simulations of polynomial 
                 specifications for RDD analysis, using robust regression and OLS, 
                 using all the data, and local linear regression with a triangular kernel 
                 and the \\citet{imbens2012optimal} bandwidth. The sample size for all runs was 500, 
                 the error distribution was $t_3$, and there was no treatment effect. 
                 The data generating models are those depicted in Figure \\ref{fig:dgms}.'),
  label='tab:poly'),
file="lrd/inst/tab-polynomialSimulation.tex")
```

```
## Warning in file(file, if (append) "a" else "w"): cannot open file 'lrd/
## inst/tab-polynomialSimulation.tex': No such file or directory
```

```
## Error in file(file, if (append) "a" else "w"): cannot open the connection
```

```r
kable(prntTab(totalPoly,ikp,full=TRUE,md=TRUE),
      caption='Full results for polynomial simulation',digits=2)
```

```
## Error in resTab(totalPoly[[paste0(dgm, "_t")]], full = full): object 'totalPoly' not found
```


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
##  [1] lrd_0.0.0.9000    rdd_0.57          Formula_1.2-1    
##  [4] AER_1.2-4         survival_2.40-1   car_2.1-4        
##  [7] lmtest_0.9-34     zoo_1.7-13        sandwich_2.3-4   
## [10] robustbase_0.92-7 xtable_1.8-2      ggplot2_2.2.1    
## [13] knitr_1.15.1     
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.8        highr_0.6          nloptr_1.0.4      
##  [4] DEoptimR_1.0-8     plyr_1.8.4         tools_3.3.1       
##  [7] digest_0.6.10      lme4_1.1-12        evaluate_0.10     
## [10] tibble_1.3.4       gtable_0.2.0       nlme_3.1-128      
## [13] lattice_0.20-33    mgcv_1.8-15        rlang_0.1.2       
## [16] Matrix_1.2-6       parallel_3.3.1     yaml_2.1.13       
## [19] SparseM_1.77       stringr_1.1.0      MatrixModels_0.4-1
## [22] rprojroot_1.2      grid_3.3.1         nnet_7.3-12       
## [25] foreign_0.8-66     rmarkdown_1.5      minqa_1.2.4       
## [28] magrittr_1.5       backports_1.1.1    scales_0.4.1      
## [31] htmltools_0.3.5    MASS_7.3-45        splines_3.3.1     
## [34] rsconnect_0.5      pbkrtest_0.4-6     colorspace_1.2-6  
## [37] labeling_0.3       quantreg_5.29      stringi_1.1.1     
## [40] lazyeval_0.2.0     munsell_0.4.3
```
