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
library('lrd')
```

```
## Error in library("lrd"): there is no package called 'lrd'
```

```r
source("lrd/R/functions.r")
source("lrd/R/simulations.r")
source("lrd/R/displaySim.r")
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
sink("lrd/inst/tab-levelSimulation.tex")
cat('
\\begin{table}
\\footnotesize
\\begin{tabular}{cc|ccccccc|cccccc}
\\hline
&&& \\multicolumn{6}{c}{Treatment Effect$=0$}&\\multicolumn{6}{c}{Treatment Effect$=0.2$}\\\\
&&& \\multicolumn{ 2 }{c}{Permutation}&\\multicolumn{ 2 }{c}{\`\`Limitless\'\'}&\\multicolumn{ 2 }{c}{Local OLS}&\\multicolumn{ 2 }{c}{Permutation}&\\multicolumn{ 2 }{c}{\`\`Limitless\'\'}&\\multicolumn{ 2 }{c}{Local OLS}\\\\
$n$& Error &$b=$ &', paste(rep('0.25&0.5',ncol(levTab)),collapse='&'),'\\\\
\\hline \n')
```

```
## 
## \begin{table}
## \footnotesize
## \begin{tabular}{cc|ccccccc|cccccc}
## \hline
## &&& \multicolumn{6}{c}{Treatment Effect$=0$}&\multicolumn{6}{c}{Treatment Effect$=0.2$}\\
## &&& \multicolumn{ 2 }{c}{Permutation}&\multicolumn{ 2 }{c}{``Limitless''}&\multicolumn{ 2 }{c}{Local OLS}&\multicolumn{ 2 }{c}{Permutation}&\multicolumn{ 2 }{c}{``Limitless''}&\multicolumn{ 2 }{c}{Local OLS}\\
## $n$& Error &$b=$ & 0.25&0.5&0.25&0.5&0.25&0.5&0.25&0.5&0.25&0.5&0.25&0.5 \\
## \hline
```

```r
for(i in 1:nrow(levTab)){
    spec <- strsplit(rownames(levTab)[i],' ')[[1]]
    if(spec[1]=='norm'){
        cat('\\hline \n')
        cat('\\multirow{2}{*}{',round(as.numeric(spec[2])/2),'} & $\\mathcal{N}(0,1)$ &&')
    } else cat(' & $t_3$ &&')
    cat(paste(round(c(levTab[i,],powTab[i,]),2),collapse='&'),'\\\\ \n')
}
```

```
## \hline 
## \multirow{2}{*}{ 50 } & $\mathcal{N}(0,1)$ &&0.06&0.13&0.09&0.07&0.09&0.07&0.11&0.33&0.1&0.08&0.11&0.08 \\ 
##  & $t_3$ &&0.05&0.1&0.09&0.07&0.09&0.07&0.09&0.23&0.09&0.08&0.09&0.07 \\ 
## \hline 
## \multirow{2}{*}{ 250 } & $\mathcal{N}(0,1)$ &&0.11&0.48&0.06&0.05&0.06&0.05&0.41&0.93&0.1&0.12&0.09&0.12 \\ 
##  & $t_3$ &&0.09&0.36&0.05&0.05&0.05&0.06&0.29&0.79&0.08&0.1&0.06&0.08 \\ 
## \hline 
## \multirow{2}{*}{ 2500 } & $\mathcal{N}(0,1)$ &&0.57&1&0.05&0.05&0.05&0.05&1&1&0.41&0.67&0.36&0.66 \\ 
##  & $t_3$ &&0.43&1&0.05&0.06&0.05&0.05&0.99&1&0.3&0.51&0.16&0.29 \\
```

```r
cat('\\hline
\\end{tabular}
\\caption{Proportion of ',length(outcomeSim[[1]]),' simulations resulting in a p-value below $\\alpha=0.05$ using either permutation tests, limitless or local OLS methods. The left column gives sample sizes with $b=0.5$; the sample size when $b=0.25$ is roughly half the listed $n$ value. When the treatment effect is zero (left) these are empirical estimates of size; otherwise (right), they are estimates of power.}
\\label{tab:level}',sep='')
```

```
## \hline
## \end{tabular}
## \caption{Proportion of 5000 simulations resulting in a p-value below $\alpha=0.05$ using either permutation tests, limitless or local OLS methods. The left column gives sample sizes with $b=0.5$; the sample size when $b=0.25$ is roughly half the listed $n$ value. When the treatment effect is zero (left) these are empirical estimates of size; otherwise (right), they are estimates of power.}
## \label{tab:level}
```

```r
cat('\\end{table}\n')
```

```
## \end{table}
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
prntTab(totalPoly,ikp,full=FALSE,caption=paste0('Results from ',ncol(totalPoly[[1]]),' simulations of polynomial specifications for RDD analysis, using robust regression and OLS, using all the data, and local linear regression with a triangular kernel and the \\citet{imbens2012optimal} bandwidth. The sample size for all runs was 500, the error distribution was $t_3$, and there was no treatment effect. The data generating models are those depicted in Figure \\ref{fig:dgms}.'),label='tab:poly')
```

```
## 
##         \begin{table}[ht]
## \centering
## \begin{tabular}{cr|llll|llll|l| } 
##   \hline 
## && \multicolumn{4}{c|}{Limitless} &  \multicolumn{4}{c|}{OLS} &\makecell[c]{Local\\Linear}  \\
##  &degree&1&2&3&4&1&2&3&4&   \\
## \hline
## \hline
## \multirow{ 2 }{*}{ Linear }& level &0.05&0.05&0.05&0.05&0.05&0.05&0.05&0.06&0.07\\ 
## & RMSE &0.23&0.23&0.30&0.30&0.31&1.08&4.72&21.82&0.49\\ 
## \hline
## \hline
## \multirow{ 2 }{*}{ Anti-Sym }& level &0.80&0.80&0.05&0.05&0.55&0.37&0.06&0.08&0.08\\ 
## & RMSE &0.64&0.64&0.30&0.30&0.70&2.04&4.94&23.77&0.51\\ 
## \hline
## \hline
## \multirow{ 2 }{*}{ One-Side }& level &0.27&0.28&0.06&0.05&0.18&0.13&0.06&0.05&0.07\\ 
## & RMSE &0.38&0.38&0.30&0.30&0.44&1.38&4.86&22.37&0.49\\ 
## 
##  \hline
## \end{tabular}
## \caption{Results from 5000 simulations of polynomial specifications for RDD analysis, using robust regression and OLS, using all the data, and local linear regression with a triangular kernel and the \citet{imbens2012optimal} bandwidth. The sample size for all runs was 500, the error distribution was $t_3$, and there was no treatment effect. The data generating models are those depicted in Figure \ref{fig:dgms}.}
## \label{tab:poly}
## \end{table}
```


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
## [1] C
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] knitr_1.15.1
## 
## loaded via a namespace (and not attached):
## [1] compiler_3.3.1 magrittr_1.5   tools_3.3.1    stringi_1.1.1 
## [5] stringr_1.1.0  evaluate_0.10
```
