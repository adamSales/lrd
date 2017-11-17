---
title: "Tables from simulations presented in lrd paper"
author: "lrd authors"
date: "17 November, 2017"
output: html_document
---



General dependencies.

```r
library('knitr')
library('lrd')
library('kableExtra')
```



Initialization. Note that `nreps=0` corresponds to no simulations,
just print results from previously saved simulations.
In order to re-run the simulations, the `nreps`
variable should have been set to a positive integer before initiating this script.

##Level/Power Simulation
The following code (optionally, if `nreps>0`) runs the simulation
reported in Table 3 of "Limitless Regression Discontinuity"


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

set.seed(201609)
st <- system.time(outcomeSim <- totalOutcomeSim(nreps))
save(outcomeSim, file="dataResults/outcomeSim.RData")
cat(paste0(date(), ', nreps=', nreps, '\n'),
    paste(c(names(st),'\n', collapse=T)),
    st,
    file='dataResults/fullOutcomeSim-runtime.txt', append=TRUE)
} else load('dataResults/outcomeSim.RData')
```

This code creates Table 3:

```r
levTab <- simlevels(outcomeSim)
powTab <-simpower(outcomeSim)
capture.output({
 prntOutcomeSim(levTab=levTab,powTab=powTab,
                caption= paste('Proportion of ',ncol(outcomeSim[[1]]),' simulations resulting in a p-value below $\\alpha=0.05$ using either permutation tests, limitless or local OLS methods. When the treatment effect $\tau$ is zero (left) these are empirical estimates of size; otherwise (right), they are estimates of power with a treatment effect of 0.2.'),
                label='tab:level')
},file="tab-levelSimulation.tex")
```
Here are the results, first for the empirical size of $\alpha=0.05$
hypothesis tests:

```r
rownames(levTab) <- rep(c('N(0,1) error','$t_3$ error'),3)
colnames(levTab) <- c('Permutation','Limitless','Local OLS')
kable(levTab,caption = 'Empirical size for hypothesis tests',digits = 2,format='html')%>%
    kable_styling(full_width=FALSE)%>%
    group_rows("n=50",1,2)%>%group_rows("n=250",3,4)%>%group_rows("n=2500",5,6)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Empirical size for hypothesis tests</caption>
 <thead><tr>
<th style="text-align:left;">   </th>
   <th style="text-align:right;"> Permutation </th>
   <th style="text-align:right;"> Limitless </th>
   <th style="text-align:right;"> Local OLS </th>
  </tr></thead>
<tbody>
<tr grouplength="2"><td colspan="4" style="border-bottom: 1px solid;"><strong>n=50</strong></td></tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1"> N(0,1) error </td>
   <td style="text-align:right;"> 0.14 </td>
   <td style="text-align:right;"> 0.07 </td>
   <td style="text-align:right;"> 0.07 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1"> $t_3$ error </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.07 </td>
   <td style="text-align:right;"> 0.06 </td>
  </tr>
<tr grouplength="2"><td colspan="4" style="border-bottom: 1px solid;"><strong>n=250</strong></td></tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1"> N(0,1) error </td>
   <td style="text-align:right;"> 0.48 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1"> $t_3$ error </td>
   <td style="text-align:right;"> 0.34 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
  </tr>
<tr grouplength="2"><td colspan="4" style="border-bottom: 1px solid;"><strong>n=2500</strong></td></tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentLevel="1"> N(0,1) error </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentLevel="1"> $t_3$ error </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
  </tr>
</tbody>
</table>

Here are the results for power, when the treatment effect is 0.2:

```r
rownames(powTab) <- rep(c('N(0,1) error','$t_3$ error'),3)
colnames(powTab) <- c('Permutation','Limitless','Local OLS')
kable(powTab,caption = 'Empirical power for hypothesis tests, treatment effect =0.2',digits = 2,format='html')%>%
    kable_styling(full_width=FALSE)%>%
    group_rows("n=50",1,2)%>%group_rows("n=250",3,4)%>%group_rows("n=2500",5,6)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Empirical power for hypothesis tests, treatment effect =0.2</caption>
 <thead><tr>
<th style="text-align:left;">   </th>
   <th style="text-align:right;"> Permutation </th>
   <th style="text-align:right;"> Limitless </th>
   <th style="text-align:right;"> Local OLS </th>
  </tr></thead>
<tbody>
<tr grouplength="2"><td colspan="4" style="border-bottom: 1px solid;"><strong>n=50</strong></td></tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1"> N(0,1) error </td>
   <td style="text-align:right;"> 0.32 </td>
   <td style="text-align:right;"> 0.08 </td>
   <td style="text-align:right;"> 0.08 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1"> $t_3$ error </td>
   <td style="text-align:right;"> 0.22 </td>
   <td style="text-align:right;"> 0.08 </td>
   <td style="text-align:right;"> 0.07 </td>
  </tr>
<tr grouplength="2"><td colspan="4" style="border-bottom: 1px solid;"><strong>n=250</strong></td></tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1"> N(0,1) error </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.12 </td>
   <td style="text-align:right;"> 0.12 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1"> $t_3$ error </td>
   <td style="text-align:right;"> 0.79 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.08 </td>
  </tr>
<tr grouplength="2"><td colspan="4" style="border-bottom: 1px solid;"><strong>n=2500</strong></td></tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentLevel="1"> N(0,1) error </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.67 </td>
   <td style="text-align:right;"> 0.66 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentLevel="1"> $t_3$ error </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 0.29 </td>
  </tr>
</tbody>
</table>

##Polynomial Simulation


```r
if (!exists('nreps') ) nreps <- 0
nreps
```

```
## [1] 0
```

```r
if (nreps) {


set.seed(201609)
st2 <- system.time(totalPoly <- totalPolySim(nreps))
save(totalPoly,file="dataResults/totalPolySim.RData")
cat(paste0(date(), ', nreps=', nreps, '\n'),
    paste(c(names(st),'\n', collapse=T)),
    st,
    file='dataResults/totalPolySim-runtime.txt', append=TRUE)
} else{
    load('dataResults/totalPolySim.RData')
}
```

The following gives the results in Table 4 of the paper, in addition
to the break-down of RMSE into bias and variance, and analogous
results for normally-distributed errors.


```r
tab.paper <- prntTab(totalPoly,full=FALSE,md=FALSE)
capture.output(
polyLatex(tab.paper,full=FALSE,caption=paste0('Results from ',ncol(totalPoly[[1]]),' simulations of polynomial specifications for RDD analysis, using robust regression and OLS, using all the data, and local linear regression with a triangular kernel and the \\citet{imbens2012optimal} bandwidth. The sample size for all runs was 500, the error distribution was $t_3$, and there was no treatment effect. The data generating models are those depicted in Figure \\ref{fig:dgms}.'),label='tab:poly'),
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

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Full results for polynomial simulation</caption>
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="text-align:center; border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;" colspan="4"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">Limitless</div></th>
<th style="text-align:center; border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;" colspan="4"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">OLS</div></th>
<th style="text-align:center; border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;" colspan="1"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">Loc. Lin.</div></th>
</tr>
<tr>
<th style="text-align:left;">   </th>
   <th style="text-align:right;"> deg=1 </th>
   <th style="text-align:right;"> deg=2 </th>
   <th style="text-align:right;"> deg=3 </th>
   <th style="text-align:right;"> deg=4 </th>
   <th style="text-align:right;"> deg=1 </th>
   <th style="text-align:right;"> deg=2 </th>
   <th style="text-align:right;"> deg=3 </th>
   <th style="text-align:right;"> deg=4 </th>
   <th style="text-align:right;">  </th>
  </tr>
</thead>
<tbody>
<tr grouplength="12"><td colspan="10" style="border-bottom: 1px solid;"><strong>$t_3$ Error</strong></td></tr>
<tr grouplength="4"><td colspan="10" style="border-bottom: 1px solid;"><strong>linear</strong></td></tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> level </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.05 </td>
   <td style="text-align:right;"> 0.07 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> RMSE </td>
   <td style="text-align:right;"> 0.22 </td>
   <td style="text-align:right;"> 0.22 </td>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.30 </td>
   <td style="text-align:right;"> 0.31 </td>
   <td style="text-align:right;"> 1.09 </td>
   <td style="text-align:right;"> 4.75 </td>
   <td style="text-align:right;border-right:1px solid;"> 21.85 </td>
   <td style="text-align:right;"> 0.49 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> bias </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.01 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.02 </td>
   <td style="text-align:right;"> -0.08 </td>
   <td style="text-align:right;border-right:1px solid;"> -0.24 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> sd </td>
   <td style="text-align:right;"> 0.22 </td>
   <td style="text-align:right;"> 0.22 </td>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.30 </td>
   <td style="text-align:right;"> 0.31 </td>
   <td style="text-align:right;"> 1.09 </td>
   <td style="text-align:right;"> 4.75 </td>
   <td style="text-align:right;border-right:1px solid;"> 21.85 </td>
   <td style="text-align:right;"> 0.49 </td>
  </tr>
<tr grouplength="4"><td colspan="10" style="border-bottom: 1px solid;"><strong>antiSym</strong></td></tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> level </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.05 </td>
   <td style="text-align:right;"> 0.08 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> RMSE </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.24 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 2.72 </td>
   <td style="text-align:right;border-right:1px solid;"> 12.70 </td>
   <td style="text-align:right;"> 0.28 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> bias </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.16 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> sd </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.24 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 2.72 </td>
   <td style="text-align:right;border-right:1px solid;"> 12.70 </td>
   <td style="text-align:right;"> 0.28 </td>
  </tr>
<tr grouplength="4"><td colspan="10" style="border-bottom: 1px solid;"><strong>oneSide</strong></td></tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> level </td>
   <td style="text-align:right;"> 0.79 </td>
   <td style="text-align:right;"> 0.79 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.06 </td>
   <td style="text-align:right;"> 0.54 </td>
   <td style="text-align:right;"> 0.37 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.07 </td>
   <td style="text-align:right;"> 0.08 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1"> RMSE </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.30 </td>
   <td style="text-align:right;"> 0.70 </td>
   <td style="text-align:right;"> 2.10 </td>
   <td style="text-align:right;"> 5.16 </td>
   <td style="text-align:right;border-right:1px solid;"> 25.45 </td>
   <td style="text-align:right;"> 0.51 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> bias </td>
   <td style="text-align:right;"> -0.60 </td>
   <td style="text-align:right;"> -0.60 </td>
   <td style="text-align:right;"> -0.02 </td>
   <td style="text-align:right;border-right:1px solid;"> -0.02 </td>
   <td style="text-align:right;"> -0.63 </td>
   <td style="text-align:right;"> 1.73 </td>
   <td style="text-align:right;"> 1.70 </td>
   <td style="text-align:right;border-right:1px solid;"> -9.08 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> sd </td>
   <td style="text-align:right;"> 0.20 </td>
   <td style="text-align:right;"> 0.20 </td>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.30 </td>
   <td style="text-align:right;"> 0.31 </td>
   <td style="text-align:right;"> 1.20 </td>
   <td style="text-align:right;"> 4.88 </td>
   <td style="text-align:right;border-right:1px solid;"> 23.77 </td>
   <td style="text-align:right;"> 0.51 </td>
  </tr>
<tr grouplength="12"><td colspan="10" style="border-bottom: 1px solid; padding-left: 2em;" indentlevel="1"><strong>N(0,1) Error</strong></td></tr>
<tr grouplength="4"><td colspan="10" style="border-bottom: 1px solid;"><strong>linear</strong></td></tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> level </td>
   <td style="text-align:right;"> 0.92 </td>
   <td style="text-align:right;"> 0.92 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.06 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.77 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.11 </td>
   <td style="text-align:right;"> 0.07 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> RMSE </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 0.25 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.25 </td>
   <td style="text-align:right;"> 0.66 </td>
   <td style="text-align:right;"> 1.83 </td>
   <td style="text-align:right;"> 3.23 </td>
   <td style="text-align:right;border-right:1px solid;"> 15.63 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> bias </td>
   <td style="text-align:right;"> -0.62 </td>
   <td style="text-align:right;"> -0.62 </td>
   <td style="text-align:right;"> -0.02 </td>
   <td style="text-align:right;border-right:1px solid;"> -0.02 </td>
   <td style="text-align:right;"> -0.63 </td>
   <td style="text-align:right;"> 1.71 </td>
   <td style="text-align:right;"> 1.76 </td>
   <td style="text-align:right;border-right:1px solid;"> -9.21 </td>
   <td style="text-align:right;"> 0.01 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> sd </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 0.25 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.25 </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 2.71 </td>
   <td style="text-align:right;border-right:1px solid;"> 12.63 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
<tr grouplength="4"><td colspan="10" style="border-bottom: 1px solid;"><strong>antiSym</strong></td></tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> level </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.05 </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 0.13 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.05 </td>
   <td style="text-align:right;"> 0.07 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> RMSE </td>
   <td style="text-align:right;"> 0.39 </td>
   <td style="text-align:right;"> 0.39 </td>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.30 </td>
   <td style="text-align:right;"> 0.45 </td>
   <td style="text-align:right;"> 1.41 </td>
   <td style="text-align:right;"> 4.88 </td>
   <td style="text-align:right;border-right:1px solid;"> 22.49 </td>
   <td style="text-align:right;"> 0.51 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> bias </td>
   <td style="text-align:right;"> -0.32 </td>
   <td style="text-align:right;"> -0.32 </td>
   <td style="text-align:right;"> -0.01 </td>
   <td style="text-align:right;border-right:1px solid;"> -0.01 </td>
   <td style="text-align:right;"> -0.32 </td>
   <td style="text-align:right;"> 0.87 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;border-right:1px solid;"> -4.52 </td>
   <td style="text-align:right;"> -0.01 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1"> sd </td>
   <td style="text-align:right;"> 0.23 </td>
   <td style="text-align:right;"> 0.23 </td>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.30 </td>
   <td style="text-align:right;"> 0.31 </td>
   <td style="text-align:right;"> 1.11 </td>
   <td style="text-align:right;"> 4.79 </td>
   <td style="text-align:right;border-right:1px solid;"> 22.03 </td>
   <td style="text-align:right;"> 0.51 </td>
  </tr>
<tr grouplength="4"><td colspan="10" style="border-bottom: 1px solid;"><strong>oneSide</strong></td></tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1" indentLevel="1"> level </td>
   <td style="text-align:right;"> 0.37 </td>
   <td style="text-align:right;"> 0.38 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.05 </td>
   <td style="text-align:right;"> 0.40 </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> 0.07 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.06 </td>
   <td style="text-align:right;"> 0.08 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1" indentLevel="1"> RMSE </td>
   <td style="text-align:right;"> 0.36 </td>
   <td style="text-align:right;"> 0.36 </td>
   <td style="text-align:right;"> 0.25 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.25 </td>
   <td style="text-align:right;"> 0.36 </td>
   <td style="text-align:right;"> 1.08 </td>
   <td style="text-align:right;"> 2.88 </td>
   <td style="text-align:right;border-right:1px solid;"> 13.12 </td>
   <td style="text-align:right;"> 0.29 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1" indentLevel="1"> bias </td>
   <td style="text-align:right;"> -0.31 </td>
   <td style="text-align:right;"> -0.31 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.00 </td>
   <td style="text-align:right;"> -0.31 </td>
   <td style="text-align:right;"> 0.88 </td>
   <td style="text-align:right;"> 0.83 </td>
   <td style="text-align:right;border-right:1px solid;"> -4.29 </td>
   <td style="text-align:right;"> 0.01 </td>
  </tr>
<tr>
<td style="text-align:left; padding-left: 2em; padding-left: 2em;" indentlevel="1" indentLevel="1"> sd </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 0.25 </td>
   <td style="text-align:right;border-right:1px solid;"> 0.25 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 2.76 </td>
   <td style="text-align:right;border-right:1px solid;"> 12.40 </td>
   <td style="text-align:right;"> 0.29 </td>
  </tr>
</tbody>
</table>


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
## [1] C
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] nnet_7.3-12       RItools_0.2-0     SparseM_1.77     
##  [4] rdd_0.57          Formula_1.2-1     AER_1.2-4        
##  [7] survival_2.40-1   car_2.1-4         lmtest_0.9-34    
## [10] zoo_1.8-0         sandwich_2.4-0    robustbase_0.92-8
## [13] lrd_0.0.2.9000    rmarkdown_1.8     kableExtra_0.6.1 
## [16] knitr_1.15.1     
## 
## loaded via a namespace (and not attached):
##  [1] splines_3.3.1      lattice_0.20-33    colorspace_1.2-6  
##  [4] htmltools_0.3.5    viridisLite_0.2.0  yaml_2.1.13       
##  [7] mgcv_1.8-15        rlang_0.1.2        nloptr_1.0.4      
## [10] withr_1.0.2        plyr_1.8.4         stringr_1.2.0     
## [13] MatrixModels_0.4-1 munsell_0.4.3      svd_0.4           
## [16] rvest_0.3.2        devtools_1.13.2    memoise_1.0.0     
## [19] evaluate_0.10      quantreg_5.29      pbkrtest_0.4-6    
## [22] parallel_3.3.1     DEoptimR_1.0-8     highr_0.6         
## [25] Rcpp_0.12.8        xtable_1.8-2       readr_1.1.1       
## [28] backports_1.1.1    scales_0.4.1       abind_1.4-5       
## [31] lme4_1.1-12        hms_0.3            digest_0.6.10     
## [34] stringi_1.1.1      grid_3.3.1         rprojroot_1.2     
## [37] tools_3.3.1        magrittr_1.5       tibble_1.3.4      
## [40] MASS_7.3-45        Matrix_1.2-6       xml2_1.1.1        
## [43] minqa_1.2.4        httr_1.2.1         R6_2.1.3          
## [46] nlme_3.1-128       git2r_0.15.0       compiler_3.3.1
```
