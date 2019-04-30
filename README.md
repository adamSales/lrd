# lrd
Replication material for "Limitless Regression Discontinuity"

To reproduce the results in the paper:

1. Make sure R is up-to-date with required packages installed:
  - We used R version 3.4.4
  - Required packages to carry out analyses:
	- `robustbase`
	- `rdd`
	- `sandwich`
	- `nnet`
	- `RItools`
	- `pbs`
  - Required packages to display results (paper, analysis documents):
    - `knitr`
    - `kableExtra`
 	- `ggplot`
	- `gridExtra`
  - Optional packages:
    - `parallel` to run simulations in parallel
    - `rmarkdown` to make analysis documents more easily
2. In `R`, Run data analysis for Maria Study:
    - Dataset available from private github repo https://github.com/alexisrsantos/Deaths_Puerto_Rico_JAMA
    - Store data as `extdata/death_file_JAMA.csv`
    - Then run to create analysis file `maria.md`
```
library(knitr)
knit('maria.Rmd')
```
3. In `R`, Run data analysis for AP Study: (this automatically downloads publicly available data)
```
knit('dataAnalysis.Rmd')
```
  - Creates files:
    - `dataAnalysis.md` analysis document
    - `RDanalysis-[M][D][Time].RData` analysis output
    - `tab-results.tex` `tab-alt.tex` tables 1 & 2 for paper
4. In `R` Run simulation studies
```
nrep <- 5000 ## number of simulation replications
knit('fullOutcomeSim.Rmd')
```
   - Substitute `nrep <- X` for `X` simulation replications
   - `nrep <- 0` loads previously-run simulation files (files
     corresponding to results in the paper are in the repo)
   - Creates files:
     - `fullOutcomeSim.md` analysis document
	 - `outcomeSim-YYYY-MM-DD.RData` simulation results for table 3
     - `totalPolySim-YYYY-MM-DD.RData` simulation results for table 4
	 - `tab-levelSimulation.tex` latex for table 3
	 - `tab-polynomialSimulation.tex` latex for table 4
	 - some other random files.
 

