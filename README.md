# lrd
Replication material for "Limitless Regression Discontinuity"

To reproduce the results in the paper:

1. Make sure R is up-to-date with required packages installed:
  - We used R version 3.4.4
  - Required packages to carry out analyses:
	- `robustbase`
	- `rdd`
	- `sadnwich`
	- `nnet`
	- `RItools`
  - Required packages to display results (paper, analysis documents):
    - `knitr`
    - `kableExtra`
 	- `ggplot`
	- `gridExtra`
  - Optional packages:
    - `parallel` to run simulations in parallel
	- `rmarkdown` to make analysis documents more easily
2. In `R`, Run data analysis:
```
library(knitr)
knit('dataAnalysis.Rmd')
```
  - Creates files:
    - `dataAnalysis.md` analysis document
	- `RDanalysis-[M][D][Time].RData` analysis output
	- `tab-results.tex` `tab-alt.tex` tables 1 & 2 for paper
3. In `R` Run simulation studies
```
nrep <- 5000 ## number of simulation replications
knit('fullOutcomeSim.Rmd')
```
   - Substitute `nrep <- X` for `X` simulation replications
   - `nrep <- 0` loads previously-run simulation
   - Creates files:
     - `fullOutcomeSim.md` analysis document
	 - `outcomeSim-YYYY-MM-DD.RData` simulation results for table 3
     - `totalPolySim-YYYY-MM-DD.RData` simulation results for table 4
	 - `tab-levelSimulation.tex` latex for table 3
	 - `tab-polynomialSimulation.tex` latex for table 4
	 - some other random files.
 4. In `R` compile LaTeX for the manuscript:
 ```
 knit('lrd-r1.Rnw')
 ```
 5. In the console, compile the pdf:
 ```
 pdflatex lrd-r1.tex
 bibtex lrd-r1
 pdflatex lrd-r1.tex
 pdflatex lrd-r1.tex
 ```

