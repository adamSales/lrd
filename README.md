# Analysis replication materials

December 2017

Accompanies the paper "Limitless Regression Discontinuity".

## Setup

The repository is bundled as an R package.  You'll need a working,
recent version of R (see DESCRIPTION) to use it.  To install it in its
own directory, you should

1.  Select/create a directory that will serve as a dedicated package
    library for this replication.
2.  Install/confirm installation of ordinary R packages that this one requires 
3.  Install this package

Referring to the directory set up in step 1 as TEMPDIR, install the
dependency packages using either

```{r}
install.packages(c("rdd", "robustbase", "withr", "foreign",  "sandwich"), lib=TEMPDIR)
```
or
```{r}
install.packages(c("rdd", "robustbase", "withr", "foreign",  "sandwich"),
       repo=URL_OF_FAVORITE_CRAN_MIRROR, lib=TEMPDIR)
```
according to your preference.  Then install the `lrd` package via

```{r}
install.packages("lrd_0.0.2.9000.tar.gz", repos = NULL,
                          lib=TEMPDIR, type = "source")
```

# Replicating computations in paper

Once installed, you can load the package via

```{r}
library("lrd", lib.loc=TEMPDIR)
```

## Data analysis

Create a subdirectory called `dataResults` in your working directory.
The script will store analysis artifacts here.

With the lrd package loaded, you can locate our data analysis file using
`system.file("dataAnalysis.R", package="lrd")`.  Run it via e.g.

```{r}
knitr::knit( system.file("dataAnalysis.Rmd", package="lrd") )
```

which requires that you have the knitr package installed. 

## Power/size simulations

The simulations take a while.  Results from our run are bundled with
the lrd package -- see "`fullOutcomeSim*.md`" and
"`fullOutcomeSim*.pdf`" files in the package root directory.


The fullOutcomeSim.Rmd script displays and optionally re-runs power and
size simulations presented in the paper.  With the lrd package loaded, you can locate it using
`system.file("fullOutcomeSim.Rmd", package="lrd")`.

To run it, first create a subdirectory called `dataResults` in your working directory.
The script will store simulation artifacts here.

To reproduce the simulation results from the command line, do 
>   Rscript -e 'nreps={N}; library("lrd", lib.loc=TEMPDIR); knitr::knit(system.file("fullOutcomeSim.Rmd", package="lrd"))'

after replacing "`{N}`" with the number of simulations you want.  (We used 5000.)  

Once they've been generated one can do

>   Rscript -e 'library("lrd", lib.loc=TEMPDIR); knitr::knit(system.file("fullOutcomeSim.Rmd", package="lrd"))'

to generate an html file with code and results from whatever
simulations may be saved in "dataResults/simResults.RData".




