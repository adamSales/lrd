# limitlessRD
explorations of distribution-free large sample inference for regression discontinuity designs

Accompanies the paper [Limitless regression discontinuity](http://arxiv.org/abs/1403.5478).

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
install.packages(???, repo=URL_OF_FAVORITE_CRAN_MIRROR)
```
or
```{r}
install.packages(???, repo=URL_OF_FAVORITE_CRAN_MIRROR, lib.loc=TEMPDIR)
```
according to your preference.  Then install the `lrd` package via

```{r}
install.packages(LRD_TARBALL , lib.loc=TEMPDIR)
```

# Replicating computations in paper

## Data analysis

See file inst/dataAnalysis.R (or perhaps dataAnalysis.R if you
unpacked a tarball to get here).   Commands to be run from main
directory/package root directory.

## Power/size simulations


Simulation scripts live in ./inst (or perhaps ./ if
you unpacked a tarball to get here)

The fullOutcomeSim.Rmd script displays and optionally re-runs power and
size simulations presented in the paper. 

To run it from the command line, do 

>   Rscript -e 'setwd("./inst/"); rmarkdown::render("fullOutcomeSim.Rmd")'
or
>   Rscript -e 'setwd("./inst/"); knitr::knit("fullOutcomeSim.Rmd")'

This will generate an html file with code and results from whatever
simulations are saved in "data/simResults.RData".

To not only display the simulation results but also reproduce them, do 
>   Rscript -e 'nreps=<N>; setwd("./inst/"); rmarkdown::render("fullOutcomeSim.Rmd")'

after replacing "`<N>`" with the number of simulations you want, e.g 1000.  




