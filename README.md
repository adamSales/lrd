# limitlessRD
explorations of distribution-free large sample inference for regression discontinuity designs

Accompanies the paper "Limitless regression discontinuity".

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
install.packages("lrd_0.0.0.9000.tar.gz", repos = NULL,
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
rmarkdown::render( system.file("dataAnalysis.Rmd", package="lrd") )
```

which requires that you have the rmarkdown package installed.  (If you
can't install it, try installing the knitr package instead. The
parallel command with `knitr::knit` in place of `rmarkdown::render`
should work, creating a markdown file instead of a PDF.)

## Power/size simulations

Create a subdirectory called `dataResults` in your working directory.
The script will store simulation artifacts here.

The fullOutcomeSim.Rmd script displays and optionally re-runs power and
size simulations presented in the paper.  With the lrd package loaded, you can locate it using
`system.file("fullOutcomeSim.Rmd", package="lrd")`.


To run it from the command line, do 

>   Rscript -e 'withr::with_libpaths(TEMPDIR, library("lrd"),  "prefix"); rmarkdown::render(system.file("fullOutcomeSim.Rmd", package="lrd"))'

or
>   Rscript -e 'withr::with_libpaths(TEMPDIR, library("lrd"),  "prefix"); knitr::knit(system.file("fullOutcomeSim.Rmd", package="lrd"))'

This will generate an html file with code and results from whatever
simulations may be saved in "data/simResults.RData".

To not only display the simulation results but also reproduce them, do 
>   Rscript -e 'nreps={N}; withr::with_libpaths(TEMPDIR, library("lrd"),  "prefix"); rmarkdown::render(system.file("fullOutcomeSim.Rmd", package="lrd"))'

after replacing "`{N}`" with the number of simulations you want, e.g 1000.  




