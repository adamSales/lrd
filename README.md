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
install.packages(c("rdd", "robustbase", "withr", "foreign",  "sandwich"), lib.loc=TEMPDIR)
```
or
```{r}
install.packages(c("rdd", "robustbase", "withr", "foreign",  "sandwich"),
       repo=URL_OF_FAVORITE_CRAN_MIRROR, lib.loc=TEMPDIR)
```
according to your preference.  Then install the `lrd` package via

```{r}
install.packages("lrd_0.0.0.9000.tar.gz", repos = NULL,
                          lib.loc=TEMPDIR, type = "source")
```

# Replicating computations in paper

Once installed, you can load the package via

```{r}
library("lrd", lib.loc=TEMPDIR)
```

## Data analysis

With the lrd package loaded, you can locate our data analysis file using
`system.file("dataAnalysis.R", package="lrd")`.  Run it via

```{r}
rmarkdown::render( system.file("dataAnalysis.Rmd", package="lrd") )
```

(You'll need to have the rmarkdown package installed, of course.)

## Power/size simulations



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




# miscellaneous notes

## soft links to material outside this repo

- Directory for data we're not confident we're allowed to repackage: `dataResults/`
- bibtex bibliography file: `bibliography.bib`  (points to drafts`/causalinference.bib`
  in AS/BH shared dropbox folder. )
