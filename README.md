# limitlessRD
explorations of distribution-free large sample inference for regression discontinuity designs

Accompanies the paper [Limitless regression discontinuity](http://arxiv.org/abs/1403.5478).


## Power/size simulations

The power and size simulations presented in the August 2016 version of the paper were imported into this repo as part of R/functions.r.  Maybe this will be reorganized a little as we go.

Scripts for simulations themselves live in ./inst/simstudy (or perhaps ./simstudy if you unpacked a tarball to get here)
To run a script from the command line, do

>     Rscript -e 'nreps=<N>; setwd("./inst/simstudy/"); rmarkdown::render("check-sh-deps.Rmd")'

or 

>     Rscript -e 'nreps=<N>; setwd("./inst/simstudy/"); library("knitr"); knit("check-sh-deps.Rmd")'

after replacing `<N>` with a simulation size number, e.g 1000.  


# miscellaneous notes

## soft links to material outside this repo

- Directory for data we're not confident we're allowed to repackage: `dataResults/`
- bibtex bibliography file: `bibliography.bib`  (points to drafts`/causalinference.bib`
  in AS/BH shared dropbox folder. )
