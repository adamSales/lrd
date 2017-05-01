# limitlessRD
explorations of distribution-free large sample inference for regression discontinuity designs

Accompanies the paper [Limitless regression discontinuity](http://arxiv.org/abs/1403.5478).


## Power/size simulations


Simulation scripts live in ./inst/simstudy (or perhaps ./simstudy if
you unpacked a tarball to get here)

The fullOutcomeSim.Rmd script displays and optionally re-runs power and
size simulations presented in the paper. 

To run it from the command line, do 

>   Rscript -e 'setwd("./inst/simstudy/"); rmarkdown::render("fullOutcomeSim.Rmd")'

This will generate an html file with code and results from whatever
simulations are saved in "data/simResults.RData".

To not only display the simulation results but also reproduce them, do 
>   Rscript -e 'nreps=<N>; setwd("./inst/simstudy/"); rmarkdown::render("fullOutcomeSim.Rmd")'

after replacing "`<N>`" with the number of simulations you want, e.g 1000.  




