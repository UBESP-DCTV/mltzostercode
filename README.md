
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mltzostercode

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/UBESP-DCTV/mltzostercode?branch=master&svg=true)](https://ci.appveyor.com/project/CorradoLanera/mltzostercode)
[![Travis build
status](https://travis-ci.org/UBESP-DCTV/mltzostercode.svg?branch=master)](https://travis-ci.org/UBESP-DCTV/mltzostercode)
<!-- badges: end -->

The goal of mltzostercode is to collect the function and the code used
for the analysis reported in the paper “Use of Machine Learning
techniques for case-detection of Varicella Zoster using routinely
collected textual ambulatory records”

## Installation

To access to all the functions provided and their documentations, you
can install the development version of the package from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UBESP-DCTV/mltzostercode")
```

## Analyses

The analysis for the paper are not a part of the package core. Anyway,
they are provided in the `inst/analysis/analysis.R` script. You can find
it in that folder here on GitHub, or, once the package will be installed
on your system, you can find it under the folder
`system.file("analysis/analysis.R", package = "mltzostercode")`.

> **NOTE**: The script for the analysis reported in the paper is
> included in the package to show the exact procedure followed only. We
> cannot provide the data used for the analysis. Hence, at the end of
> the day, it cannot run by itself.

> **NOTE 2**: To be sure you have installed all the needed package to
> run the script `inst/analysis/analysis.R` (maybe on your data), you
> may install the package with the option `dependencies = TRUE`. On the
> contrary, the installation procedure will guarantee that all the
> functions from the `mltzostercode` package will run, but you could
> miss some dependencies to run the code reported in
> `inst/analysis/analysis.R`.

> **NOTE 3**: To run the whole script `inst/analysis/analysis.R`, in its
> part involving the *Maximum Entropy* analyses too, you need the
> **maxent** package, which has been removed from the CRAN recently.
> Hence, if you want to run all the `inst/analysis/analysis.R` (and in
> your system is not already present the **maxent** package v.1.3.3.1)
> you need to build the **maxent** package version 1.3.3.1 from source
> by your own. You can find the archived version
> [here](https://cran.r-project.org/src/contrib/Archive/maxent/).

## Code of Conduct

Please note that the ‘mltzostercode’ project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.
