
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mltzostercode

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/UBESP-DCTV/mltzostercode?branch=master&svg=true)](https://ci.appveyor.com/project/UBESP-DCTV/mltzostercode)
[![Travis build
status](https://travis-ci.org/UBESP-DCTV/mltzostercode.svg?branch=master)](https://travis-ci.org/UBESP-DCTV/mltzostercode)
<!-- badges: end -->

The goal of mltzostercode is to Collect the function and the code used
for the analyses reported in the paper “Use of Machine Learning
techniques for case-detection of Varicella Zoster using routinely
collected textual ambulatory records”

## Installation

To access to all the function provided and their documentation, you can
install the development version of the packagefrom
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UBESP-DCTV/mltzostercode")
```

## Analyses

The analyses for the paper are reported in `inst/analysis/analysis.R`.

> NOTE: to be sure you have installed all the needed package used in the
> script `inst/analysis/analysis.R` too, you can install the package
> with the option `dependencies = TRUE`.

> NOTE 2: To run the script `inst/analysis/analysis.R`, in its part
> involving the *Maximum Entropy* analyses, you need the **maxent**
> package, which has been removed from the CRAN. Hence, if you want to
> run `inst/analysis/analysis.R`, you need to build **maxent** from
> source by your own. You can find the archived versions
> [here](https://cran.r-project.org/src/contrib/Archive/maxent/).

## Code of Conduct

Please note that the ‘mltzostercode’ project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.
