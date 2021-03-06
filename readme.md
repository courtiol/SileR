
# SileR

This is the GitHub repository hosting the R package SileR. The package
will not evolve much in the future because it aims at documenting how we
obtained the results for the paper entitled “Differences in age-specific
mortality between wild-caught and captive-born Asian elephants” by
Mirkka Lahdenperä, Khyne U Mar, Alexandre Courtiol and Virpi Lummaa
([Nature Communications 2018](https://rdcu.be/39Sp)).

The only planned updates are those that will be necessary to maintain
compatibility with other packages, so that our code won’t break.

The main goal of the R package SileR is to fit survival models derived
from a model introduced by Siler (“A competiting-risk model for animal
mortality”, Ecology, 1979). The main differences are that the effect of
covariate(s) are being investigated and that the survival models have to
be fitted on right and left censored longitudinal data.

This package has not been conceived for general use. It serves to
document the steps of our analysis.

## How to explore the sources of the package?

To see our source code, you can either browse the files above within
GitHub or download the \*.tar.gz file containing all sources bundled
with some data to try out our functions in R. This file is available
here:
<https://github.com/courtiol/drat/blob/gh-pages/src/contrib/SileR_0.9.0.tar.gz>

## Installation

Since the package contains C++ code that needs to be compiled, there are
two possibilities for installation. The easiest one is to install the
binary version of our package. We have created one for Windows and one
for MacOS but that may fail depending on your exact infrastructure. The
alternative is to compile the sources of the package on your own
computer but that may require you to install some development tools.

### General installation requirements

Whichever option you choose, to install successfully the R package you
will need:

  - an up-to-date installation of R (<https://cran.r-project.org>);

  - the R package `drat` installed. You can install it by typing:

<!-- end list -->

``` r
install.packages("drat")
```

### Installation from binaries

Simply type the following:

``` r
drat::addRepo("courtiol")
install.packages("SileR")
```

### Installation from sources

This option requires the tools allowing to compile R packages on your
computer:

  - if you are using Linux, you should already have them;

  - if you are using MacOS, you need to install Xcode (available from
    <https://developer.apple.com/xcode/>) if you have not done it;

  - if you are using Windows, you need to install Rtools (available from
    <https://cran.r-project.org/bin/windows/Rtools/>) if you have not
    done it.

Then, simply type the following:

``` r
drat::addRepo("courtiol")
install.packages("SileR", type = "source")
```

## Usage

1.  load the package:

<!-- end list -->

``` r
library("SileR")
```

2.  access the main documentation by typing:

<!-- end list -->

``` r
help("SileR")
```
