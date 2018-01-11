[![Travis-CI Build Status](https://travis-ci.org/dtkaplan/checkr.svg?branch=master)](https://travis-ci.org/dtkaplan/checkr)

# checkr: Checking student answers in an R-tutorial system

An R-tutorial system provides facilities for posing R-related questions to students, collecting the code that students write in reply, and providing hints as needed. Examples for R-tutorial systems are [DataCamp.com](DataCamp.com) and the [learnr](https://rstudio.github.io/learnr/) package from RStudio.

The `checkr` package supports a framework for the authors of tutorials to specify what is a correct answer, and to give formative feedback when answers fail to meet that specification. Functions are provided to enable `checkr` to be connected to `learnr` documents. 

You can install the latest version of this package from GitHub

```r
devtools::install_github("dtkaplan/checkr")
```

See the package vignette for details and examples about using `checkr`.
