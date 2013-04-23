\name{driftsel}
\alias{driftsel-package}
\alias{driftsel}
\docType{package}
\title{
Drift and selection in natural populations
}
\description{ This package implements the neutrality test described in the Genetics paper of Ovaskainen et al. (2011), and explained in more detail by Karhunen et al. (2013). Shortly, it fits a Bayesian animal model and compares it with a measure of neutral genetic differentiation. (This measure can be obtained by using the R package RAFM.) Alternatively, driftsel can also be used to estimate G matrices and effects of covariates on phenotypes in a panmictic population. The package includes the estimation method MH, the neutrality test, visualization aids and example datasets.
}
\details{
\tabular{ll}{
Package: \tab driftsel\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-10-02\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
Use package RAFM to analyze the neutral DNA. Subsequently, use driftsel to analyze the breeding design and to do the neutrality test. Note that the estimation function MH gives much information besides the neutrality test, e.g. the effects of fixed and random covariates on the traits of interest.
}
\author{
Markku Karhunen, <markku.karhunen@helsinki.fi>
}
\references{
Ovaskainen et al. (2011): A new method to uncover signatures of divergent and stabilizing selection in quantitative traits. Genetics 189: 621-632.

Karhunen et al. (2013): driftsel: an R package for detecting signals of natural selection in quantitative traits. Molecular Ecology Resources XX: pp-pp.
}
\examples{
data(specimen)
A <- specimen
samp <- MH(A$poster, A$ped, A$covars, A$traits, 12, 4, 1)
neut.test(samp$pop.ef, samp$G, samp$theta, silent=TRUE)
}
