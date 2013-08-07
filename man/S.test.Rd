\name{S.test}
\alias{S.test}

\title{
Neutrality test for the posteriors obtained from MH
}
\description{
This function calculates the test statistic S (see Ovaskainen et al. 2011 and Karhunen et al. 2013).
}
\usage{
S.test(popefpost, Gpost, THpost, silent = T, G.off = F,
th.off = F, main = NA)
}

\arguments{
  \item{popefpost}{
posterior of population effects from function MH
}
  \item{Gpost}{
posterior of G matrix
}
  \item{THpost}{
posterior of theta
}
  \item{silent}{
plot the trace of the test statistic?
}
  \item{G.off}{
ignore genetic correlations?
}
  \item{th.off}{
ignore signatures of gene flow?
}
  \item{main}{
title for the diagnostic plot, optional
}
}
\details{
popefpost, Gpost and THpost should be $pop.ef, $G and $theta from the output of MH. You can use silent=FALSE to investigate convergence in the neutrality test. Note that you don't necessarily want to see white noise on [0,1], but trendless noise in some subset of [0,1].
}
\value{
The test statistic S. It measures how far the multivariate population means have drifted from the ancestral mean. S=0.5 implies perfect match with neutrality. S=0 and S=1 imply perfect match with stabilizing and diversifying selection, respectively. The test is known to be conservative, so that S=0.99 implies diversifying selection at least on the significance level of 0.99.
}
\references{
Ovaskainen et al. (2011): A new method to uncover signatures of divergent and stabilizing selection in quantitative traits. Genetics 189: 621-632.

Karhunen et al. (2013): driftsel: an R package for detecting signals of natural selection in quantitative traits. Molecular Ecology Resources XX: pp-pp.
}

\examples{
data(specimen)
A <- specimen
samp <- MH(A$poster, A$ped, A$covars, A$traits, 99, 66, 3) # a small posterior
S <- S.test(samp$pop.ef, samp$G, samp$theta) # neutrality test
S <- S.test(samp$pop.ef, samp$G, samp$theta, G.off=TRUE) # does the value change?
}
