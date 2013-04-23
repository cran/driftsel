\name{viz.traits}
\alias{viz.traits}

\title{
Population means in trait-trait space
}
\description{
This is for visualizing the pattern of phenotypic differentiation.
}
\usage{
viz.traits(fixedpost, popefpost, Gpost, THpost, traits, main=NA)
}

\arguments{
  \item{fixedpost}{
posterior of fixed effects from function MH
}
  \item{popefpost}{
posterior of population effects
}
  \item{Gpost}{
posterior of G matrix
}
  \item{THpost}{
posterior of theta
}
  \item{traits}{
which two traits to plot, integer vector
}
  \item{main}{
an overall title
}

}
\details{
The numbers represent population-level additive genetic effects, 'population means'. Each ellipse represents the median drift distance for the population of same color. (The ellipses have different sizes, because the local populations have different intra-population thetas, and thus experience different amounts of genetic random drift.) The ellipses are centered around the ancestral mean of the trait pair. All parameters refer to posterior means; estimation uncertainty is not presented in this figure.
}
\value{
Does not return anything.
}

\examples{
data(specimen)
A <- specimen
samp <- MH(A$poster, A$ped, A$covars, A$traits, 99, 66, 3)
viz.traits(samp$fixed.ef, samp$pop.ef, samp$G, samp$theta, 1:2)
}
