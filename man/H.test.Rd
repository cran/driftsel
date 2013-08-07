\name{H.test}
\alias{H.test}

\title{
Neutrality test for the posteriors obtained from MH
}
\description{
This function calculates the test statistic H (Karhunen et al. 2013b).
}
\usage{
H.test(popefpost, Gpost, THpost, env, silent = T)
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
  \item{env}{
matrix of environmental data, rows=subpopulations, columns=variables
}
  \item{silent}{
plot convergence checks?
}
}

\details{
H.test works in a way much analogous to S.test. S.test asks, whether the population means are too near or far to the ancestral mean. H.test asks, whether the population means correlate with the environmental data more than would be expected on basis of shared evolutionary history. E.g. all polar bears are similar and different to brown bears, both because of shared climate and shared phylogeny. Does shared phylongey explain this all, or are the similarities suspiciously similar to similarities in let's say latitude?

Large values of H imply that the populations are more adapted to their environment than would be expected at random, i.e. on basis of random drift. E.g. H>0.95 implies adaptation to the covariates in env with p value 0.05. (Small values of H are difficult to interpret; probably they mean that the covariates in env are just irrelevant regarding evolutionary adaptation.)
}
\value{
The test statistic H, defined as the posterior probability that the observed product moment of environmental and phenotypic distance is greater than a similar product moment produced under the neutral model, i.e. in absence of natural selection.
}
\references{
Karhunen et al. (2013b): Evolution of quantitative traits by natural selection: a case study of nine-spined sticklebacks, manuscript.
}

\examples{

# Analayzing specimen data by MCMC:
data(specimen)
A <- specimen
samp <- MH(A$poster, A$ped, A$covars, A$traits, 21, 7, 2) # a very small posterior

# Let's make some environmental data!
dim(A$poster) # how many populations?
pops = 1:2
rain = runif(2, 0, 1000)
temp = runif(2, 0, 25)
wind = runif(2, 0, 360)
env = cbind(pops, rain, temp, wind)

# Let's calculate the test:
H <- H.test(samp$pop.ef, samp$G, samp$theta, env, silent=TRUE)
}
