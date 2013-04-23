\name{MH}
\alias{MH}

\title{
A Metropolis-Hastings algorithm for quantitative genetics
}
\description{
This function fits the animal model described by Ovaskainen et al. while sampling from the posterior of population-population coancestry coefficients.
}
\usage{
MH(poster, ped, covars, traits, nmc, burnin, thin,
blocks = NA, priors = NA, tmp = NA, alt = F)
}

\arguments{
  \item{poster}{
posterior of AFM, a 3-dimensional array, see Details
}
  \item{ped}{
pedigree matrix
}
  \item{covars}{
covariates modelled as fixed effects, matrix
}
  \item{traits}{
traits of interest, matrix
}
  \item{nmc}{
total number of MC iterations
}
  \item{burnin}{
length of transient period
}
  \item{thin}{
thinning interval
}
  \item{blocks}{
covariates modelled as random effects, optional
}
  \item{priors}{
list of prior hyper-parameters, optional
}
  \item{tmp}{
tmpmax for sparse Cholesky decomposition, optional
}
  \item{alt}{
is pedigree in alternative short format (as in specimen2)?
}
}
\details{
Use the R package RAFM to analyze molecular markers, calculate posterior of theta, and use it as argument poster in this function. Use MH to analyze the phenotypic data. Please see specimen and specimen2 for formatting your data for use with this package. The observations have to be in the same order for every argument of MH. Blocks do not need to be coded as dummys (whereas categorical variables in covars need to be). Note that the arguments of MH are matrices or arrays.

If you do not wish to give any covariates, assign covars <- traits[,1] (which is ID). In this case, each trait is modelled by a constant term. In fact, a constant term is always used regardless of the covariates, corresponding to traits~covars+1 in standard regression models. It is the first 'fixed effect' in the output.

As regarding the priors, format the list according to specimen$priors. You can also use \cr priors$fixed[[2]]=NA, unlike in specimen. This produces the model described by Ovaskainen et al., whereas typing a covariance matrix removes the link between the population means and the ancestral G matrix, potentially confusing the neutrality test. Use priors=NA, if you don't know what to do. The default prirors are quite uninformative.  

Try different values of argument tmp, if the program crashes on your data with 'Error: ... increase tmpmax'. High values of tmp should decrease the degree of sparseness in Cholesky decomposition, and allow you to run your data at the cost of increased computation time. If this error doesn't occur, don't touch tmp.

Regarding the output, posteriors of all parameters are given as three-dimensional arrays, last dimension referring to iteration. Output includes fixed effects, population means, ancestral G matrix, E matrix and theta matrix. Theta is the population-level coancestry matrix. Its posterior sample is a subset of the values given in poster, filttered according to their likelihood in the phenotypic data.

Remember to check the convergence of the Markov chains in the posterior sample of parameters of interest. Increase nmc, burnin and/or thin, if convergence is poor.

}
\value{
\item{fixed.ef}{Posterior of fixed effects, dimension: covariate x trait x iteration}
\item{pop.ef}{Posterior of subpopulation effects, dimension: population x trait x iteration}
\item{G}{Posterior of G matrix, dimension: trait x trait x iteration}
\item{E}{Posterior of E matrix, dimension: trait x trait x iteration}
\item{theta}{Posterior of theta matrix, dimension: population x population x iteration}
}
\references{
Ovaskainen et al. (2011): A new method to uncover signatures of divergent and stabilizing selection in quantitative traits. Genetics 189: 621-632.
}

\examples{

# Basic usage:
data(specimen)
A <- specimen
samp <- MH(A$poster, A$ped, A$covars, A$traits, 2, 1, 1) # uninformative priors

# Using informative priors:
samp <- MH(A$poster, A$ped, A$covars, A$traits, 2, 1, 1, priors=A$priors)

# Modelling study-design blocks as random effects:
samp <- MH(A$poster, A$ped, A$covars, A$traits, 2, 1, 1, blocks=A$blocks)
names(samp)
mains <- samp$main.result # DO THIS, IF YOU USE RANDOM EFFECTS
mains$G
plot(mains$pop.ef[1,1,], type='l', xlab="iter", ylab="", main="") # one Markov chain
plot(mains$G[1,2,], type='l', xlab="iter", ylab="", main="") # another Markov chain
}
