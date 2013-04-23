\name{specimen}
\alias{specimen}
\docType{data}
\title{
Sample input for MH
}
\description{
Non-informative data that will roughly return the prior.
}
\usage{data(specimen)}
\format{
List of six:
\describe{
\item{poster}{RAFM output, dimension: population x population x iteration}
\item{ped}{Pedigree matrix}
\item{covars}{Covariate matrix}
\item{traits}{Traits of interest, matrix}
\item{blocks}{Study design blocks, matrix}
\item{prior}{List of prior hyper-parameters}
}
}

\details{ 
The component ped is the pedigree matrix. Individuals who have a number in the fourth column are interpreted as founders catched from the wild. Other individuals are interpreted as offspring (P1, P2 etc.). An individual should always appear before its offspring. The order of individuals should be the same in all data matrices (ped, covars, traits and blocks). IDs refer directly to sire and dam codes in ped. Selfing is allowed, so use separate codes for sires and dams, if it's impossible for your data.

The component prior consists of lists. First component is the mean and variance of the fixed and population effects. You can replace the covariance matrix by NA for a theoretically justified model. The components $G and $E give the degrees of freedom and the inverse scale matrix of the respective priors. 

Component $random is a list such that one component is a similar parameter list as is used for G and E matrices. In this case, length(specimen$prior$random) is 1, because there is only one random effect, that of design blocks. If you use more columns (and custom priors), you need a longer list. If you use your own priors, it is your responsibility to check that they are conformable with the study design. }

\examples{
# How it looks:
data(specimen)
head(specimen$ped)
head(specimen$traits)
n = nrow(specimen$ped)

# Including another 'block effect':
specimen$blocks <- cbind(specimen$blocks, sample(1:2, n, replace=TRUE))

# Correcting prior accordingly:
specimen$priors$random <- list(specimen$priors$random, specimen$priors$random)
specimen$priors$random

# Default model:
specimen$priors$fixed[[2]] = NA
}
\keyword{datasets}
