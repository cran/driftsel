\name{specimen2}
\alias{specimen2}
\docType{data}
\title{
Sample input for MH, short format
}
\description{
Short format for one-generation study designs.
}
\usage{data(specimen2)}
\format{
List of six:
\describe{
\item{poster}{RAFM output, dimension: population x population x iteration}
\item{ped}{Pedigree matrix, sires and dams do not appear}
\item{covars}{Covariate matrix}
\item{traits}{Traits of interest, matrix}
\item{blocks}{Study design blocks, matrix}
\item{prior}{List of prior hyper-parameters}
}
}

\details{
You can use this data format for one-generation study designs, with option alt=TRUE in MH.
 
In these data, the component ped is a pedigree matrix with five columns: ID, sire, dam, sire's population and dam's population. This way you avoid typing extra rows for F generation, and it is also slightly faster to run than specimen$ped. The order of individuals should be the same in all data matrices (ped, covars, traits and blocks). Same individuals are used in specimen and specimen2, but there's more information in specimen, because it contains observations from F generation.

The component prior consists of lists. First component is the mean and variance of the fixed and population effects. You can replace the covariance matrix by NA for a theoretically justified model. The components $G and $E give the degrees of freedom and the inverse scale matrix of the respective priors. 

Component $random is a list such that one component is a similar parameter list as is used for G and E matrices. In this case, length(specimen$prior$random) is 1, because there is only on random effect, that of design blocks. If you use more columns (and custom priors), you need a longer list. If you use your own priors, it is your responsibility to check that they are conformable with the study design. }

\examples{
# How it looks:
data(specimen2)
head(specimen2$ped)
head(specimen2$traits)
n = nrow(specimen2$ped)

# Running MH:
A <- specimen2
samp <- MH(A$poster, A$ped, A$covars, A$traits, 21, 7, 2, alt=TRUE)
}
\keyword{datasets}
