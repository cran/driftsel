\name{threespines}
\alias{threespines}
\docType{data}
\title{
Data from threespine sticklebacks
}
\description{
Threespine sticklebacks from Lake Pulmanki, Lake Vattern and Baltic Sea.
}
\usage{data(threespines)}
\format{
List of six:
\describe{
\item{poster}{RAFM output, dimension: population x population x iteration}
\item{ped}{Pedigree matrix as in specimen2}
\item{covars}{Covariate matrix, only IDs}
\item{traits}{Four morphological traits}
\item{blocks}{Study design blocks, matrix}
\item{prior}{Empty list, amounts to default priors}
}
}
\details{
These data were used as an example by Karhunen et al. (2013). 
}
\source{
Data from Ecological Genetics Research Unit, University of Helsinki.
}
\references{
Karhunen et al. (2013): driftsel: an R package for detecting signals of natural selection in quantitative traits. Molecular Ecology Resources XX: pp-pp.

See also Makinen et al. (2006): Genetic relationships among marine and freshwater populations of the European three-spined stickleback (Gasterosteus aculeatus) revealed by microsatellites. Molecular Ecology 15: 1519-1534.
}
\examples{
data(threespines)
hist(threespines$traits[,2])
}
\keyword{datasets}
