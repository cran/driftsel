\name{viz.theta}
\alias{viz.theta}

\title{
Illustrating neutral genetic differentiation
}
\description{
This function illustrates the posterior of coancestry matrix theta as a planar graph.
}
\usage{
viz.theta(thetapost, distance = T, center = F, main = NA)
}

\arguments{
  \item{thetapost}{
posterior of theta
}
  \item{distance}{
one of TRUE, FALSE or NA, see below
}
  \item{center}{
plot ancestral mean at origin?
}
  \item{main}{
overall title, optional
}

}
\details{
This function calculates the expected (absolute value of) difference in population means for each pair of populations under random genetic drift. Measuring the distance in ancestral standard deviation implies that this depends only on the coancestry matrix. This calculation produces a distance matrix. We use multidimensional scaling to present the distance matrix in a plane.

Using distance=T types the expected differences on the edges of the graph, while distance=F types the coancestry coefficients which depend inversely on the distances. distance=NA omits the edges. 

You can use center=T to increase readability of the graph. This puts the ancestral population to origin, even though this is not in line with MDS as such.

Note that you can use viz.theta for both the posterior of theta obtained from RAFM and one obtained from driftsel::MH. The graph may change, because the quantitative traits also convoy some information regarding the pattern of coancestry.
}
\value{
Does not return anything.
}

\examples{
data(specimen)
A <- specimen
samp <- MH(A$poster, A$ped, A$covars, A$traits, 99, 66, 3)
viz.theta(A$poster) # based only on molecular markers
viz.theta(samp$theta) # based on molecular markers & phenotypes
}
