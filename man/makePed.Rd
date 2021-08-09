\name{makePed}
\alias{makePed}
\title{
A function to add founders and sort pedigrees
}
\description{
Raw pedigrees are typically not sorted chronologically and may be missing founders. Using the supplied pedigree, this function adds missing founders and then sorts the pedigree to ensure parents always come before offspring.
}
\usage{
makePed(pedigree)
}
\arguments{
  \item{pedigree}{
A dataframe where the first three columns correspond to ID, Sire, and Dam. Unknown parents should be given a value of zero. This function will help to add missing founders with sire and dam labeled as zero. It then sorts the pedigree to ensure that parents come before their offspring.
}
}

\value{
When this function is needed, the user can load it as \code{Ped1 = makePed(pedigree)}, the dataframe \code{Ped1} will contain a new sorted and renumbered pedigree with the parents ordered before their offspring
\item{ID }{sorted animals in the pedigree}
\item{Sire }{Sorted sires in the pedigree }
\item{Dam}{Sorted dams in the pedigree }
%% ...
}
\references{
Gutierrez J.P., Goyache F. (2005): A note on ENDOG: a computer program for analysing pedigree information. Journal of Animal Breeding and Genetics, 122, 172-176.

}
\author{
Victor Oribamise and Ernest Osanyinlokun
}
\note{
For issues or problems with this function, please contact Victor Oribamise at \email{victor.oribamise@ndsu.edu}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
pedigree = data.frame(ID = c(1,2,3,4,5,6,7,8), Sire = c(0,0,1,1,3,3,5,5), Dam = c(0,0,0,2,4,4,6,6))
Ped1 = makePed(pedigree)
}
