\name{SibCount}
\alias{SibCount}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to count and provide statistics on full-siblings, paternal and maternal half-siblings based on pedigree.
}
\description{
\code{SibCount} allows users to supply pedigree of any size to determine sibling structure within the available pedigree for full-siblings, paternal and maternal half-siblings. The function returns basic statistics on each type that includes number of that relative sibling type in the pedigree, as well as the average number, standard deviation, minimum and maximum of that sibling type per "family". In this case "family" is dependent on the type of relationship being evaluated. For full-siblings, family refers to having the same sire and dam, whereas family would refer to having the same sire or dam for paternal or maternal half-siblings, respectively. If savefiles is true, then additional detail on what defined the family for each sibling type is outputed as text files.
}
\usage{
SibCount(Ped,savefiles=TRUE,destfile)
}
\arguments{
  \item{Ped}{
A pedigree of the population being evaluated. Format should be a dataframe either read in using \code{read.table} or similar functions or created in R as a dataframe (e.g., \code{data.frame}). The dataframe should consist of an individual ID, Sire ID, and Dam ID columns. Unknown parents should be designated as 0 or NA. Only Sire and Dam columns are used in this function, but other functions within the Sibs package may require the ID column and is therefore recommended to have all 3.
}
  \item{savefiles}{
Default is true. This term allows you to save .... from the function only if set to TRUE.
Options: Must be either TRUE or FALSE.
}
  \item{destfile}{
This is the pathway to the computer location in which files will be saved and must be specified using quotation marks (e.g., \code{"C:/Temp/"} or \code{getwd()}).
}
}
\details{
The pedigree used can be alphanumeric, numeric only, or alphabet only. The function does not rely on object types within the dataframe to conduct counts and summary statistics.
}
\value{
When the function is given an object name, such as \code{Count = SibCount(Ped,destfile = getwd())}, the object \code{Count} will consist of summary statistics with rows based on:
  \item{FullSib}{Full-sibling statistics}
  \item{PaternalHalfSib}{Paternal half-sibling statistics}
  \item{MaternalHalfSib}{Maternal half-sibling statistics}

and columns of:
  \item{Count}{The number of families present in the pedigree for that sibling type.}
  \item{Average}{The average number of siblings within families present in the pedigree for that sibling type.}
  \item{SD}{The standard deviation to the average number of siblings within families present in the pedigree for that sibling type.}
  \item{Min}{The minimum number of siblings per family found in the pedigree for the sibling type. This must be 2 or more.}
  \item{Max}{The maximum number of siblings per family found in the pedigree for the sibling type. This must be 2 or more.}

This summary table is also exported using the \code{write.table} function if \code{savefiles = TRUE}. Additional data exported include lists by parents that made full-sibling families (\code{FullSibs.txt}), sires that made paternal half-sibling families (\code{PatHalfSibs.txt}), and dams that made maternal half-sibling families (\code{MatHalfSibs.txt}). The typical format for full-sibling family output is:
  \item{Sire}{}
  \item{Dam}{}
  \item{NuFullSib}{}

Half-sibling family files are subsets of full-sibling files.
}
\references{

}
\author{
Lauren L. Hulsman Hanna
}
\note{
For issues or problems with this function, please contact Lauren Hanna at \email{Lauren.Hanna@ndsu.edu}.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Ped = data.frame(ID = c(1,2,3,4,5,6,7,8), Sire = c(0,0,1,1,3,3,5,5), Dam = c(0,0,0,2,4,4,6,6))
Count = SibCount(Ped,destfile = getwd())
}
