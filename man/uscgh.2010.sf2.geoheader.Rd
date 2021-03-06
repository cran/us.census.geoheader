% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uscgh.R
\name{uscgh.2010.sf2.geoheader}
\alias{uscgh.2010.sf2.geoheader}
\title{Return the 2010 Census SF2 Geographic Header Data}
\usage{
uscgh.2010.sf2.geoheader()
}
\value{
a dataframe (tibble, if the tibble package is available)
}
\description{
Returns the 2010 Census SF2 geographic header dataset
}
\details{
`uscgh.2010.dataset` returns the SF2 geographic header data from
the 2010 US census
}
\examples{
x <- uscgh.2010.sf2.geoheader()

}
\seealso{
'RShowDoc("a-tour", package="us.census.geoheader")' for a
  short tour of a few columns of the database
}
