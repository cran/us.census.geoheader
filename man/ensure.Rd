% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uscgh.R
\name{ensure}
\alias{ensure}
\title{(internal) Make Sure a Dataframe Exists}
\usage{
ensure(name)
}
\arguments{
\item{name}{Name of the dataframe

Stops in the case of an error}
}
\value{
returns the (possibly retrieved) dataframe.
}
\description{
ensure() returns the dataframe (or tibble), reading it in if necessary
}
\details{
takes the name of the data and the suffix of the backing store for
the dataframe and makes sure it is in our private environment.
}
