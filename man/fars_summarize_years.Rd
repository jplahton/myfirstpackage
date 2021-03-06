% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarize (count the fatal injuries) the data by year and month given specific years}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A vector of years}
}
\value{
This function returns a tibble object which gives the summary by year and month
}
\description{
This function reads data from files specified by the given years and counts
the number of accidents with fatal injuries by year and month.
}
\examples{
fars_summarize_years(2013)
fars_summarize_years(c(2013, 2014, 2015))

}
