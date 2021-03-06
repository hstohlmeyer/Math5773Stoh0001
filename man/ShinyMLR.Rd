% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShinyMLR.R
\name{ShinyMLR}
\alias{ShinyMLR}
\title{Shiny MLR}
\usage{
ShinyMLR(data)
}
\arguments{
\item{data}{dataset}
}
\value{
summary for MLR, normality check, mean of errors
}
\description{
Input your data and then using the Shiny server you can choose your dependent and
independent variables.
}
\examples{
quasar<-readxl::read_excel("C:/Users/hstoh/OneDrive - University of Oklahoma/Fall 2020/MATH 5773/Data/QUASAR.xls")
bub<-readxl::read_excel("C:/Users/hstoh/OneDrive - University of Oklahoma/Fall 2020/MATH 5773/Data/BUBBLE2.xls")
\dontrun{ ShinyMLR(data=quasar)}
\dontrun{ ShinyMLR(data=bub)}

}
