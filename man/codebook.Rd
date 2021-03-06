% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codebook.R
\name{codebook}
\alias{codebook}
\title{Create PsychDS Codebook from Data}
\usage{
codebook(
  data,
  name = NULL,
  vardesc = list(),
  ...,
  schemaVersion = "Psych-DS 0.1.0",
  return = c("json", "list", "data"),
  interactive = FALSE
)
}
\arguments{
\item{data}{The data frame to generate a codebook for}

\item{name}{The name of this dataset (if NULL, will be the same as \code{data})}

\item{vardesc}{Optional variable properties in the format of a named list of vectors (can be named or unnamed and in the same order as the data) from the options description, privacy, type, propertyID, minValue, maxValue, levels, ordered, na, naValues, alternateName, unitCode}

\item{...}{Further dataset properties (e.g., description, license, author, citation, funder, url, identifier, keywords, privacyPolicy)}

\item{schemaVersion}{defaults to "Psych-DS 0.1.0"}

\item{return}{Whether the output should be in JSON format (json), a list (list) or the reformatted data with the codebook as an attribute (data)}

\item{interactive}{Whether the function should prompt the user to describe columns and factor levels}
}
\value{
a list or json-formatted codebook, or reformatted data withthe codebook as an attribute
}
\description{
Create PsychDS Codebook from Data
}
\examples{

vardesc = list(
  description = c("Length of the sepal",
                  "Width of the sepal",
                  "Length of the petal",
                  "Width of the petal",
                  "The flower species"),
  type = c("float", "float", "float", "float", "string")
)
codebook(iris, vardesc = vardesc)

}
