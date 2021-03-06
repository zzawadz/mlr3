% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlr_measures.R
\name{mlr_measures}
\alias{mlr_measures}
\title{Dictionary of registered performance measures}
\description{
Dictionary of registered performance measures
}
\section{Usage}{

See \link{Dictionary}.
}

\examples{
mlr_measures$keys()
as.data.table(mlr_measures)
m = mlr_measures$get("mmce")
print(m)
}
\seealso{
Other Dictionary: \code{\link{Dictionary}},
  \code{\link{mlr_learners}},
  \code{\link{mlr_resamplings}}, \code{\link{mlr_tasks}}

Other Measure: \code{\link{Measure}}
}
\concept{Dictionary}
\concept{Measure}
