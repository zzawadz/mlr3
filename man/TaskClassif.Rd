% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TaskClassif.R
\name{TaskClassif}
\alias{TaskClassif}
\title{Classification task}
\description{
This task specializes \link{Task} and \link{TaskSupervised} for classification problems.
The target column is assumed to be a factor.
}
\section{Usage}{
\preformatted{t = TaskClassif$new(id, backend, target, positive = NULL)

t$class_names
t$class_n
t$task_type
}
}

\section{Arguments}{

\itemize{
\item \code{positive} (\code{character(1)}):
Name of the "positive" class for binary classification problems.
}
}

\section{Details}{

\code{$class_names} returns all class labels of the rows which \code{role == "use"}.
\code{$class_n} returns the number of class labels of the rows which \code{role == "use"}.
\code{$task_type} is \code{"classif"}
}

\examples{
b = DataBackendDataTable$new(iris)
task = TaskClassif$new("iris", backend = b, target = "Species")
task$formula
task$class_names
}
\seealso{
Other Task: \code{\link{TaskRegr}},
  \code{\link{TaskSupervised}}, \code{\link{Task}},
  \code{\link{mlr_tasks}}
}
\concept{Task}
