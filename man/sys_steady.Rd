% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sys_steady.R
\name{sys_steady}
\alias{sys_steady}
\title{Iterate to obtain the steady state probability}
\usage{
sys_steady(
  binary_dataset,
  tran_matrix,
  tol_vec = c(0.01, 0.001, 1e-04, 1e-05, 1e-06, 1e-07)
)
}
\arguments{
\item{binary_dataset}{A binary dataframe of datasets used to establish evaluation criteria.}

\item{tran_matrix}{A transfer probability matrix.}

\item{tol_vec}{A smaller constants used as constraints.}
}
\value{
A data frame containing the constraints and the number of iterations to reach the steady state.
}
\description{
Change the values of the constraints step by step and record the number of iterations to reach the steady state.
}
\examples{
tol_iter <- sys_steady(binary_dataset, tran_matrix, 1e-5)
}
