% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1-1-class-def.R, R/1-2-method-def.R
\docType{class}
\name{cvGrid-class}
\alias{cvGrid-class}
\alias{show,cvGrid-method}
\title{An S4 class containing the grid search results}
\usage{
\S4method{show}{cvGrid}(object)
}
\arguments{
\item{object}{A \code{cvGrid} object.}
}
\description{
An S4 class containing the grid search results
}
\section{Methods (by generic)}{
\itemize{
\item \code{show}: Method to show \code{cvGrid} object.
}}

\section{Slots}{

\describe{
\item{\code{best.model}}{The best model fitted with the grid that has an optimal performance}

\item{\code{performances}}{The performance table for all grids}

\item{\code{cutoff.list}}{Cutoff estimated fro all grids}

\item{\code{models}}{A list of models for all grids}
}}

\seealso{
\code{\link{ModelObj-class}}\cr
\code{\link{ModelEnsembleObj-class}}\cr
\code{\link{ITRObj-class}}\cr
\code{\link{PredObj-class}}\cr
\code{\link{ListPredObj-class}}\cr
\code{\link{cvGrid-class}}\cr
\code{\link{cvKfold-class}}\cr
\code{\link{workflow-class}}\cr
}
