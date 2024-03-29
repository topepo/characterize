% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/num_features_active.R
\name{pluck_num_features_active}
\alias{pluck_num_features_active}
\alias{.pluck_num_features_active}
\alias{.pluck_num_features_active.default}
\alias{.pluck_num_features_active.workflow}
\alias{.pluck_num_features_active.model_fit}
\alias{.pluck_num_features_active.glmnet}
\alias{.pluck_num_features_active.tidy_cubist}
\alias{.pluck_num_features_active.cubist}
\alias{.pluck_num_features_active.tidy_C50}
\alias{.pluck_num_features_active.C5.0}
\alias{.pluck_num_features_active.tidy_xrf}
\alias{.pluck_num_features_active.xrf}
\alias{.pluck_num_features_active.lgb_trees}
\alias{.pluck_num_features_active.lgb.Booster}
\title{Number of predictors used in the model}
\usage{
.pluck_num_features_active(x, ...)

\method{.pluck_num_features_active}{default}(x, ...)

\method{.pluck_num_features_active}{workflow}(x, ...)

\method{.pluck_num_features_active}{model_fit}(x, ...)

\method{.pluck_num_features_active}{glmnet}(x, penalty = 0.001, ...)

\method{.pluck_num_features_active}{tidy_cubist}(x, committees = max(x$committee), ...)

\method{.pluck_num_features_active}{cubist}(x, committees = x$committees, ...)

\method{.pluck_num_features_active}{tidy_C50}(x, trials = max(x$trial), ...)

\method{.pluck_num_features_active}{C5.0}(x, trials = x$trials["Actual"], ...)

\method{.pluck_num_features_active}{tidy_xrf}(x, ...)

\method{.pluck_num_features_active}{xrf}(x, penalty = 0.001, ...)

\method{.pluck_num_features_active}{lgb_trees}(x, trees = max(x$num_iterations), ...)

\method{.pluck_num_features_active}{lgb.Booster}(x, trees = x$params$num_iterations, ...)
}
\arguments{
\item{x}{An object.}

\item{...}{Not currently used.}

\item{penalty}{A numeric parameter value for the amount of
regularization/penalty of the model.}

\item{committees}{The number of Cubist committees to use in the characterization.}
}
\description{
Number of predictors used in the model
}
\keyword{internal}
