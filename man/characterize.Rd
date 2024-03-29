% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/characterize.R
\name{characterize}
\alias{characterize}
\alias{characterize.default}
\alias{characterize.model_fit}
\alias{characterize.workflow}
\alias{characterize.glmnet}
\alias{characterize.cubist}
\alias{characterize.tidy_cubist}
\alias{characterize.tidy_C50}
\alias{characterize.C5.0}
\alias{characterize.xgb.Booster}
\alias{characterize.tidy_xrf}
\alias{characterize.xrf}
\alias{characterize.lgb.Booster}
\alias{characterize.fda}
\title{Compute model characteristics}
\usage{
characterize(x, ...)

\method{characterize}{default}(x, ...)

\method{characterize}{model_fit}(x, ...)

\method{characterize}{workflow}(x, ...)

\method{characterize}{glmnet}(x, penalty = 0.001, ...)

\method{characterize}{cubist}(x, committees = NULL, ...)

\method{characterize}{tidy_cubist}(x, committees = NULL, ...)

\method{characterize}{cubist}(x, committees = NULL, ...)

\method{characterize}{tidy_C50}(x, trials = max(x$trials), ...)

\method{characterize}{C5.0}(x, trials = x$trials["Actual"], ...)

\method{characterize}{xgb.Booster}(x, nrounds = x$niter, ...)

\method{characterize}{tidy_xrf}(x, penalty = 0.001, ...)

\method{characterize}{xrf}(x, penalty = 0.001, ...)

\method{characterize}{lgb.Booster}(x, trees = NULL, ...)

\method{characterize}{fda}(x, ...)
}
\arguments{
\item{x}{An object.}

\item{...}{Not currently used.}

\item{penalty}{A numeric parameter value for the amount of
regularization/penalty of the model.}

\item{committees}{The number of Cubist committees to use in the characterization.}

\item{trials, nrounds, trees}{The number of boosting to use in the characterization.}
}
\value{
A tibble with columns \code{.metric}, \code{.estimator}, and \code{.estimate}. If
there are no statistics to compute, a zero-row tibble is returned. Integer
values are converted to double.
}
\description{
\code{characterize()} attempts to report data driven characteristics of a model fit
that describe the model. For example, the number of predictors used, the
number of terminal nodes, etc. These are interesting characteristics of the
model that often reflect complexity of the model.
}
\details{
For a list of object types supported, see \code{\link[=list_characteristics]{list_characteristics()}}.
}
\examples{

library(rpart)
cart_mod <- rpart(mpg ~ ., data = mtcars)
characterize(cart_mod)

# When there is nothing to report:
ppr_mod <- ppr(mpg ~ ., data = mtcars, nterms = 5)
characterize(ppr_mod)
}
\seealso{
\code{\link[=multi_characterize]{multi_characterize()}}
}
