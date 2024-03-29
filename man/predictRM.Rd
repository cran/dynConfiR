% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictratingdist_RM.R
\name{predictRM}
\alias{predictRM}
\alias{predictRM_Conf}
\alias{predictIRM}
\alias{predictPCRM}
\alias{predictRM_RT}
\title{Prediction of Confidence Rating and Reaction Time Distribution in race models of confidence}
\usage{
predictRM_Conf(paramDf, model = "IRM", time_scaled = FALSE, maxrt = 15,
  subdivisions = 100L, stop.on.error = FALSE, .progress = TRUE)

predictRM_RT(paramDf, model = "IRM", time_scaled = FALSE, maxrt = 9,
  subdivisions = 100L, minrt = NULL, scaled = FALSE, DistConf = NULL,
  .progress = TRUE)
}
\arguments{
\item{paramDf}{a list or data frame with one row. Column names should match the names of
\link{RaceModels} parameter names (only \code{mu1} and \code{mu2} are not used in this context but
replaced by the parameter \code{v}). For different stimulus quality/mean
drift rates, names should be \code{v1}, \code{v2}, \code{v3},....
Different \code{s} parameters are possible with \code{s1}, \code{s2}, \code{s3},... with equally many steps as for drift rates. Additionally, the confidence
thresholds should be given by names with \code{thetaUpper1}, \code{thetaUpper2},..., \code{thetaLower1},... or,
for symmetric thresholds only by \code{theta1}, \code{theta2},....}

\item{model}{character scalar. One of "IRM" or "PCRM". ("IRMt" and "PCRMt" will also be accepted. In that case,
time_scaled is set to TRUE.)}

\item{time_scaled}{logical. Whether the confidence measure should be scaled by 1/sqrt(rt). Default: FALSE.
(It is set to TRUE, if model is "IRMt" or "PCRMt")}

\item{maxrt}{numeric. The maximum RT for the
integration/density computation. Default: 15 (for \code{predictRM_Conf}
(integration)), 9 (for \code{predictRM_RT}).}

\item{subdivisions}{\code{integer} (default: 100).
For \code{predictRM_Conf} it is used as argument for the inner integral routine.
For \code{predictRM_RT} it is the number of points for which the density is computed.}

\item{stop.on.error}{logical. Argument directly passed on to integrate. Default is FALSE,
since the densities invoked may lead to slow convergence of the integrals (which are still
quite accurate) which causes R to throw an error.}

\item{.progress}{logical. If TRUE (default) a progress bar is drawn to the console.}

\item{minrt}{numeric or NULL(default). The minimum rt for the density computation.}

\item{scaled}{logical. For \code{predictRM_RT}. Whether the computed density
should be scaled to integrate to one (additional column \code{densscaled}). Otherwise the output
contains only the defective density (i.e. its integral is equal to the probability of a
response and not 1). If \code{TRUE}, the argument \code{DistConf} should be given, if available.
Default: \code{FALSE}.}

\item{DistConf}{\code{NULL} or \code{data.frame}. A \code{data.frame} or \code{matrix} with column
names, giving the distribution of response and rating choices for
different conditions and stimulus categories in the form of the output of
\code{predictRM_Conf}. It is only necessary, if \code{scaled=TRUE}, because these
probabilities are used for scaling. If \code{scaled=TRUE} and \code{DistConf=NULL}, it will be computed
with the function \code{predictRM_Conf}, which takes some time and the function will
throw a message. Default: \code{NULL}}
}
\value{
\code{predictRM_Conf} returns a \code{data.frame}/\code{tibble} with columns: \code{condition}, \code{stimulus},
\code{response}, \code{rating}, \code{correct}, \code{p}, \code{info}, \code{err}. \code{p} is the predicted probability of a response
and \code{rating}, given the stimulus category and condition. \code{info} and \code{err} refer to the
respective outputs of the integration routine used for the computation.
\code{predictRM_RT} returns a \code{data.frame}/\code{tibble} with columns: \code{condition}, \code{stimulus},
\code{response}, \code{rating}, \code{correct}, \code{rt} and \code{dens} (and \code{densscaled}, if \code{scaled=TRUE}).
}
\description{
\code{predictRM_Conf} predicts the categorical response distribution of
decision and confidence ratings, \code{predictRM_RT} computes the
RT distribution (density) in the independent and partially anti-correlated
race models  (Hellmann et al., 2023), given specific parameter
constellations. See \link{RaceModels} for more information about the models
and parameters.
}
\details{
The function \code{predictRM_Conf} consists merely of an integration of
the response time density, \code{\link{dIRM}} and \code{\link{dPCRM}}, over the
response time in a reasonable interval (0 to \code{maxrt}). The function
\code{predictRM_RT} wraps these density
functions to a parameter set input and a data.frame output.
For the argument \code{paramDf}, the output of the fitting function \code{\link{fitRTConf}}
with the respective model may be used.

The drift rate parameters differ from those used in \code{\link{dIRM}}/\code{\link{dPCRM}}
since in many perceptual decision experiments the drift on one accumulator is assumed to
be the negative of the other. The drift rate of the correct accumulator is \code{v} (\code{v1}, \code{v2},
... respectively) in \code{paramDf}.
}
\note{
Different parameters for different conditions are only allowed for drift rate,
\code{v}, and process variability \code{s}. All other parameters are used for all
conditions.
}
\examples{
# Examples for "PCRM" model (equivalent applicable for "IRM" model)
# 1. Define some parameter set in a data.frame
paramDf <- data.frame(a=2,b=2, v1=0.5, v2=1, t0=0.1,st0=0,
                      wx=0.6, wint=0.2, wrt=0.2,
                      theta1=4)

# 2. Predict discrete Choice x Confidence distribution:
preds_Conf <- predictRM_Conf(paramDf, "PCRM", time_scaled=TRUE)
# equivalent:
preds_Conf <- predictRM_Conf(paramDf, "PCRMt")
head(preds_Conf)

# 3. Compute RT density
preds_RT <- predictRM_RT(paramDf, "PCRMt", maxrt=7, subdivisions=50)
# same output with scaled density column:
preds_RT <- predictRM_RT(paramDf, "PCRMt", maxrt=7, subdivisions=50,
                         scaled=TRUE, DistConf = preds_Conf)
head(preds_RT)
\donttest{
  # produces a warning, if scaled=TRUE and DistConf missing
  preds_RT <- predictRM_RT(paramDf, "PCRMt", maxrt=7, subdivisions=50,
                           scaled=TRUE)
}

\donttest{
  # Example of visualization
  library(ggplot2)
  preds_Conf$rating <- factor(preds_Conf$rating, labels=c("unsure", "sure"))
  preds_RT$rating <- factor(preds_RT$rating, labels=c("unsure", "sure"))
  ggplot(preds_Conf, aes(x=interaction(rating, response), y=p))+
    geom_bar(stat="identity")+
    facet_grid(cols=vars(stimulus), rows=vars(condition), labeller = "label_both")
  ggplot(preds_RT, aes(x=rt, color=interaction(rating, response), y=dens))+
    geom_line(stat="identity")+
    facet_grid(cols=vars(stimulus), rows=vars(condition), labeller = "label_both")+
    theme(legend.position = "bottom")
  ggplot(aggregate(densscaled~rt+correct+rating+condition, preds_RT, mean),
         aes(x=rt, color=rating, y=densscaled))+
    geom_line(stat="identity")+
    facet_grid(cols=vars(condition), rows=vars(correct), labeller = "label_both")+
    theme(legend.position = "bottom")
}
\donttest{
  # Use PDFtoQuantiles to get predicted RT quantiles
  # (produces warning because of few rt steps (--> inaccurate calculations))
  PDFtoQuantiles(preds_RT, scaled = FALSE)
}

}
\references{
Hellmann, S., Zehetleitner, M., & Rausch, M. (2023). Simultaneous modeling of choice, confidence and response time in visual perception. \emph{Psychological Review} 2023 Mar 13. doi: 10.1037/rev0000411. Epub ahead of print. PMID: 36913292.
}
\author{
Sebastian Hellmann.
}
