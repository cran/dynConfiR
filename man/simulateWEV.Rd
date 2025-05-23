% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulateWEV.R
\name{simulateWEV}
\alias{simulateWEV}
\alias{simulate2DSD}
\title{Simulation of confidence ratings and RTs in dynWEV and 2DSD confidence models}
\usage{
simulateWEV(paramDf, n = 10000, model = "dynWEV", simult_conf = FALSE,
  gamma = FALSE, agg_simus = FALSE, stimulus = c(-1, 1), delta = 0.01,
  maxrt = 15, seed = NULL, process_results = FALSE)
}
\arguments{
\item{paramDf}{a list or dataframe with one row. Column names should match the names
of \link{dynaViTE} and \link{2DSD} model specific parameter names. For different stimulus quality/mean
drift rates, names should be \code{v1}, \code{v2}, \code{v3},....
Different \code{sv} and/or \code{s} parameters are possible with \code{sv1}, \code{sv2}, \code{sv3}... (\code{s1}, \code{s2}, \code{s3},...
respectively) with equally many steps as for drift rates. Additionally, the confidence
thresholds should be given by names with \code{thetaUpper1}, \code{thetaUpper2},..., \code{thetaLower1},... or,
for symmetric thresholds only by \code{theta1}, \code{theta2},....}

\item{n}{integer. The number of samples (per condition and stimulus direction) generated.
Total number of samples is \code{n*nConditions*length(stimulus)}.}

\item{model}{character scalar. One of "dynaViTE", "dynWEV", or "2DSD".}

\item{simult_conf}{logical. \code{TRUE}, if in the experiment confidence was reported simultaneously
with the decision, as then decision and confidence judgment are assumed to have happened
subsequent before response and tau is added to the simulated decision time. If \code{FALSE}
returned response time will only be decision time plus non-judgment time component.}

\item{gamma}{logical. If TRUE, the gamma correlation between confidence ratings, rt
and accuracy is computed.}

\item{agg_simus}{logical. Simulation is done on a trial basis with RTs outcome.
If TRUE, the simulations will be aggregated over RTs to return only the distribution
of response and confidence ratings. Default: FALSE.}

\item{stimulus}{numeric vector. Either 1, -1 or c(-1, 1) (default). Together with
condition represents the experimental situation. In a binary decision task the presented
stimulus belongs to one of two categories. In the default setting trials with
both categories presented are simulated but one can choose to simulate only trials with
the stimulus coming from one category (1 for the category that is associated with positive
drift in the decision process where "upper"/1 responses are considered correct and -1
correspondingly for negative drifts and "lower"/-1 correct decisions).}

\item{delta}{numeric. Discretization steps for simulations with the stochastic process.}

\item{maxrt}{numeric. Maximum reaction time returned.
If the simulation of the stochastic process exceeds a rt of \code{maxrt},
the response will be set to 0 and \code{maxrt} will be returned as rt.}

\item{seed}{numerical. Seeding for non-random data generation.}

\item{process_results}{logical. Whether the output simulations should contain the final
state of the decision (and visibility) process as additional column. Default is FALSE, meaning that
no additional columns for the final process states are returned.}
}
\value{
Depending on \code{gamma} and \code{agg_simus}.

If \code{gamma} is \code{FALSE}, returns a \code{data.frame} with columns: \code{condition},
\code{stimulus}, \code{response}, \code{correct}, \code{rt}, \code{conf} (the continuous confidence
measure) and \code{rating} (the discrete confidence rating), and \code{dec} and \code{vis}
(only if \code{process_results=TRUE}) for the final states of accumulators in the
simulation or
(if \code{agg_simus=TRUE}): \code{condition}, \code{stimulus},\code{response}, \code{correct},
\code{rating} and \code{p} (for the probability of a response and rating, given
the condition and stimulus).

If \code{gamma} is \code{TRUE}, returns a \code{list} with elements:
\code{simus} (the simulated data frame) and \code{gamma}, which is again a \code{list} with elements
\code{condition}, \code{rt} and \code{correct}, each a \code{tibble} with two columns (see details for more
information).
}
\description{
Simulates the decision responses and reaction times together with a
discrete confidence judgment in the dynaViTE model, the 2DSD model (Pleskac & Busemeyer, 2010)
and the dynWEV model (Hellmann et al., 2023), given specific parameter constellations.
See \code{\link{ddynaViTE}} and \code{\link{d2DSD}} for more information about parameters.
Also computes the Gamma rank correlation between the confidence ratings and condition
(task difficulty), reaction times and accuracy in the simulated output.
Basically, this function is a wrapper for \code{\link{rdynaViTE}} and \code{\link{r2DSD}}
for application in confidence experiments with manipulation of specific parameters.
}
\details{
Simulation of response and decision times is done by simulating
normal variables in discretized steps until the lower or upper boundary
is met (or the maximal rt is reached). Afterwards, a confidence measure
is simulated according to the respective model.

The confidence outputs are then binned according to the given thresholds.
The output of the fitting function \code{\link{fitRTConf}} with the respective model
fits the argument \code{paramDf} for simulation.
The Gamma coefficients are computed separately for correct/incorrect responses for the
correlation of confidence ratings with condition and rt and separately for conditions
for the correlation of accuracy and confidence. The
resulting data frames in the output thus have two columns. One for the grouping variable
and one for the Gamma coefficient.
}
\note{
Different parameters for different conditions are only allowed for drift rate,
\code{v}, drift rate variability, \code{sv} and diffusion constant \code{s}.
All other parameters are used for all conditions.
}
\examples{
# Examples for "dynWEV" model (equivalent applicable
# for "2DSD" model (with less parameters))
# 1. Define some parameter set in a data.frame
paramDf <- data.frame(a=2.5,v1=0.1, v2=1, t0=0.1,z=0.55,
                      sz=0.3,sv=0.8, st0=0,  tau=3, w=0.1,
                      theta1=0.8, svis=0.5, sigvis=0.8)

# 2. Simulate trials for both stimulus categories and all conditions (2)
simus <- simulateWEV(paramDf, model="dynWEV")
head(simus)
\donttest{
  library(ggplot2)
  simus <- simus[simus$response!=0,]
  simus$rating <- factor(simus$rating, labels=c("unsure", "sure"))
  ggplot(simus, aes(x=rt, group=interaction(correct, rating),
                    color=as.factor(correct), linetype=rating))+
    geom_density(linewidth=1.2)+xlim(c(0,5))+
    facet_grid(rows=vars(condition), labeller = "label_both")
}

# automatically aggregate simulation distribution
# to get only accuracy x confidence rating distribution for
# all conditions
agg_simus <- simulateWEV(paramDf, model="dynWEV", agg_simus = TRUE)
head(agg_simus)
\donttest{
  agg_simus$rating <- factor(agg_simus$rating, labels=c("unsure", "sure"))
  library(ggplot2)
  ggplot(agg_simus, aes(x=rating, group=correct, fill=as.factor(correct), y=p))+
    geom_bar(stat="identity", position="dodge")+
    facet_grid(cols=vars(condition), labeller = "label_both")
}
\donttest{
  # Compute Gamma correlation coefficients between
  # confidence and other behavioral measures
  # output will be a list
  simu_list <- simulateWEV(paramDf,n = 400, model="dynWEV", gamma=TRUE)
  simu_list
}
}
\references{
Hellmann, S., Zehetleitner, M., & Rausch, M. (2023). Simultaneous modeling of choice, confidence and response time in visual perception. \emph{Psychological Review} 2023 Mar 13. doi: 10.1037/rev0000411. Epub ahead of print. PMID: 36913292.
}
\author{
Sebastian Hellmann.
}
