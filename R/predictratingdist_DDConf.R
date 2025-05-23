#' Prediction of Confidence Rating and Reaction Time Distribution in the drift diffusion confidence model
#'
#' \code{predictDDConf_Conf} predicts the categorical response distribution of
#' decision and confidence ratings, \code{predictDDConf_RT} computes the
#' RT distribution (density) in the drift diffusion confidence model
#' (Hellmann et al., 2023), given specific parameter
#' constellations. See \code{\link{dDDConf}} for more information about the model
#' and parameters.
#'
#' @param paramDf a list or data frame with one row. Column names should match the names of
#' \link{DDConf} model parameter names. For different stimulus quality/mean
#' drift rates, names should be `v1`, `v2`, `v3`,....
#' Different `sv` and/or `s` parameters are possible with `sv1`, `sv2`, `sv3`... (`s1`, `s2`, `s3`,...
#' respectively) with equally many steps as for drift rates. Additionally, the confidence
#' thresholds should be given by names with `thetaUpper1`, `thetaUpper2`,..., `thetaLower1`,... or,
#' for symmetric thresholds only by `theta1`, `theta2`,....
#' @param maxrt numeric. The maximum RT for the
#' integration/density computation. Default: 15 (for `predictDDConf_Conf`
#' (integration)), 9 (for `predictDDConf_RT`).
#' @param subdivisions \code{integer} (default: 100).
#' For \code{predictDDConf_Conf} it is used as argument for the inner integral routine.
#' For \code{predictDDConf_RT} it is the number of points for which the density is computed.
#' @param minrt numeric or `NULL`(default). The minimum rt for the density computation.
#' @param scaled logical. For \code{predictDDConf_RT}. Whether the computed density
#' should be scaled to integrate to one (additional column `densscaled`). Otherwise the output
#' contains only the defective density (i.e. its integral is equal to the probability of a
#' response and not 1). If `TRUE`, the argument `DistConf` should be given, if available.
#' Default: `FALSE`.
#' @param DistConf `NULL` or `data.frame`. A `data.frame` or `matrix` with column
#' names, giving the distribution of response and rating choices for
#' different conditions and stimulus categories in the form of the output of
#' \code{predictDDConf_Conf}. It is only necessary, if `scaled=TRUE`, because these
#' probabilities are used for scaling. If `scaled=TRUE` and `DistConf=NULL`, it will be computed
#' with the function \code{predictDDConf_Conf}, which takes some time and the function will
#' throw a message. Default: `NULL`
#' @param stop.on.error logical. Argument directly passed on to integrate. Default is `FALSE`,
#' since the densities invoked may lead to slow convergence of the integrals (which are still
#' quite accurate) which causes R to throw an error.
#' @param .progress logical. If `TRUE` (default) a progress bar is drawn to the console.
#'
#' @return \code{predictDDConf_Conf} returns a `data.frame`/`tibble` with columns: `condition`, `stimulus`,
#' `response`, `rating`, `correct`, `p`, `info`, `err`. `p` is the predicted probability of a response
#' and `rating`, given the stimulus category and condition. `info` and `err` refer to the
#' respective outputs of the integration routine used for the computation.
#' \code{predictDDConf_RT} returns a `data.frame`/`tibble` with columns: `condition`, `stimulus`,
#' `response`, `rating`, `correct`, `rt` and `dens` (and `densscaled`, if `scaled=TRUE`).
#'
#'
#' @details The function \code{predictDDConf_Conf} consists merely of an integration of
#' the response time density, \code{\link{dDDConf}}, over the
#' response time in a reasonable interval (0 to `maxrt`). The function
#' \code{predictDDConf_RT} wraps these density
#' functions to a parameter set input and a `data.frame` output.
#' For the argument \code{paramDf}, the output of the fitting function \code{\link{fitRTConf}}
#' with the DDConf model may be used.
#'
#' @note Different parameters for different conditions are only allowed for drift rate
#' \code{v}, drift rate variability \code{sv}, and process variability `s`. Otherwise, `s` is
#' not required in `paramDf` but set to 1 by default. All other parameters are used for all
#' conditions.
#'
#' @references  Hellmann, S., Zehetleitner, M., & Rausch, M. (2023). Simultaneous modeling of choice, confidence and response time in visual perception. \emph{Psychological Review} 2023 Mar 13. doi: 10.1037/rev0000411. Epub ahead of print. PMID: 36913292.
#'
#' @author Sebastian Hellmann.
#'
#' @name predictDDConf
#' @importFrom stats integrate
#' @importFrom progress progress_bar
# @importFrom pracma integral
#'
#' @examples
#' # 1. Define some parameter set in a data.frame
#' paramDf <- data.frame(a=2,v1=0.5, v2=1, t0=0.1,z=0.55,
#'                       sz=0,sv=0.2, st0=0, theta1=0.8)
#'
#' # 2. Predict discrete Choice x Confidence distribution:
#' preds_Conf <- predictDDConf_Conf(paramDf,  maxrt = 15)
#' head(preds_Conf)
#'
#' # 3. Compute RT density
#' preds_RT <- predictDDConf_RT(paramDf, maxrt=4, subdivisions=200) #(scaled=FALSE)
#' # same output with scaled density column:
#' preds_RT <- predictDDConf_RT(paramDf, maxrt=4, subdivisions=200,
#'                               scaled=TRUE, DistConf = preds_Conf)
#' head(preds_RT)
#'
#' \donttest{
#'   # Example of visualization
#'   library(ggplot2)
#'   preds_Conf$rating <- factor(preds_Conf$rating, labels=c("unsure", "sure"))
#'   preds_RT$rating <- factor(preds_RT$rating, labels=c("unsure", "sure"))
#'   ggplot(preds_Conf, aes(x=interaction(rating, response), y=p))+
#'     geom_bar(stat="identity")+
#'     facet_grid(cols=vars(stimulus), rows=vars(condition), labeller = "label_both")
#'   ggplot(preds_RT, aes(x=rt, color=interaction(rating, response), y=dens))+
#'     geom_line(stat="identity")+
#'     facet_grid(cols=vars(stimulus), rows=vars(condition), labeller = "label_both")+
#'     theme(legend.position = "bottom")
#'   ggplot(aggregate(densscaled~rt+correct+rating+condition, preds_RT, mean),
#'          aes(x=rt, color=rating, y=densscaled))+
#'     geom_line(stat="identity")+
#'     facet_grid(cols=vars(condition), rows=vars(correct), labeller = "label_both")+
#'     theme(legend.position = "bottom")
#' }
#' # Use PDFtoQuantiles to get predicted RT quantiles
#' head(PDFtoQuantiles(preds_RT, scaled = FALSE))
#'

#' @rdname predictDDConf
#' @export
predictDDConf_Conf <- function(paramDf,
                                maxrt=15, subdivisions = 100L, stop.on.error=FALSE,
                                .progress=TRUE){

  nConds <- length(grep(pattern = "^v[0-9]", names(paramDf), value = T))
  symmetric_confidence_thresholds <- length(grep(pattern = "thetaUpper", names(paramDf), value = T))<1
  if (symmetric_confidence_thresholds) {
    nRatings <- length(grep(pattern = "^theta[0-9]", names(paramDf)))+1
  } else {
    nRatings <- length(grep(pattern = "^thetaUpper[0-9]", names(paramDf)))+1
  }

  if (nConds > 0 ) {
    V <- c(t(paramDf[,paste("v",1:(nConds), sep = "")]))
  } else {
    V <- paramDf$v
    nConds <- 1
  }
  vary_s <-   length(grep(pattern = "^s[0-9]", names(paramDf), value = T))>1
  if (vary_s){
    S <- c(t((paramDf[,paste("s",1:(nConds), sep = "")])))
  } else {
    if ("s" %in% names(paramDf)) {
      S <- rep(paramDf$s, nConds)
    } else {
      S <- rep(1, nConds)
    }
  }
  vary_sv <-   length(grep(pattern = "^sv[0-9]", names(paramDf), value = T))>1
  if (vary_sv){   ## ToDo: vary< sv across conditions
    SV <- c(t((paramDf[,paste("sv",1:(nConds), sep = "")])))
  } else {
    SV <- rep(paramDf$sv, nConds)
  }
  ## Recover confidence thresholds
  if (symmetric_confidence_thresholds) {
    thetas_upper <- c(0, t(paramDf[,paste("theta",(nRatings-1):1, sep = "")]), 1e+64)
    thetas_lower <- c(0, t(paramDf[,paste("theta",(nRatings-1):1, sep = "")]), 1e+64)
  } else {
    thetas_upper <- c(0, t(paramDf[,paste("thetaUpper",(nRatings-1):1, sep = "")]), 1e+64)
    thetas_lower <- c(0, t(paramDf[,paste("thetaLower",(nRatings-1):1, sep="")]), 1e+64)
  }
  # Because we integrate over the response time, st0 does not matter
  # So, to speed up computations for high values of st0, we set it to 0
  # but add the constant to maxrt
  maxrt <- maxrt + paramDf$st0
  a    = paramDf$a
  z    = paramDf$z
  sz = paramDf$sz

  res <- expand.grid(condition = 1:nConds, stimulus=c(-1,1),
                     response=c(-1,1), rating = 1:nRatings,
                     p=NA, info=NA, err=NA)
  if (.progress) {
    pb <- progress_bar$new(total = nConds*nRatings*4)
  }

  for (i in 1:nrow(res)) {
    row <- res[i,]
    s    = S[row$condition]
    th1  = ifelse(row$response == 1, thetas_upper[(nRatings+1-row$rating)], thetas_lower[(nRatings+1-row$rating)])
    th2  = ifelse(row$response == 1, thetas_upper[(nRatings+2-row$rating)], thetas_lower[(nRatings+2-row$rating)])
    v    = V[row$condition]*(row$stimulus)
    sv   = SV[row$condition]
    p <- integrate(function(rt) return(dDDConf(rt, response=row$response,
                                                th1 = 0, th2=1e+64,
                                                v=v, s=s,
                                                sv = sv, z=z, sz=sz,
                                                a = a,
                                                st0 = 0, t0 =0,
                                                z_absolute = FALSE)),
                   lower=th1, upper=min(th2, maxrt), subdivisions = subdivisions,
                   stop.on.error = stop.on.error)
    if (.progress) pb$tick()
    res[i, 5:7] <- list(p = p$value, info = p$message, err = p$abs.error)
  }
  res$correct <- as.numeric(res$stimulus==(2*as.numeric(res$response=="upper")-1 ))
  res <- res[c("condition", "stimulus", "response", "correct", "rating", "p", "info", "err")]
  # the last line is to sort the output columns
  # (to combine outputs from predictWEV_Conf and predictDDConf_Conf)
  res
}



### Predict RT-distribution
#' @rdname predictDDConf
#' @export
predictDDConf_RT <- function(paramDf,
                              maxrt=9, subdivisions = 100L,  minrt=NULL,
                              scaled = FALSE, DistConf=NULL,
                              .progress = TRUE) {
  if (scaled && is.null(DistConf)) {
    message(paste("scaled is TRUE and DistConf is NULL. The rating distribution will",
                  " be computed, which will take additional time.", sep=""))
  }
  nConds <- length(grep(pattern = "^v[0-9]", names(paramDf), value = T))
  symmetric_confidence_thresholds <- length(grep(pattern = "thetaUpper", names(paramDf), value = T))<1
  if (symmetric_confidence_thresholds) {
    nRatings <- length(grep(pattern = "^theta[0-9]", names(paramDf)))+1
  } else {
    nRatings <- length(grep(pattern = "^thetaUpper[0-9]", names(paramDf)))+1
  }

  if (nConds > 0 ) {
    V <- c(t(paramDf[,paste("v",1:(nConds), sep = "")]))
  } else {
    V <- paramDf$v
    nConds <- 1
  }
  vary_s <-   length(grep(pattern = "^s[0-9]", names(paramDf), value = T))>1
  if (vary_s){
    S <- c(t((paramDf[,paste("s",1:(nConds), sep = "")])))
  } else {
    if ("s" %in% names(paramDf)) {
      S <- rep(paramDf$s, nConds)
    } else {
      S <- rep(1, nConds)
    }
  }
  vary_sv <-   length(grep(pattern = "^sv[0-9]", names(paramDf), value = T))>1
  if (vary_sv){   ## ToDo: vary< sv across conditions
    SV <- c(t((paramDf[,paste("sv",1:(nConds), sep = "")])))
  } else {
    SV <- rep(paramDf$sv, nConds)
  }
  ## Recover confidence thresholds
  if (symmetric_confidence_thresholds) {
    thetas_upper <- c(0, t(paramDf[,paste("theta",(nRatings-1):1, sep = "")]), 1e+64)
    thetas_lower <- c(0, t(paramDf[,paste("theta",(nRatings-1):1, sep = "")]), 1e+64)
  } else {
    thetas_upper <- c(0, t(paramDf[,paste("thetaUpper",(nRatings-1):1, sep = "")]), 1e+64)
    thetas_lower <- c(0, t(paramDf[,paste("thetaLower",(nRatings-1):1, sep="")]), 1e+64)
  }

  if (is.null(minrt)) minrt <- paramDf$t0
  rt = seq(minrt, maxrt, length.out = subdivisions)
  df <- expand.grid(rt = rt,
                    rating = 1:nRatings,
                    response=c(-1,1),
                    stimulus=c(-1,1),
                    condition = 1:nConds, dens=NA)
  if (scaled) {
    ## Scale RT-density to integrate to 1 (for plotting together with simulations)
    # Therefore, divide the density by the probability of a
    # decision-rating-response (as in data.frame DistConf)
    if (is.null(DistConf)) {
      DistConf <- predictDDConf_Conf(paramDf,
                                      maxrt = maxrt, subdivisions=subdivisions,
                                      .progress = FALSE)
    }
    DistConf <- DistConf[,c("rating", "response", "stimulus", "condition", "p")]
    df$densscaled <- NA
  }


  if (.progress) {
    pb <- progress_bar$new(total = nConds*nRatings*4)
  }
  for ( i in 1:(nRatings*2*2*nConds)) {
    cur_row <- df[1+((i-1)*subdivisions),]
    s <- S[cur_row$condition]
    th1 <- ifelse(cur_row$response == 1, thetas_upper[(nRatings+1-cur_row$rating)], thetas_lower[(nRatings+1-cur_row$rating)])
    th2 <-ifelse(cur_row$response == 1, thetas_upper[(nRatings+2-cur_row$rating)], thetas_lower[(nRatings+2-cur_row$rating)])
    v <- V[cur_row$condition]*(cur_row$stimulus)
    sv <- SV[cur_row$condition]

    df[(1:subdivisions) + subdivisions*(i-1), "dens"] <-
      dDDConf(rt, as.character(cur_row$response),
              th1 = th1, th2=th2,
              v = v, s=s, sv=sv,
              a  = paramDf$a,
              z  = paramDf$z,  sz  = paramDf$sz,
              t0 = paramDf$t0, st0 = paramDf$st0)
    if (scaled) {
      P <- DistConf[DistConf$condition==cur_row$condition &
                      DistConf$response==cur_row$response &
                      DistConf$rating == cur_row$rating &
                      DistConf$stimulus==cur_row$stimulus,]$p
      if (P != 0) {
        df[(1:subdivisions) + subdivisions*(i-1), "densscaled"] <-
          df[(1:subdivisions) + subdivisions*(i - 1), "dens"]/P
      } else {
        df[(1:subdivisions) + subdivisions*(i-1), "densscaled"] <- 0
      }
    }
    if (.progress) pb$tick()
  }

  df$correct <-  as.numeric(df$stimulus==df$response)
  df <- df[,c("condition", "stimulus", "response", "correct", "rating",
             "rt", "dens", rep("densscaled", as.numeric(scaled)))]
  # the last line is to sort the output columns
  # (to combine outputs from predictWEV_RT and predictDDConf_RT)
  return(df)
}
