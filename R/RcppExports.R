# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

d_2DSD <- function(rts, params, precision = 1e-5, boundary = 2L, stop_on_error = TRUE) {
    .Call(`_dynConfiR_d_2DSD`, rts, params, precision, boundary, stop_on_error)
}

d_WEVmu <- function(rts, params, precision = 1e-5, boundary = 2L, stop_on_error = TRUE) {
    .Call(`_dynConfiR_d_WEVmu`, rts, params, precision, boundary, stop_on_error)
}

d_IRM <- function(rts, params, win = 1L, step_width = 0.0001) {
    .Call(`_dynConfiR_d_IRM`, rts, params, win, step_width)
}

d_PCRM <- function(rts, params, win = 1L, step_width = 0.0001) {
    .Call(`_dynConfiR_d_PCRM`, rts, params, win, step_width)
}

dd_IRM <- function(rts, xj, params, win = 1L, method = 1L) {
    .Call(`_dynConfiR_dd_IRM`, rts, xj, params, win, method)
}

dd_PCRM <- function(rts, xj, params, win = 1L) {
    .Call(`_dynConfiR_dd_PCRM`, rts, xj, params, win)
}

r_RM <- function(n, params, indep, delta = 0.01, maxT = 9) {
    .Call(`_dynConfiR_r_RM`, n, params, indep, delta, maxT)
}

r_WEV <- function(n, params, model, delta = 0.01, maxT = 9, stop_on_error = TRUE) {
    .Call(`_dynConfiR_r_WEV`, n, params, model, delta, maxT, stop_on_error)
}

r_RM_Kiani <- function(n, params, rho, Bl, delta = 0.01, maxT = 9) {
    .Call(`_dynConfiR_r_RM_Kiani`, n, params, rho, Bl, delta, maxT)
}

r_LCA <- function(n, params, delta = 0.01, maxT = 9.0) {
    .Call(`_dynConfiR_r_LCA`, n, params, delta, maxT)
}
