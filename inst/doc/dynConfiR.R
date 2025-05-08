## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dynConfiR)
library(dplyr)
library(ggplot2)


## ----echo=TRUE----------------------------------------------------------------
data("ConfidenceOrientation")
part8 <- ConfidenceOrientation %>%
  filter(participant == 8) %>%
  select(SOA, stimulus, response, rt, disc_rating)
head(part8)

## ----echo=FALSE---------------------------------------------------------------
parfit <- data.frame(v1 = 0.0372688024414027, v2 = 0.0297559327941849, 
    v3 = 0.228682139296959, v4 = 0.907332624555809, v5 = 1.51928135797365, 
    sv = 0.703366746805957, a = 1.9909337760955, z = 0.484042545790362, 
    sz = 0.968085010668281, t0 = 0.013792576932083, st0 = 0.50473451191079839, 
    thetaLower1 = 0.977932449428557, thetaLower2 = 1.4002660381916, 
    thetaLower3 = 1.65136147933687, thetaLower4 = 1.82689584748271, 
    thetaUpper1 = 0.894965128071132, thetaUpper2 = 1.21972431731305, 
    thetaUpper3 = 1.67435525566997, thetaUpper4 = 1.85889858426025, 
    tau = 1.49783458056779, w = 0.632631960506194, svis = 0.00228472967185629, 
    sigvis = 0.0698971790853653, fixed = "sym_thetas = TRUE", 
    negLogLik = 3130.4865270751, N = 1611L, k = 23L, BIC = 6430.81909296327, 
    AICc = 6307.61073530962, AIC = 6306.9730541502)

## -----------------------------------------------------------------------------
part8 <- part8 %>% rename(condition=SOA, 
                          rating = disc_rating)
# parfit <- fitRTConf(part8, "dynWEV",
#                     restr_tau="simult_conf")
parfit

## -----------------------------------------------------------------------------
predictedResponses <- 
  predictWEV_Conf(parfit, "dynWEV", simult_conf = TRUE, 
                  precision = 3, maxrt = 5, subdivisions = 50)
predictedRTdist <- 
  predictWEV_RT(parfit, "dynWEV", simult_conf = TRUE, 
  maxrt = 5, precision = 3, subdivisions = 50,
  scaled=TRUE, DistConf = predictedResponses)
print(head(predictedResponses))
print(head(predictedRTdist))


## -----------------------------------------------------------------------------
part8 <- part8 %>% 
  mutate(condition = as.factor(condition), 
         correct = as.numeric(stimulus==response))
empirical_response_dist <- part8 %>% 
  group_by(condition) %>% 
  mutate(ntrials = n()) %>%
  group_by(correct, condition, rating) %>%
  summarise(p = n()/ntrials[1], .groups = "drop")
predictedResponses <- predictedResponses %>%
  mutate(condition = factor(condition, labels=levels(part8$condition))) %>%
  group_by(correct, condition, rating) %>%
  summarise(p = mean(p), .groups = "drop") 
  
predictedRTdist <- predictedRTdist %>%
  mutate(condition = factor(condition, labels=levels(part8$condition))) %>%
  group_by(correct, rating, rt) %>%
  summarise(dens = mean(dens),
            densscaled = mean(densscaled), .groups = "drop")


## ----out.width="100%", fig.dim=c(6, 8)----------------------------------------
ggplot(empirical_response_dist, aes(x=rating, y=p)) +
  geom_bar(aes(fill=as.factor(correct)), stat="identity")+
  geom_point(data=predictedResponses) +
  scale_fill_discrete(name="Accuracy")+
  facet_grid(cols=vars(correct), rows=vars(condition))


## ----out.width="100%", fig.dim=c(6, 8)----------------------------------------
ggplot(subset(part8, rt<18), aes(x=rt, color=as.factor(correct))) +
  geom_density(aes(linetype="Observed"), linewidth=1)+
  geom_line(data = predictedRTdist, 
            aes(y=densscaled, linetype="Prediction"), 
            linewidth=1)+
  scale_color_discrete(name="Accuracy")+
  scale_linetype_discrete(name="")+
  theme(legend.position = "bottom")+
  xlim(0, 5)+
  facet_grid(rows=vars(rating), cols=vars(correct))
  

