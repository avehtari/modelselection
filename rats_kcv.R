#' ---
#' title: "Cross-validation for hierarchical models"
#' author: "Aki Vehtari"
#' date: "First version 2019-03-11. Last modified `r format(Sys.Date())`."
#' output:
#'   html_document:
#'     fig_caption: yes
#'     toc: TRUE
#'     toc_depth: 2
#'     number_sections: TRUE
#'     toc_float:
#'       smooth_scroll: FALSE
#' bibliography: modelsel.bib
#' csl: harvard-cite-them-right.csl
#' ---

#' # Setup  {.unnumbered}
#' 
#+ setup, include=FALSE
knitr::opts_chunk$set(cache=TRUE, message=FALSE, error=FALSE, warning=FALSE, comment=NA)
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#' **Load packages**
library("rprojroot")
root<-has_dirname("modelselection_tutorial")$make_fix_file()
library("loo")
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))

#' # Introduction
#'
#' In this case study, we demonstrate different cross-validation variants for 
#' hierarchical/multilevel models. A [related video](https://www.youtube.com/watch) 
#' discusses the here used examples starting at
#' [22:53](https://www.youtube.com/watch?v=Re-2yVd0Mqk&t=22m53s).
#' For time series specific cross-validation, see @Burkner+Gabry+Vehtari:LFO-CV:2019.
#'

#' # Data
#'
#' Throughout, we will use a simple grouped data.  The example data is taken 
#' from Section 6 of @Gelfand+etal:1990, and concerns 30 young rats whose
#' weights were measured weekly for five consecutive weeks.
#'

#' **Load data**
sourceToList = function(file){
  source(file, local = TRUE)
  d = mget(ls())
  d$file = NULL
  d
}
#
rats = sourceToList(root("rats.data.R"))
rats = with(rats, list(
  N = N,
  Npts = length(y),
  rat = rep(1:nrow(y), ncol(y)),
  x = rep(x, each = nrow(y)),
  y = as.numeric(y),
  xbar = xbar
  ))
#' **Create dataframe**
dfrats <- with(rats, data.frame(age=x, age_c=x-22, weight=y, rat=rat))
N <- dim(dfrats)[1]

#' **Plot data**
pr <- ggplot(data=dfrats, aes(x=age, y=weight)) +
    geom_line(aes(group=rat), color="black", size=0.1) +
    geom_point(color="black", size=2) +
    labs(x="Age (days)", y="Weight (g)", title="Rats data")
pr
#' Just by looking at the data, it seems that if the rat growth would be
#' modelled with a linear model (up to an age of 36 days). Individual 
#' intercepts are likely and possibly also individual slopes.
#' 

#' # Models
#'
#' We are going to compare three models: One with population effect only, 
#' another with an additional varying intercept term, and a third one with
#' both varying intercept and slope terms.
#' 
#' **Simple linear model**
fit_1 <- stan_glm(weight ~ age, data=dfrats, refresh=0)
#' **Linear model with hierarchical intercept**
fit_2 <- stan_glmer(weight ~ age + (1 | rat), data=dfrats, refresh=0)
#' **Linear model with hierarchical intercept and slope**
fit_3 <- stan_glmer(weight ~ age + (age | rat), data=dfrats, refresh=0)

#' # Leave-one-out cross-validation
#'
#' In leave-one-out cross-validation (LOO), one observation is left out at
#' a time and predicted given all the other observations.
pr1 <- pr +
    geom_point(data=dfrats[69,], color="red", size=5, shape=1) +
    ggtitle('Leave-one-out')
pr1
#' This is useful and valid if we are interested in model fit in
#' general. We would use the model to predict random missing data, or
#' if we were comparing different conditional observation models.
#'
#' The `loo` package offers a fast Pareto smoothed importance sampling
#' approximation of LOO [@Vehtari+etal:PSIS-LOO:2017,Vehtari+etal:PSIS:2017]
loo_1 <- loo(fit_1)
loo_2 <- loo(fit_2)
loo_3 <- loo(fit_3)
#' We get warnings that PSIS-LOO is failing for the third model. We
#' can check the details via
loo_3
#' As there are only 5 observations per rat, and hierarchical model
#' has 2 rat specific parameters, some of the observations are highly
#' influential and PSIS-LOO is not able to give reliable estimate (if
#' PSIS-LOO fails, WAIC fails, too, but a failure of WAIC is more
#' difficult to diagnose [@Vehtari+etal:PSIS-LOO:2017])
#'
#' We can run exact LOO-CV for the failing folds.
(loo_3 <- loo(fit_3, k_threshold=0.7))
#' We see that PSIS-LOO-estimated `elpd_loo` for model 3 was too
#' optimistic by 2.6 points. Further its SE was also underestimated.
#' 
#' We can now safely do the model comparison:
loo_compare(loo_1, loo_2, loo_3)
#' Model 3 is slightly better than model 2. Model 1 is clearly worst.
#' Knowing all the other observations except one, it is beneficial to
#' have individual intercept and slope terms.
#' 

#' # K-fold cross-validation
#'
#' In K-fold cross-validation the data is divided in K blocks. By using
#' different ways to divide the data, we can target for different
#' prediction tasks or assess different model parts.
#'
#' ## Random K-fold approximation of LOO
#' 
#' Sometimes it is possible that very large number of PSIS-LOO folds fail. In 
#' this case, performing exact LOO-CV for all of these observations would take 
#' too long. We can approximate LOO cross-validation running K-fold-CV with
#' completely random division of data and then looking at the individual CV
#' predictions.
#'
#' The helper function `kfold_split_random` can be used to form such a random
#' division. We generate random divisions with K=10 and K=30.
cv10rfolds <- kfold_split_random(K=10, N = N)
cv30rfolds <- kfold_split_random(K=30, N = N)
#' Let's illustrate the first of the 30 folds:
prr <- pr +
    geom_point(data=dfrats[cv30rfolds==1,], color="red", size=5, shape=1) +
    ggtitle('Random kfold approximation of LOO')
prr

#' We use the `kfold` function for K-fold cross-validation. (The current
#' release version requires the number of folds (K), but a fixed
#' version can infer it from the folds argument.)
cv10r_1 <- rstanarm::kfold(fit_1, K=10, folds = cv10rfolds)
cv10r_2 <- rstanarm::kfold(fit_2, K=10, folds = cv10rfolds)
cv10r_3 <- rstanarm::kfold(fit_3, K=10, folds = cv10rfolds)
cv30r_1 <- rstanarm::kfold(fit_1, K=30, folds = cv30rfolds)
cv30r_2 <- rstanarm::kfold(fit_2, K=30, folds = cv30rfolds)
cv30r_3 <- rstanarm::kfold(fit_3, K=30, folds = cv30rfolds)

#' Compare models
loo_compare(cv10r_1, cv10r_2, cv10r_3)
loo_compare(cv30r_1, cv30r_2, cv30r_3)
#' The results are similar to LOO. In both cases, the elpd's are slightly
#' lower than with LOO, and the difference is larger for K=10 and
#' increases with model complexity, which is due to model fitted to
#' less observations than in LOO.
#' 

#' ## Stratified K-fold approximation of LOO
#' 
#' The random split might just by chance leave out more than one
#' observation from one rat, which would not be good for approximating
#' LOO in case of hierarchical models. We can further improve K-fold-CV by
#' using stratified resampling which ensures that the
#' relative category frequencies are approximately preserved. In this
#' case, it means that from each rat only up to one observation is left out
#' per fold.
#' 
#' The helper function `kfold_split_stratified` can be used to form a
#' stratified division.
cv10sfolds <- kfold_split_stratified(K=10, x = dfrats$rat)
cv30sfolds <- kfold_split_stratified(K=30, x = dfrats$rat)
#' Let's illustrate the first of the 30 folds:
prs <- pr +
    geom_point(data=dfrats[cv30sfolds==1,], color="red", size=5, shape=1) +
    ggtitle('Stratified K-fold approximation of LOO')
prs

#' We use the `kfold` function for K-fold cross-validation.
cv10s_1 <- rstanarm::kfold(fit_1, K=10, folds = cv10sfolds)
cv10s_2 <- rstanarm::kfold(fit_2, K=10, folds = cv10sfolds)
cv10s_3 <- rstanarm::kfold(fit_3, K=10, folds = cv10sfolds)
cv30s_1 <- rstanarm::kfold(fit_1, K=30, folds = cv30sfolds)
cv30s_2 <- rstanarm::kfold(fit_2, K=30, folds = cv30sfolds)
cv30s_3 <- rstanarm::kfold(fit_3, K=30, folds = cv30sfolds)

#' Compare models
loo_compare(cv10s_1, cv10s_2, cv10s_3)
loo_compare(cv30s_1, cv30s_2, cv30s_3)
#' The results are similar to LOO. In both cases, elpd's are slightly
#' lower than with LOO, and the difference increases with model
#' complexity, which is due to model fitted to less observations than
#' in LOO. For hierarchical models, the results with K=10 and k=30 are
#' closer to each other than in case of complete random division, as
#' the stratified division balances the data division.
#' 

#' ## Grouped K-fold for leave-one-group-out
#' 
#' K-fold cross-validation can also be used for leave-one-group-out
#' cross-validation (LOGO-CV). In our example, each group could represent all
#' observation from a particular rat. LOGO-CV is useful if the future prediction
#' task would be to predict growth curves for new rats, or if we are interested
#' in primarily assessing hierarchical part of the model.
#'
#' In theory, PSIS could be used to also approximate LOGO-CV. 
#' However, in hierarchical models, each group has its own set of
#' parameters and the posterior of those parameters tend to change a
#' lot if all observations in that group are removed, which likely leads to
#' failure of importance sampling. For certain models, quadrature
#' methods could be used to compute integrated (marginalized)
#' importance sampling [@Merkle+Furr+Rabe-Hesketh:2018].
#' 
#' The helper function `kfold_split_grouped` can be used to form a grouped
#' division. With K=30 we thus perform leave-one-rat-out CV. With K=10 we get
#' faster computation by leaving out 3 rats at a time, but the results
#' are likely to be similar to K=30.
cv10gfolds <- kfold_split_grouped(K = 10, x = dfrats$rat)
cv30gfolds <- kfold_split_grouped(K = 30, x = dfrats$rat)
#' Let's illustrate the first of the 30 folds:
prg <- pr +
    geom_point(data=dfrats[cv30gfolds==1,], color="red", size=5, shape=1) +
    ggtitle('Leave-one-rat-out')
prg

#' We use the `kfold` function for K-fold cross-validation.
cv10g_1 <- rstanarm::kfold(fit_1, K=10, folds = cv10gfolds)
cv10g_2 <- rstanarm::kfold(fit_2, K=10, folds = cv10gfolds)
cv10g_3 <- rstanarm::kfold(fit_3, K=10, folds = cv10gfolds)
cv30g_1 <- rstanarm::kfold(fit_1, K=30, folds = cv30gfolds)
cv30g_2 <- rstanarm::kfold(fit_2, K=30, folds = cv30gfolds)
cv30g_3 <- rstanarm::kfold(fit_3, K=30, folds = cv30gfolds)

#' Compare models
loo_compare(cv10g_1, cv10g_2, cv10g_3)
loo_compare(cv30g_1, cv30g_2, cv30g_3)
#' The results are very different than those obtained by LOO. The order of the
#' models is the same, but the differences are much smaller. As there
#' is no rat-specific covariate information, there is not much
#' difference between predicting with the population curve and a normal
#' response distribution with large scale (`fit_1`) or predicting with 
#' uncertain individual curves and and a normal response distribution with 
#' small scale (`fit_2` and `fit_3`).
#' 

#' In the above model, The SE of the elpd differences (`se_diff`) was computed 
#' without taking into account the grouping structure. A more accurate 
#' SE estimate could be obtained by firsting computing the group specific elpds:
cvgfix <- function(cv, cvidx) {
    groupwise=numeric();
    K <- length(unique(cvidx))
    for (i in 1:K) { groupwise[i]=sum(cv$pointwise[cvidx==i])}
    cv$pointwise <- cbind(elpd_kfolds=groupwise)
    cv$se_elpd_kfold <- sd(groupwise)*sqrt(K)
    cv$estimates[2] <- cv$se_elpd_kfold
    cv
}
#' Note that, to sum the pointwise elpds for each rat, we use the 30-fold
#' structure even when grouping elpds from 10-fold-CV.
cv10gg_1 <- cvgfix(cv10g_1, cv30gfolds)
cv10gg_2 <- cvgfix(cv10g_2, cv30gfolds)
cv10gg_3 <- cvgfix(cv10g_3, cv30gfolds)
cv30gg_1 <- cvgfix(cv30g_1, cv30gfolds)
cv30gg_2 <- cvgfix(cv30g_2, cv30gfolds)
cv30gg_3 <- cvgfix(cv30g_3, cv30gfolds)

#' Now we are comparing 30 groupwise elpds, instead of 150 pointwise elpds.
loo_compare(cv10gg_1, cv10gg_2, cv10gg_3)
loo_compare(cv30gg_1, cv30gg_2, cv30gg_3)
#' The groupwise computation doesn't change the elpd differences, but changes
#' the corresponding SE, which are now smaller. This implies a bit more accuracy 
#' in the comparison, but the differences are still small.
#' 

#' ## Grouped K-fold for prediction given initial weight
#'
#' We can modify cross-validation to different prediction tasks. If in
#' the future we would like to predict growth curves after we have
#' measured the birth weight, we could leave one rat out except for
#' the first observation.
#' 
#' We can use `kfold_split_grouped` for this purpose and then manually 
#' form an extra group for the initial weight. 
#' Here, We compute results only for K=10.
cv10xfolds <- kfold_split_grouped(K = 10, x = dfrats$rat)
cv10xfolds[dfrats$age==8] <- 11
#' Still, the 30-folds have to be used for the rat specific predictions before
#' performing the actual comparison.
cv30xfolds <- kfold_split_grouped(K = 30, x = dfrats$rat)
cv30xfolds[dfrats$age==8] <- 31
prx <- pr +
    geom_point(data=dfrats[cv10xfolds==1,], color="red", size=5, shape=1) +
    ggtitle('Predict given initial weight')
prx

#' We use the `kfold` function for K-fold cross-validation.
cv10x_1 <- rstanarm::kfold(fit_1, K=10, folds = cv10xfolds)
cv10x_2 <- rstanarm::kfold(fit_2, K=10, folds = cv10xfolds)
cv10x_3 <- rstanarm::kfold(fit_3, K=10, folds = cv10xfolds)

#' Compute groupwise elpds:
cvxxfix <- function(cv, cvidx) {
    groupwise=numeric();
    K <- length(unique(cvidx))-1
    for (i in 1:K) { groupwise[i]=sum(cv$pointwise[cvidx==i])}
    cv$pointwise <- cbind(elpd_kfolds=groupwise)
    cv$se_elpd_kfold <- sd(groupwise)*sqrt(K)
    cv$estimates[2] <- cv$se_elpd_kfold
    cv
}
cv10xx_1 <- cvxxfix(cv10x_1, cv30xfolds)
cv10xx_2 <- cvxxfix(cv10x_2, cv30xfolds)
cv10xx_3 <- cvxxfix(cv10x_3, cv30xfolds)

#' Compare models
loo_compare(cv10xx_1, cv10xx_2, cv10xx_3)
#' The results are different from those obtained by LOO and LOGO. 
#' Still, the order of the models is the same. Model 1 is clearly worse. 
#' Knowing the initial weight, we get quite similar predictive accuracy when
#' using a common slope instead of varying slopes.
#' 

#' # Alternative models for the prediction given initial weight
#' 
#' We may formulate the initial weight approach also in a different way,
#' which then resembles a group-specific covariate approach.
#' 
#' **Create dataframe**
dfrats2 <- with(rats, data.frame(age=x[x>8], age_c=x[x>8]-25.5, weight=y[x>8], rat=rat[x>8],
                                 initweight_c = rep(y[x==8],4)-mean(y[x==8])))

#' ## Models
#'
#' **Simple linear model**
fit2_1 <- stan_glm(weight ~ initweight_c + age_c, data=dfrats2, refresh=0)
#' **Linear model with hierarchical intercept**
fit2_2 <- stan_glmer(weight ~ initweight_c + age_c + (1 | rat), data=dfrats2, refresh=0)
#' **Linear model with hierarchical intercept and slope**
fit2_3 <- stan_glmer(weight ~ initweight_c + age_c + (age_c | rat), data=dfrats2, refresh=0)

#' ## Grouped K-fold for prediction given initial weight
#' 
#' The helper function `kfold_split_grouped` can be used to form a
#' grouped division.
cv10g2folds <- kfold_split_grouped(K = 10, x = dfrats2$rat)
cv30g2folds <- kfold_split_grouped(K = 30, x = dfrats2$rat)

#' We use the `kfold` function for K-fold cross-validation.
cv10g2_1 <- rstanarm::kfold(fit2_1, K=10, folds = cv10g2folds)
cv10g2_2 <- rstanarm::kfold(fit2_2, K=10, folds = cv10g2folds)
cv10g2_3 <- rstanarm::kfold(fit2_3, K=10, folds = cv10g2folds)

#' Compute groupwise elpds:
cv10gg2_1 <- cvgfix(cv10g2_1, cv30g2folds)
cv10gg2_2 <- cvgfix(cv10g2_2, cv30g2folds)
cv10gg2_3 <- cvgfix(cv10g2_3, cv30g2folds)

#' Now we are comparing 30 groupwise elpds, instead of 150 pointwise elpds.
loo_compare(cv10gg2_1, cv10gg2_2, cv10gg2_3)
#' Model 3 is clearly the best. When the model includes the initial
#' weight, adding varying intercepts doesn't improve prediction
#' accuracy, but adding varying slopes does.
#' 

#' # Conclusion
#'
#' In all comparisons shown in this case study, model 3 was best followed
#' by model 2 while model 1 clearly performed worst.
#' However, depending on the particular cross-validation approach,
#' the differences between models varied quite a lot.
#' 
#' Throughout this case study, we have used **rstanarm** for the model fitting.
#' If instead you prefer to use **brms**, the `loo` and `kfold` methods
#' and their primary arguments will continue to work in the same way.
#' 
#'
#' 
#' <br />
#' 
#' # References {.unnumbered}
#' 
#' <div id="refs"></div>
#' 
#' # Licenses {.unnumbered}
#' 
#' * Code &copy; 2019, Aki Vehtari, licensed under BSD-3.
#' * Text &copy; 2019, Aki Vehtari, licensed under CC-BY-NC 4.0.
#' 
#' # Original Computing Environment {.unnumbered}
#' 
sessionInfo()
#' 
#' <br />
