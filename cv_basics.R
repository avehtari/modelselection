#' ---
#' title: "Basics of Bayesian cross-validation for model assessment and selection"
#' author: "Aki Vehtari"
#' date: "2018-08-29"
#' encoding: "UTF-8"
#' ---

#' Code for producing figures in StanCon 2018 Helsinki tutorial
#' 
#' -------------
#' 

#' **Load packages**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library(dplyr)
library(tidyr)
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans", base_size=16))
library(brms)
library(loo)

#' # Simple fake data
#' 
#' **Simulate fake data**
x <- 1:20
n <- length(x)
a <- 0.2
b <- 0.3
sigma <- 1
# set the random seed to get reproducible results
# change the seed to experiment with variation due to random noise
set.seed(2141) 
y <- a + b*x + sigma*rnorm(n)
fake <- data.frame(x, y)
# another random seed for second data realisation
set.seed(12241) 
y2 <- a + b*x + sigma*rnorm(n)
fake2 <- data.frame(x, y=y2)
# data with random x
set.seed(2141) 
x <- rnorm(20, 10, 5)
y <- a + b*x + sigma*rnorm(n)
faker <- data.frame(x, y)

#' **Fit linear model**
#+ results='hide'
fit_1 <- stan_glm(y ~ x, data = fake, seed=2141)
fit_2 <- stan_glm(y ~ x, data = fake2, seed=2141)
#+
print(fit_1, digits=2)

#' **Compute exact leave-one-out cross-validation for i=18**
fit_1loo <- stan_glm(y ~ x, data = fake[-18,], seed=2141)
fit_2loo <- stan_glm(y ~ x, data = fake2[-18,], seed=2141)

#' **Extract posterior draws**
sims <- as.matrix(fit_1)
sims2 <- as.matrix(fit_2)
simsloo <- as.matrix(fit_1loo)
sims2loo <- as.matrix(fit_2loo)
n_sims <- nrow(sims)

#' **Compute conditional distribution given x=18**
cond<-data.frame(y=seq(0,9,length.out=100))
cond$x <- dnorm(cond$y, a + b*18, sigma)*6+18

#' **Compute posterior predictive distribution given x=18**
condpred<-data.frame(y=seq(0,9,length.out=100))
condpred$x <- sapply(condpred$y, FUN=function(y) mean(dnorm(y, sims[,1] + sims[,2]*18, sims[,3])*6+18))

#' **Compute posterior predictive distribution given x=18 for second data set**
condpred2<-data.frame(y=seq(0,10,length.out=100))
condpred2$x <- sapply(condpred2$y, FUN=function(y) mean(dnorm(y, sims2[,1] + sims2[,2]*18, sims2[,3])*6+18))

#' **Compute LOO posterior predictive distribution given x=18**
condpredloo<-data.frame(y=seq(0,9,length.out=100))
condpredloo$x <- sapply(condpredloo$y, FUN=function(y) mean(dnorm(y, simsloo[,1] + simsloo[,2]*18, simsloo[,3])*6+18))

#' **Compute LOO posterior predictive distribution given x=18 for second data set**
condpred2loo<-data.frame(y=seq(0,10,length.out=100))
condpred2loo$x <- sapply(condpred2loo$y, FUN=function(y) mean(dnorm(y, sims2loo[,1] + sims2loo[,2]*18, sims2loo[,3])*6+18))

#' **Plot mean of data generating mechanism**
p1 <- ggplot(fake, aes(x = x, y = y)) +
    geom_abline(intercept=a, slope=b) +
    xlim(0,21) + ylim(0,9) +
    labs(title = "True mean y = a + bx") 
p1
ggsave('figs/fake1.pdf', p1, width=6, height=4)

#' **Plot data generating mechanism**
p2 <- p1 +
    geom_path(data=cond,aes(x=x,y=y)) + geom_vline(xintercept=18, linetype=2) +
    labs(title = "True mean and sigma") 
p2
ggsave('figs/fake2.pdf', p2, width=6, height=4)

#' **Plot one realisation from the data generating mechanism**
p3 <- p2 +
  geom_point(color = "white", size = 3) +
  geom_point(color = "black", size = 2) +
  labs(title = "Data") 
p3
ggsave('figs/fake3.pdf', p3, width=6, height=4)

#' **Plot posterior mean**
p4 <- p3 +
  geom_abline(
    intercept = mean(sims[, 1]),
    slope = mean(sims[, 2]),
    size = 1,
    color = "red"
  ) +
  labs(title = "Posterior mean") 
p4
ggsave('figs/fake4.pdf', p4, width=6, height=4)

#' **Plot posterior mean given second data set**
p4b <- p2 +
  geom_point(data=fake2, color = "white", size = 3) +
  geom_point(data=fake2, color = "black", size = 2) +
  geom_abline(
    intercept = mean(sims2[, 1]),
    slope = mean(sims2[, 2]),
    size = 1,
    color = "red"
  ) +
  labs(title = "Posterior mean, alternative data realisation") 
p4b
ggsave('figs/fake4b.pdf', p4b, width=6, height=4)

#' **Plot posterior draws**
p4s <- p4 +
    geom_abline(
        intercept = sims[seq(1,1001,by=10), 1],
        slope = sims[seq(1,1001,by=10), 2],
        size = 0.1,
        color = "blue",
        alpha = 0.1
    ) + ggtitle("Posterior draws")
p4s
ggsave('figs/fake4s.pdf', p4s, width=6, height=4)

#' **Plot posterior predictive distribution**
p5 <- p4 +
    geom_path(data=condpred,aes(x=x,y=y), color="red") +
    geom_vline(xintercept=18, linetype=2, color="red") +
    ggtitle("Posterior predictive distribution")
p5
ggsave('figs/fake5.pdf', p5, width=6, height=4)

#' **Plot posterior predictive distribution given second data set**
p5b <- p4b +
    geom_path(data=condpred2,aes(x=x,y=y), color="red") +
    geom_vline(xintercept=18, linetype=2, color="red") +
    ggtitle("Posterior predictive distribution")
p5b
ggsave('figs/fake5b.pdf', p5b, width=6, height=4)

p5

#' **Plot new data**
p5 + geom_point(data=fake2, color = "black", size = 2, shape=1) +
    ggtitle("New data")
ggsave('figs/fake5n.pdf', width=6, height=4)

#' **Highlight 18th data point**
p6 <- p5 +
    geom_point(data=fake[18,], color = "forestgreen", size = 5, shape=1)
p6
ggsave('figs/fake6.pdf', p6, width=6, height=4)

#' **Plot leave-one-out predictive mean**
p7 <- p6 +
  geom_abline(
    intercept = mean(simsloo[, 1]),
    slope = mean(simsloo[, 2]),
    size = 1,
    color = "forestgreen"
  ) +
    ggtitle("Leave-one-out mean")
p7
ggsave('figs/fake7.pdf', p7, width=6, height=4)

#' **Illustrate leave-one-out residual**
p7 + geom_segment(x=18, y=mean(simsloo[,1] + simsloo[,2]*18),
                  xend=18, yend=fake[18,"y"],
                  color="blue", size=1,
                  arrow = arrow(length = unit(4, "mm"))) +
    ggtitle("Leave-one-out residual") +
    geom_text(x=17,y=6,label=round(fake[18,"y"]-mean(simsloo[,1] + simsloo[,2]*18),1),color="blue")
ggsave('figs/fake7r.pdf', width=6, height=4)

#' **Plot leave-one-out predictive distribution**
p8 <- p7 +
    geom_path(data=condpredloo,aes(x=x,y=y), color="forestgreen") +
    geom_vline(xintercept=18, linetype=2, color="forestgreen") +
    ggtitle("Leave-one-out predictive distribution")
p8
ggsave('figs/fake8.pdf', p8, width=6, height=4)

#' **Illustrate posterior predictive density given 18th data point**
pd18<-mean(dnorm(fake[18,"y"], sims[,1] + sims[,2]*18, sims[,3]))
p8 + geom_segment(x=18, y=fake[18,"y"], yend=fake[18,"y"],
                  xend=18+pd18*6,
                  color="blue", size=1) +
    ggtitle("Posterior predictive density") +
    geom_text(x=17,y=fake[18,"y"],label=round(pd18,2),color="blue")
ggsave('figs/fake8pd.pdf', width=6, height=4)

#' **Illustrate leave-one-out predictive density given 18th data point**
pd18<-mean(dnorm(fake[18,"y"], simsloo[,1] + simsloo[,2]*18, simsloo[,3]))
p8 + geom_segment(x=18, y=fake[18,"y"], yend=fake[18,"y"],
                  xend=mean(dnorm(fake[18,"y"], simsloo[,1] + simsloo[,2]*18, simsloo[,3])*6+18),
                  color="blue", size=1) +
    ggtitle("Leave-one-out predictive density") +
    geom_text(x=17,y=fake[18,"y"],label=round(pd18,2),color="blue")
ggsave('figs/fake8loopd.pdf', width=6, height=4)

#' **Compute PSIS-LOO**
loo_1 <- loo(fit_1, save_psis=TRUE)
round(exp(loo_1$pointwise[,1]),2)

#' **Leave-one-out predictive densities**
p8 + geom_text(data=fake[seq(1,20,by=2),], aes(x=x, y=rep(8.5,10)), label=signif(exp(loo_1$pointwise[seq(1,20,by=2),1]),1),
               color="blue") +
    geom_segment(data=fake[seq(1,20,by=2),], aes(x=x, y=y, xend=x, yend=rep(8.5,10)),
                 linetype=3, size=0.2) +
    geom_text(data=fake[seq(2,20,by=2),], aes(x=x, y=rep(9,10)), label=signif(exp(loo_1$pointwise[seq(2,20,by=2),1]),1),
               color="blue") +
    geom_segment(data=fake[seq(2,20,by=2),], aes(x=x, y=y, xend=x, yend=rep(9,10)),
                 linetype=3, size=0.2) +
    ggtitle("Leave-one-out predictive densities")
ggsave('figs/fake8loopds.pdf', width=6, height=4)

#' **Leave-one-out log predictive densities**
p8 + geom_text(data=fake[seq(1,20,by=2),], aes(x=x, y=rep(8.5,10)), label=signif((loo_1$pointwise[seq(1,20,by=2),1]),2),
               color="blue") +
    geom_segment(data=fake[seq(1,20,by=2),], aes(x=x, y=y, xend=x, yend=rep(8.5,10)),
                 linetype=3, size=0.2) +
    geom_text(data=fake[seq(2,20,by=2),], aes(x=x, y=rep(9,10)), label=signif((loo_1$pointwise[seq(2,20,by=2),1]),2),
               color="blue") +
    geom_segment(data=fake[seq(2,20,by=2),], aes(x=x, y=y, xend=x, yend=rep(9,10)),
                 linetype=3, size=0.2) +
    ggtitle("Leave-one-out log predictive densities")
ggsave('figs/fake8loolpds.pdf', width=6, height=4)

#' **Leave-one-out log predictive densities**
p8elpds <- p8 + geom_text(data=fake[seq(1,20,by=2),], aes(x=x, y=rep(8.6,10)), label=signif((loo_1$pointwise[seq(1,20,by=2),1]),2),
               color="blue") +
    geom_segment(data=fake[seq(1,20,by=2),], aes(x=x, y=y, xend=x, yend=rep(8.6,10)),
                 linetype=3, size=0.2) +
    geom_text(data=fake[seq(2,20,by=2),], aes(x=x, y=rep(8.9,10)), label=signif((loo_1$pointwise[seq(2,20,by=2),1]),2),
               color="blue") +
    geom_segment(data=fake[seq(2,20,by=2),], aes(x=x, y=y, xend=x, yend=rep(8.9,10)),
                 linetype=3, size=0.2) +
    ggtitle("Leave-one-out log predictive densities")
p8elpds

#' **Highlight 18th data point in second data set**
p6b <- p5b +
    geom_point(data=fake2[18,], color = "forestgreen", size = 5, shape=1)
p6b

#' **Plot leave-one-out predictive mean for second data set**
p7b <- p6b +
  geom_abline(
    intercept = mean(sims2loo[, 1]),
    slope = mean(sims2loo[, 2]),
    size = 1,
    color = "forestgreen"
  ) +
    ggtitle("Leave-one-out mean")
p7b

#' **Plot leave-one-out predictive distribution for second data set**
p8b <- p7b +
    geom_path(data=condpred2loo,aes(x=x,y=y), color="forestgreen") + geom_vline(xintercept=18, linetype=2, color="forestgreen") +
    ggtitle("Leave-one-out predictive distribution")
p8b

#' **Plot data**
pd <- ggplot(fake, aes(x = x, y = y)) +
    xlim(0,21) + ylim(0,9) +
  geom_point(color = "white", size = 3) +
    geom_point(color = "black", size = 2) +
    ggtitle("Data")
pd
ggsave('figs/fakedata.pdf', pd, width=6, height=4)

#' **Plot posterior draws**
pdl <- pd + 
    geom_abline(
        intercept = sims[seq(1,4000,by=10), 1],
        slope = sims[seq(1,4000,by=10), 2],
        size = 0.1,
        color = "blue",
        alpha = 0.1
    )  + ggtitle("Posterior draws")
pdl
ggsave('figs/fakedraws.pdf', pdl, width=6, height=4)

#' **Add posterior predictive distribution**
pdl2 <- pdl + geom_path(data=condpred,aes(x=x,y=y), color="red") +
    geom_vline(xintercept=18, linetype=2, color="red") +
    ggtitle("Posterior predictive distribution")
pdl2  
ggsave('figs/fakepostpred.pdf', pdl2, width=6, height=4)

#' **Highlight 18th data point**
pdl3 <- pdl2 + geom_point(data=fake[18,], color = "forestgreen", size = 5, shape=1)
pdl3
ggsave('figs/fakepostpred18.pdf', pdl3, width=6, height=4)

#' **Plot PSIS-LOO weighted draws**
q<-exp(loo_1$psis_object$log_weights[seq(1,4000,by=10),18])
q<-q/max(q)
p4sl <- pd +
    geom_abline(
        intercept = sims[seq(1,4000,by=10), 1],
        slope = sims[seq(1,4000,by=10), 2],
        size = 0.1,
        color = "blue",
        alpha = q
    ) +
    geom_point(data=fake[18,], color = "forestgreen", size = 5, shape=1) +
    ggtitle("PSIS-LOO weighted draws")
p4sl
ggsave('figs/fakepsisdraws.pdf', p4sl, width=6, height=4)

#' **Plot LOO predictive distribution**
p4sl2 <- p4sl + geom_path(data=condpred,aes(x=x,y=y), color="red") +
    geom_vline(xintercept=18, linetype=2, color="red") +
    geom_path(data=condpredloo,aes(x=x,y=y), color="forestgreen") +
    geom_vline(xintercept=18, linetype=2, color="forestgreen") +
    ggtitle("PSIS-LOO weighted predictive distribution")
p4sl2
ggsave('figs/fakepsispostpred.pdf', p4sl2, width=6, height=4)

#' **Plot 400 PSIS-LOO weights**
q<-exp(loo_1$psis_object$log_weights[seq(1,4000,by=10),18]);
q<-q/sum(q)
pw <- mcmc_hist(data.frame(weight=q),  binwidth=1/4000) +
    labs(title="400 importance weights for leave-18th-out", x="w_i") +
    geom_vline(xintercept=1/400, linetype=2, color="red") +
    geom_text(x=1/400, y=60, hjust="left", label=" Equal weights", fontface="plain", size=5)
pw 
ggsave('figs/fakepsisweights.pdf', pw, width=6, height=4)

#' **Plot 4000 PSIS-LOO weights**
q<-exp(loo_1$psis_object$log_weights[seq(1,4000,by=1),18]);
q<-q/sum(q)
pw <- mcmc_hist(data.frame(weight=q),  binwidth=1/40000) +
    labs(title="4000 importance weights for leave-18th-out", x="w_i") +
    geom_vline(xintercept=1/4000, linetype=2, color="red") +
    geom_text(x=1/4000, y=550, hjust="left", label=" Equal weights", fontface="plain", size=5)
pw 
ggsave('figs/fakepsisweights4000.pdf', pw, width=6, height=4)

#' **Plot PSIS-LOO Pareto-k diagnostics**
pkdf<-data.frame(pk=loo_1$diagnostics$pareto_k,
                 neff=loo_1$diagnostics$n_eff,
                 n=1:20)
ggplot(pkdf, aes(x=n,y=pk)) + geom_point(shape=3, color="blue" ,size=3) +
    labs(x="Observation left out", y="Pareto k") +
    geom_hline(yintercept = 0, linetype=3, color="red", size=0.2) +
    geom_hline(yintercept = 0.5, linetype=4, color="red", size=0.2) +
    geom_hline(yintercept = 0.7, linetype=2, color="red", size=0.2) +
    scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.7), limits=c(-0.1,0.7)) +
    ggtitle("PSIS-LOO diagnostics")
ggsave('figs/fakepks.pdf', width=6, height=4)

#' **Plot PSIS-LOO n_effs'**
ggplot(pkdf, aes(x=n,y=neff)) + geom_point(shape=3, color="blue" ,size=3) +
    labs(x="Observation left out", y="n_eff") +
    geom_hline(yintercept = 0, linetype=1, size=0.2) +
    geom_hline(yintercept = 4000, linetype=2, size=0.2) +
    geom_hline(yintercept = 400, linetype=3, color="red", size=0.2) +
    ggtitle("PSIS-LOO diagnostics")
ggsave('figs/fakeneffs.pdf', width=6, height=4)


#' **Illustration of fixed/design x**
pd <- ggplot(fake, aes(x = x, y = y)) +
    xlim(0,21) + ylim(0,9) +
    geom_point(color = "white", size = 3) +
    geom_point(color = "black", size = 2) +
    geom_vline(xintercept=1:20, linetype=3) +
    labs(title = "Fixed / designed x")
pd
ggsave('figs/fakedfixed.pdf', pd, width=6, height=4)

#' **Illustration of x with distribution**
xd<-data.frame(x=seq(0, 21, length.out=100))
xd$y=dnorm(xd$x,10,5)*20
pd <- ggplot(faker, aes(x = x, y = y)) +
    xlim(0,21) + ylim(0,9) +
    geom_point(color = "white", size = 3) +
    geom_point(color = "black", size = 2) +
    geom_vline(xintercept=x, linetype=3) +
    labs(title = "Distribution for x") +
    geom_path(data=xd, aes(x=x,y=y))
pd
ggsave('figs/fakedrandom.pdf', pd, width=6, height=4)

#' # Lake Huron
N <- length(LakeHuron)

#' **Plot data with no year**
lake<-data.frame(x=1:N,y=LakeHuron)
pl <- ggplot(lake[1:50,], aes(x = x, y = y)) +
    geom_point(color = "black", size = 3) +
    ggtitle(" ")
pl
ggsave('figs/lake1data.pdf', pl, width=6, height=4)

#' **Fit gp-spline model**
lake$ycen<-(lake$y-mean(lake$y))/sd(lake$y)
lake$xcen<-(lake$x-mean(lake$x))/sd(lake$x)
fitl <- stan_gamm4(ycen ~ xcen + s(xcen, bs="gp"), data=lake[1:50,])
predl <- as.data.frame(posterior_linpred(fitl, draws=100))
predl$iter <- as.numeric(row.names(predl))
predl<-mutate(gather(as.data.frame(predl), "x", "y", -iter),
              x=as.numeric(x),
              y=y*sd(lake$y)+mean(lake$y))

#' **Plot nonlinear model fit**
pl <- ggplot(lake[1:50,], aes(x=x, y=y)) +
    geom_point(color = "black", size = 3) +
    geom_line(data=predl, aes(x=x, y=y, group=iter), size=0.1, alpha=0.3,
              color="blue") +
    ggtitle("Nonlinear model fit")
pl
ggsave('figs/lake1gp.pdf', pl, width=6, height=4)

#' **Plot gp-spline model + test data**
pl + geom_point(data=lake[51:80,], color = "black", size = 3, shape=1) +
    ggtitle("Nonlinear model fit + new data")
ggsave('figs/lake1gpptest.pdf', width=6, height=4)

#' **Plot with years**
lake<-data.frame(x=1875:1972,y=LakeHuron)
pl <- ggplot(lake, aes(x = x, y = y)) +
    geom_point(color = "black", size = 3) +
    labs(y="Level (feet)", x="Year")    
pl
ggsave('figs/lake2data.pdf', pl, width=6, height=4)

#' **Illustrate LOO for time serie**
pls <- list()
for (i in 1:4) {
    pls[[i]] <- ggplot(lake[-(50+i),], aes(x = x, y = y)) +
        geom_point(color = "black", size = 1) +
        geom_point(data=lake[50+i,], size = 3, shape=1) +
        xlim(1875,1943) +
        labs(y=NULL, x=NULL)
    if (i>=3)
        pls[[i]] <- pls[[i]] + xlab("Year")
    if ((i %% 2) ==1)
        pls[[i]] <- pls[[i]] + ylab("Level (feet)")
}
bp<-bayesplot_grid(plots=pls, save_gg_objects=TRUE)
bp
ggsave('figs/lake3loo.pdf', bp, width=6, height=4)

#' **Illustrate balanced kfold approximation of LOO**
pl <- ggplot(lake[1:59,][-seq(1,59,by=10),], aes(x = x, y = y)) +
    geom_point(color = "black", size = 2) +
    geom_point(data=lake[seq(1,59,by=10),], color = "black", size = 5, shape=1) +
    labs(y="Level (feet)", x="Year", title="Balance k-fold approximation of LOO")    
pl2 <- ggplot(lake[1:59,][-seq(2,59,by=10),], aes(x = x, y = y)) +
    geom_point(color = "black", size = 2) +
    geom_point(data=lake[seq(2,59,by=10),], color = "black", size = 5, shape=1) +
    labs(y="Level (feet)", x="Year", title="Balance k-fold approximation of LOO")    
bp<-bayesplot_grid(pl, pl2)
pl
pl2
ggsave('figs/lake3kfoldbal.pdf', bp, width=6, height=4)
ggsave('figs/lake3kfoldbal1.pdf', pl, width=6, height=4)
ggsave('figs/lake3kfoldbal2.pdf', pl2, width=6, height=4)

#' **Ilustrate random kfold approximation of LOO**
set.seed(12345) 
ii <- sample(1:59,6)
pl <- ggplot(lake[1:59,][-ii,], aes(x = x, y = y)) +
    geom_point(color = "black", size = 2) +
    geom_point(data=lake[ii,], color = "black", size = 5, shape=1) +
    labs(y="Level (feet)", x="Year", title="Random k-fold approximation of LOO")    
pl
ggsave('figs/lake3kfoldrand.pdf', width=6, height=4)

#' **Illustrate 1-step-ahead cross-validation**
pls <- list()
for (i in 1:4) {
    pls[[i]] <- ggplot(lake[1:(49+i),], aes(x = x, y = y)) +
        geom_point(color = "black", size = 1) +
        geom_point(data=lake[50+i,], size = 3, shape=1) +
        xlim(1875,1943) +
        labs(y=NULL, x=NULL)
    if (i>=3)
        pls[[i]] <- pls[[i]] + xlab("Year")
    if ((i %% 2) ==1)
        pls[[i]] <- pls[[i]] + ylab("Level (feet)")
}
bp<-bayesplot_grid(plots=pls)
ggsave('figs/lake3stepahead.pdf', bp, width=6, height=4)

#' **Illustrate 10-step-ahead cross-validation**
pls <- list()
for (i in 1:4) {
    pls[[i]] <- ggplot(lake[1:(49+i),], aes(x = x, y = y)) +
        geom_point(color = "black", size = 1) +
        geom_point(data=lake[i+(50:59),], size = 3, shape=1) +
        xlim(1875,1943) +
        labs(y=NULL, x=NULL)
    if (i>=3)
        pls[[i]] <- pls[[i]] + xlab("Year")
    if ((i %% 2) ==1)
        pls[[i]] <- pls[[i]] + ylab("Level (feet)")
}
bp<-bayesplot_grid(plots=pls)
ggsave('figs/lake3tenstepahead.pdf', bp, width=6, height=4)

#' **Illustrate 1-step-ahead with remove a block cross-validation**
pls <- list()
for (i in 1:4) {
    pls[[i]] <- ggplot(lake[-(i+(50:59)),], aes(x = x, y = y)) +
        geom_point(color = "black", size = 1) +
        geom_point(data=lake[i+50,], size = 3, shape=1) +
        xlim(1875,1943) +
        labs(y=NULL, x=NULL)
    if (i>=3)
        pls[[i]] <- pls[[i]] + xlab("Year")
    if ((i %% 2) ==1)
        pls[[i]] <- pls[[i]] + ylab("Level (feet)")
}
bp<-bayesplot_grid(plots=pls)
ggsave('figs/lake3stepaheadblock.pdf', bp, width=6, height=4)

#' **Illustrate Marginal likelihood / Bayes factor**
pls <- list()
for (i in 1:4) {
    pls[[i]] <- ggplot(lake[0:(i-1),], aes(x = x, y = y)) +
        geom_point(color = "black", size = 1) +
        geom_point(data=lake[i,], size = 3, shape=1) +
        xlim(1875,1943) + ylim(579.5, 582.5) +
        labs(y=NULL, x=NULL)
    if (i>=3)
        pls[[i]] <- pls[[i]] + xlab("Year")
    if ((i %% 2) ==1)
        pls[[i]] <- pls[[i]] + ylab("Level (feet)")
}
bp<-bayesplot_grid(plots=pls)
ggsave('figs/lake3bf.pdf', bp, width=6, height=4)

#' ## PSIS for m-step

#' **Plot whole data**
N <- length(LakeHuron)
df <- data.frame(y = as.numeric(LakeHuron), year=1875:1972, time=1:N)
ggplot(df, aes(x=year, y=y)) + geom_point() +
    labs(y="Level (feet)", x="Year", title="Data")
ggsave('figs/lake4data.pdf', width=6, height=4)

#' **Fit AR-2 model**
control <- list(adapt_delta = 0.95)
SEED<-1234
#+ results='hide'
fit <- brm(y ~ 1, data = df, autocor = cor_ar(~time, p = 2), 
           control = control, seed = SEED)
pred <- cbind(df, predict(fit))

#' **AR-2 prediction with 95% interval**
ggplot(pred, aes(year, Estimate)) +
  geom_smooth(
    aes(ymin = Q2.5, ymax = Q97.5),
    stat = "identity"
  ) +
  geom_point(aes(year, y), inherit.aes = FALSE) +
  labs(x="Year", title = "AR-2 prediction with 95% interval")
ggsave('figs/lake4pred.pdf', width=6, height=4)


#' **PSIS-m-step-ahead**
L<-20
approx_elpds_1sap_no_refit <- rep(NA, nrow(df))
loglik <- log_lik(fit)
logr <- matrix(nrow = nsamples(fit), ncol = nrow(df))
for (i in N:(L + 1)) {
  logr[, i] <- - rowSums(loglik[, i:N, drop = FALSE])
  psis_part <- suppressWarnings(psis(logr[, i:N]))
  w_i <- exp(psis_part$log_weights[, 1])
  approx_elpds_1sap_no_refit[i] <- 
    log(sum(exp(loglik[, i]) * w_i) / sum(w_i))
}

#' **Plot PSIS-m-step-ahead**
is_na <- apply(logr, 2, anyNA)
ps <- psis(logr[, !is_na])
pkdf <- data.frame(pk <- ps$diagnostics$pareto_k, n= ((L+1):N)+1875)
ggplot(pkdf, aes(x=n,y=pk)) + geom_point(shape=3, color="blue") +
    labs(x="Year", y="Pareto k", title="PSIS-1-step-ahead") +
    geom_hline(yintercept = 0, linetype=3, color="red", size=0.2) +
    geom_hline(yintercept = 0.5, linetype=4, color="red", size=0.2) +
    geom_hline(yintercept = 0.7, linetype=2, color="red", size=0.2) +
    geom_hline(yintercept = 1, color="red", size=0.2) +
    ylim(-0.3,1.5)
ggsave('figs/lake4psismstep.pdf', width=6, height=4)

#' **PSIS-m-step-ahead with refits**
#+ results='hide'
k_thres<-0.65
log_mean_exp <- function(x) {
  # more stable than log(mean(exp(x)))
  max_x <- max(x)
  max_x + log(sum(exp(x - max_x))) - log(length(x))
}
loglik <- logr <- matrix(nrow = nsamples(fit), ncol = nrow(df))
ks <- approx_elpds_1sap <- rep(NA, nrow(df))
fit_part <- fit
i_refit <- N
refits <- NULL
for (i in N:(L + 1)) {
  loglik[, i] <- log_lik(fit_part)[, i]
  logr[, i] <- - rowSums(loglik[, i:i_refit, drop = FALSE])
  psis_part <- suppressWarnings(psis(logr[, i]))
  ks[i]<-psis_part$diagnostics$pareto_k
  if (any(psis_part$diagnostics$pareto_k > k_thres)) {
    # refit the model based on the first i-1 observations
    i_refit <- i
    refits <- c(refits, i)
    fit_part <- update(
      fit_part, newdata = df[1:(i-1), ], 
      recompile = FALSE, chains = 1, seed = SEED
    )
    loglik[, i] <- log_lik(fit_part, newdata = df[1:i, ])[, i]
    logr[, i] <- - rowSums(loglik[, i:i_refit, drop = FALSE])
    approx_elpds_1sap[i] <- log_mean_exp(loglik[, i])
  } else {
    w_i <- exp(psis_part$log_weights[, 1])
    approx_elpds_1sap[i] <- log(sum(exp(loglik[, i]) * w_i) / sum(w_i))
  }
}

#' **Plot PSIS-m-step-ahead with refits**
is_na <- apply(logr, 2, anyNA)
pkdf<-data.frame(pk=ks[(L+1):N],n= ((L+1):N)+1875)
ggplot(pkdf, aes(x=n,y=pk)) + geom_point(shape=3, color="blue") +
    labs(x="Year", y="Pareto k", title="PSIS-1-step-ahead with refits") +
    geom_hline(yintercept = 0, linetype=3, color="red", size=0.2) +
    geom_hline(yintercept = 0.5, linetype=4, color="red", size=0.2) +
    geom_hline(yintercept = 0.7, linetype=2, color="red", size=0.2) +
    geom_hline(yintercept = 1, color="red", size=0.2) +
    geom_point(data=pkdf[refits-L,], shape=1, size=5, color="blue") +
    ylim(-0.3,1.5)
ggsave('figs/lake4psisrefits.pdf', width=6, height=4)

#' # Rats
#'
#' Load data
sourceToList = function(file){
  source(file, local = TRUE)
  d = mget(ls())
  d$file = NULL
  d
}
#
rats = sourceToList("rats.data.R")
rats = with(rats, list(
  N = N,
  Npts = length(y),
  rat = rep(1:nrow(y), ncol(y)),
  x = rep(x, each = nrow(y)),
  y = as.numeric(y),
  xbar = xbar
))
ratsdf <- with(rats, data.frame(x=x, y=y, rat=rat))

#' **Plot rats data**
pr <- ggplot(data=ratsdf, aes(x=x, y=y)) +
    geom_line(aes(group=rat), color="black", size=0.1) +
    geom_point(color="black", size=2) +
    labs(x="Age", y="Weight", title="Rats data")
pr
ggsave('figs/rats1data.pdf', pr, width=6, height=4)

#' **Leave-one-out?**
pr1 <- pr +
    geom_point(data=ratsdf[69,], color="red", size=5, shape=1) +
    ggtitle('Leave-one-out?')
pr1
ggsave('figs/rats1loo.pdf', pr1, width=6, height=4)

#' **1-step-ahead?**
pr1 <- pr +
    geom_point(data=ratsdf[121:150,], color="red", size=5, shape=1) +
    ggtitle('1-step-ahead?')
pr1
ggsave('figs/rats1step.pdf', pr1, width=6, height=4)

#' **Leave-one-time-point-out?**
pr1 <- pr +
    geom_point(data=ratsdf[61:90,], color="red", size=5, shape=1) +
    ggtitle('Leave-one-time-point-out?')
pr1
ggsave('figs/rats1onetime.pdf', pr1, width=6, height=4)

#' **Leave-one-rat-out?**
pr1 <- pr +
    geom_point(data=ratsdf[seq(9,129,by=30),], color="red", size=5, shape=1) +
    ggtitle('Leave-one-rat-out?')
pr1
ggsave('figs/rats1onerat.pdf', pr1, width=6, height=4)

## pr1 <- pr +
##     geom_point(data=ratsdf[seq(9,129,by=30),], color="red", size=5, shape=1) +
##     ggtitle('Leave-one-rat-out')
## pr1
## ggsave('figs/rats1oneratb.pdf', pr1, width=6, height=4)

#' **Predict given initial weight?**
pr1 <- pr +
    geom_point(data=ratsdf[seq(39,129,by=30),], color="red", size=5, shape=1) +
    ggtitle('Predict given initial weight?')
pr1
ggsave('figs/rats1init.pdf', pr1, width=6, height=4)

#' **Random kfold approximation of LOO?**
pr1 <- pr +
    geom_point(data=ratsdf[sample(1:150,15),], color="red", size=5, shape=1) +
    ggtitle('Random kfold approximation of LOO')
pr1
ggsave('figs/rats1kfoldrand.pdf', pr1, width=6, height=4)


#' # Primate milk
#' 
#' A popular hypothesis has it that primates with larger brains
#' produce more energetic milk, so that brains can grow quickly.
#' 

#' **Load data**
data(milk)
d <- milk[complete.cases(milk),]
d$neocortex <- d$neocortex.perc /100
str(d)

#' **Fit various models**
fit1 <- stan_glm(kcal.per.g ~ 1, data = d, seed = 2030)
fit2 <- update(fit1, formula = kcal.per.g ~ neocortex)
fit3 <- update(fit1, formula = kcal.per.g ~ log(mass))
fit4 <- update(fit1, formula = kcal.per.g ~ neocortex + log(mass))

#' **Compute LOO**
loo1 <- loo(fit1)
loo2 <- loo(fit2)
loo3 <- loo(fit3)
loo4 <- loo(fit4)
lpd_point <- cbind(
  loo1$pointwise[,"elpd_loo"], 
  loo2$pointwise[,"elpd_loo"],
  loo3$pointwise[,"elpd_loo"], 
  loo4$pointwise[,"elpd_loo"]
)

#' **Plot elpd_loo's**
lols <- data.frame(loo1=loo1$pointwise[,1], loo2=loo2$pointwise[,1],
                   loo3=loo3$pointwise[,1], loo4=loo4$pointwise[,1],
                   n=1:17)

pl <- ggplot(lols, aes(x=n)) +
    geom_point(aes(y=loo2), color = "blue", size = 3) +
    geom_point(aes(y=loo4), color = "red", size = 3) +
    labs(x="Observation", y="elpd_loo", title="Pointwise comparison LOO models: Model 1 = blue, Model 2 = red")
pl
ggsave('figs/milkelpdloo.pdf', width=6, height=4)

#' **Plot elpd_loo SE's**
pl + geom_segment(aes(xend=n, y=loo2, yend=loo4), linetype=3)
ggsave('figs/milkelpdloo2.pdf', width=6, height=4)

#' **Plot elpd_diff's**
pl <- ggplot(lols, aes(x=n)) +
    geom_point(aes(y=loo4-loo2), color = "violet", size = 3) +
    geom_hline(yintercept = 0, linetype = 2) +
    labs(x="Observation", y="Pointwise elpd_diff", title="Pointwise comparison LOO models") 
pl
ggsave('figs/milkelpddiff.pdf', width=6, height=4)
