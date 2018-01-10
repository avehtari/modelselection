library("rstan")
library("rstanarm")
library("loo")
library("ggplot2")
library(tidyverse)
options(mc.cores = parallel::detectCores())
options(mc.cores = 1)

Os<-c(1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
betaprobs<-matrix(nrow=100,ncol=10)
looprobs<-matrix(nrow=100,ncol=10)
refprobs<-matrix(nrow=100,ncol=10)
loodiff<-matrix(nrow=100,ncol=10)
loose<-matrix(nrow=100,ncol=10)
for (oi in 1:length(Os)) {
    print(oi)
    for (i1 in 1:100) {
        print(i1)
        theta10 <- 0.06;
        oddsratio0 <- Os[oi];
        theta20 <- 1/(1/(oddsratio0*(theta10/(1-theta10)))+1);
        y1 <- rbinom(1,674,theta10)
        y2 <- rbinom(1,680,theta20)
        d_bin2 <- data.frame(N = c(674, 680), y = c(y1,y2), grp2 = c(0,1))
        output <- capture.output(
            fit_bin2 <- stan_glm(y/N ~ grp2, family = binomial(), data = d_bin2,
                                 seed=1, weights = N, refresh=-1,
                                 open_progress = FALSE))
        output <- capture.output(
            fit_bin2null <- stan_glm(y/N ~ 1, family = binomial(), data = d_bin2,
                                     seed=1, weights = N, refresh=-1,
                                     open_progress = FALSE))
        samples_bin2 <- extract(fit_bin2$stanfit)
        samples_bin2null <- extract(fit_bin2null$stanfit)
        theta1 <- plogis(samples_bin2$alpha)
        theta2 <- plogis(samples_bin2$alpha + samples_bin2$beta)
        ll2<-cbind(as.vector(log(theta1)),as.vector(log(1-theta1)),as.vector(log(theta2)),as.vector(log(1-theta2)))
        ll2null<-cbind(as.vector(log(theta)),as.vector(log(1-theta)),as.vector(log(theta)),as.vector(log(1-theta)))
        ql2<-loo(ll2)
        ql2null<-loo(ll2null)
        qll2<-(c(rep(ql2$pointwise[1,"elpd_loo"],y1),rep(ql2$pointwise[2,"elpd_loo"],674-y1),rep(ql2$pointwise[3,"elpd_loo"],y2),rep(ql2$pointwise[4,"elpd_loo"],680-y2)))
        qll2null<-(c(rep(ql2null$pointwise[1,"elpd_loo"],y1),rep(ql2null$pointwise[2,"elpd_loo"],674-y1),rep(ql2null$pointwise[3,"elpd_loo"],y2),rep(ql2null$pointwise[4,"elpd_loo"],680-y2)))
        theta <- plogis(samples_bin2null$alpha)
        q<-((mean(theta1)*log(mean(theta1))+(1-mean(theta1))*log(1-mean(theta1)))*674+(mean(theta2)*log(mean(theta2))+(1-mean(theta2))*log(1-mean(theta2)))*680)-        ((mean(theta1)*log(mean(theta))+(1-mean(theta1))*log(1-mean(theta)))*674+(mean(theta2)*log(mean(theta))+(1-mean(theta2))*log(1-mean(theta)))*680)
        refprobs[i1,oi]<-exp(q)/(1+exp(q))
        oddsratio <- (theta2/(1-theta2))/(theta1/(1-theta1))
        probs[i1,oi]<- mean(oddsratio<1)
        looprobs[i1,oi]<-pnorm(0,sum((qll2null-qll2)),sd(qll2null-qll2)*sqrt(674+680))
        loodiff[i1,oi]<-sum((qll2null-qll2))
        loose[i1,oi]<-sd(qll2null-qll2)*sqrt(674+680)
    }
}
save(betaprobs, looprobs, refprobs, loodiff, loose, file="binom_test.RData")
write.csv(refprobs,file="refprobs")
write.csv(looprobs,file="looprobs")
write.csv(betaprobs,file="betaprobs")

q<-as.data.frame(betaprobs);
colnames(q)<-c("1","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1")
q<-data.frame(stack(q));q[801:1000,"values"]=1-runif(200,max=0.00001);
q %>% group_by(ind) %>%
  do(ggplot2:::compute_density(.$values, NULL, from=0, to=1, bw="SJ", adjust=1, n=2048)) %>%
  rename(values = x) -> betaprobs_densities
ggplot(betaprobs_densities, aes(x = values, y = ind, height = scaled)) + 
  geom_density_ridges(stat = "identity", scale=0.6)

q<-as.data.frame(looprobs);
colnames(q)<-c("1","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1")
q<-data.frame(stack(q));q[801:1000,"values"]=1-runif(200,max=0.00001);
q %>% group_by(ind) %>%
  do(ggplot2:::compute_density(.$values, NULL, from=0, to=1, bw="SJ", adjust=1, n=2048)) %>%
  rename(values = x) -> looprobs_densities
ggplot(looprobs_densities, aes(x = values, y = ind, height = scaled)) + 
  geom_density_ridges(stat = "identity", scale=0.6)

q<-as.data.frame(refprobs);
colnames(q)<-c("1","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1")
q<-data.frame(stack(q));q[801:1000,"values"]=1-runif(200,max=0.00001);
q %>% group_by(ind) %>%
  do(ggplot2:::compute_density(.$values, NULL, from=0, to=1, bw="SJ", adjust=1, n=2048)) %>%
  rename(values = x) -> refprobs_densities
ggplot(refprobs_densities, aes(x = values, y = ind, height = scaled)) + 
  geom_density_ridges(stat = "identity", scale=0.6)

save(betaprobs_densities, looprobs_densities, refprobs_densities, file="binom_test_densities.RData")
