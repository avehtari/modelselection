library(here)
library(rstanarm)
options(mc.cores = parallel::detectCores())
library(loo)
library(projpred)
library(dplyr)

#' Bootstrap iterations for projpred for bodyfat
df <- read.table(here("bodyfat.txt"), header = T, sep = ";")
df[,4:19] <- scale(df[,4:19])
df <- as.data.frame(df)
n <- nrow(df)
colnames(df[c("weight_kg", "height")]) <- c("weight", "height")

pred <- c("age", "weight", "height", "neck", "chest", "abdomen", "hip", 
          "thigh", "knee", "ankle", "biceps", "forearm", "wrist")
target <- "siri"
formula <- paste("siri~", paste(pred, collapse = "+"))
p <- length(pred)

p0 <- 2 # prior guess for the number of relevant variables
tau0 <- p0/(p-p0) * 1/sqrt(n)
rhs_prior <- hs(global_scale=tau0)

bootnum <- 100
boot_est <-  boot_se <- matrix(0, ncol = length(pred) + 1, nrow = bootnum,
                               dimnames = list(NULL, c("(Intercept)", pred)))
boot_nvs <- matrix(0, ncol=1, nrow=bootnum)
bbn <- matrix(0, ncol=1, nrow=bootnum)
for (i in 1:bootnum) {
  set.seed(5437854+i)
  data_id <- sample(1:dim(df)[1], replace = T)
  bbn[i,] <- length(unique(data_id))
  fitb <- stan_glm(formula, data = df[data_id, ], 
                      prior=rhs_prior, QR=TRUE, seed=i, refresh=0)
  bcvvs <- cv_varsel(fitb, method='forward', cv_method='LOO', nloo=n,
                       verbose = FALSE)
  print(nv <- suggest_size(bcvvs,alpha=0.1))
  boot_nvs[i,] <- nv
  print(bcvvs$vind[1:nv])
  projb <- project(bcvvs, nv = nv, ns = 4000)
  boot_est[i, colnames(as.matrix(projb)[,-(nv+2)])] <- colMeans(as.matrix(projb)[,-(nv+2)])
}
boot_01 <- (boot_est != 0) * 1
boot_inclusion <- data.frame(projpred_incp=(apply(boot_01, 2, function(x) sum(x) / length(x) * 100)))
boot_01 <- data.frame(boot_01)

bn <- data.frame(boot_nvs) %>% group_by_all() %>% count(sort=TRUE)
bd <- boot_01 %>% group_by_at(vars(-X.Intercept.)) %>% count(sort=TRUE)
boot_inclusion <- boot_inclusion %>% tibble::rownames_to_column(var="variable") %>% filter(variable != "X.Intercept.") %>% arrange(-projpred_incp)
boot_inclusion$steplm_incp <- c(100, 28, 98, 100, 85, 63, 51, 48, 34, 43, 54, 41, 18)
boot_inclusion <- boot_inclusion %>% rename(projpred=projpred_incp, steplm=steplm_incp)
cumsum(bd$n)
for (i in 1:20) {
    print(paste(paste0(colnames(bd)[c(as.logical(bd[i,1:13]),FALSE)], collapse=", "),bd$n[i],sep=", "))
}

save(boot_01, boot_inclusion, boot_nvs, bbn, file = "bodyfat_bootstrap.RData")
