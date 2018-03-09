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

p0 <- 5 # prior guess for the number of relevant variables
tau0 <- p0/(p-p0) * 1/sqrt(n)
rhs_prior <- hs(global_scale=tau0)

bootnum <- 100
boot_est <-  boot_se <- matrix(0, ncol = length(pred) + 1, nrow = bootnum,
                               dimnames = list(NULL, c("(Intercept)", pred)))
boot_nvs <- matrix(0, ncol=1, nrow=bootnum)

set.seed(5437854)
for (i in 1:bootnum) {
  data_id <- sample(1:dim(df)[1], replace = T)
   fitb <- stan_glm(formula, data = df[data_id, ], 
                      prior=rhs_prior, QR=TRUE, seed=i, refresh=0)
  fitb_cv <- cv_varsel(fitb, method='forward', cv_method='LOO', nloo=n,
                       verbose = FALSE)
  print(nv <- fitb_cv$varsel$ssize)
  boot_nvs[i,] <- nv
  print(fitb_cv$varsel$vind[1:nv])
  projb <- project(fitb_cv, nv = nv, ns = 4000)
  boot_est[i, colnames(as.matrix(projb)[,-(nv+2)])] <- colMeans(as.matrix(projb)[,-(nv+2)])
}
boot_01 <- (boot_est != 0) * 1
boot_inclusion <- apply(boot_01, 2, function(x) sum(x) / length(x) * 100)
save(boot_01, boot_inclusion, boot_nvs, file = "bodyfat_bootstrap.RData")
