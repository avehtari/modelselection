#' Bootstrap iterations for projpred for bodyfat
bootnum <- 100
boot_est <-  boot_se <- matrix(0, ncol = length(pred) + 1, nrow = bootnum,
                               dimnames = list(NULL, c("(Intercept)", pred)))
boot_ss <- matrix(0, ncol=1, nrow=bootnum)
n<-nrow(df)

set.seed(5437854)
for (i in 1:bootnum) {
  data_id <- sample(1:dim(df)[1], replace = T)
   fitb <- stan_glm(formula, data = df[data_id, ], 
                      prior=hs(), QR=TRUE, seed=i, refresh=0)
  fitb_cv <- cv_varsel(fitb, method='forward', cv_method='LOO', nloo=n,
                       verbose = FALSE)
  print(ss <- fitb_cv$varsel$ssize)
  boot_ss[i,] <- ss
  print(fitb_cv$varsel$vind[1:ss])
  projb <- project(fitb_cv, nv = ss, ns = 4000)
  boot_est[i, colnames(as.matrix(projb)[,-(ss+2)])] <- colMeans(as.matrix(projb)[,-(ss+2)])
}
boot_01 <- (boot_est != 0) * 1
boot_inclusion <- apply(boot_01, 2, function(x) sum(x) / length(x) * 100)
