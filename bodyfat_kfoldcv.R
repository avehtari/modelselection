#' K-fold-CV for projpred for bodyfat

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
fitrhs <- stan_glm(formula, data = df, prior=rhs_prior, QR=TRUE, 
                   seed=1513306866, refresh=0)

# NOTE: In contrast to the previous code where `ndraws = 4000` was used in
# project(), the following cv_varsel() call uses `ndraws_pred = 400` by default.
# Of course, `ndraws_pred = 4000` could be used instead, but that would increase
# runtime considerably, because `ndraws_pred = 4000` not only applies to the
# suggested model size, but all model sizes.
fit_cvvs <- cv_varsel(fitrhs, method='forward', cv_method='kfold', K = 20,
                      seed = 1513306866 + 1)
nv <- suggest_size(fit_cvvs,alpha=0.1)
# Currently (projpred v2.1.1), we need to use the internal projpred function
# .tabulate_stats() to obtain the reference model's performance:
rmse_full <- projpred:::.tabulate_stats(fit_cvvs, stats = "rmse")
rmse_full <- rmse_full$value[rmse_full$size == Inf]
# For the submodel, this is the way to go:
smmry <- summary(fit_cvvs, stats = "rmse", nterms_max = nv)
rmse_proj <- smmry$selection$rmse.kfold[smmry$selection$size == nv]
save(nv, rmse_full, rmse_proj, file = "bodyfat_kfoldcv.RData")
