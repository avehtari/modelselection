#' K-fold-CV for projpred for bodyfat

df <- read.table(here("bodyfat.txt"), header = T, sep = ";")
df[,4:19] <- scale(df[,4:19])
# no-one can have 0% body fat
df <- df[df$siri>0,]
df <- as.data.frame(df)
n <- nrow(df)
### There is already a variable called `weight` with almost the same values:
# colnames(df[c("weight_kg")]) <- c("weight")
###

pred <- c("age", "weight", "height", "neck", "chest", "abdomen", "hip", 
          "thigh", "knee", "ankle", "biceps", "forearm", "wrist")
target <- "siri"
formula_chr <- paste("siri~", paste(pred, collapse = "+"))

set.seed(1513306866)
noise <- array(rnorm(87*n), c(n,87))
dfr<-cbind(df,noise=noise)
formula_obj2<-as.formula(
  paste(formula_chr,"+",paste(colnames(dfr[,20:106]), collapse = "+"))
)
p <- 100

p0 <- 5 # prior guess for the number of relevant variables
tau0 <- p0/(p-p0) * 1/sqrt(n)
rhs_prior <- hs(global_scale=tau0)

fitrhs2 <- stan_glm(formula_obj2, data = dfr, prior=rhs_prior, QR=TRUE, 
                    seed=1513306866, refresh=0)

# NOTE: In contrast to the previous code where `ndraws = 4000` was used in
# project(), the following cv_varsel() call uses `ndraws_pred = 400` by default.
# Of course, `ndraws_pred = 4000` could be used instead, but that would increase
# runtime considerably, because `ndraws_pred = 4000` not only applies to the
# suggested model size, but all model sizes.
fit_cvvs2 <- cv_varsel(fitrhs2, method='forward', cv_method='kfold', K = 20,
                       nterms_max=10, seed = 1513306866 + 1, verbose = FALSE)
print(nv2 <- suggest_size(fit_cvvs2, alpha=0.1))
perfs2 <- performances(fit_cvvs2, stats = "rmse", nterms_max = nv2)
rmse_full2 <- unname(perfs2$reference_model["rmse"])
rmse_proj2 <- perfs2$submodels$rmse[perfs2$submodels$size == nv2]
save(nv2, rmse_full2, rmse_proj2, file = "bodyfat_kfoldcv2.RData")
