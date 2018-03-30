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

set.seed(5437854)
perm <- sample.int(n)
K <- 20
idx <- ceiling(seq(from = 1, to = n, length.out = K + 1))
bin <- .bincode(perm, breaks = idx, right = FALSE, include.lowest = TRUE)

muss <- list()
vsmuss <- list()
vsnvss <- list()
vsnvss2 <- list()
for (k in 1:K) {
    message("Fitting model ", k, " out of ", K)
    omitted <- which(bin == k)
    fit_k <- update(
        object = fitrhs,
        data = df[-omitted,, drop=FALSE],
        weights = NULL,
        refresh = 0
    )
    fit_cvvs_k <- cv_varsel(fit_k, method='forward', cv_method='LOO',
                            nloo=nrow(df[-omitted,, drop=FALSE]))
    nvk <- suggest_size(fit_cvvs_k,alpha=0.1)
    vsnvss[[k]] <- nvk
    proj_k <- project(fit_cvvs_k, nv = nvk, ns = 4000)
    muss[[k]] <-
        colMeans(posterior_linpred(fit_k,
                                   newdata = df[omitted, , drop = FALSE]))
    vsmuss[[k]] <-
        colMeans(proj_linpred(proj_k, xnew = df[omitted, , drop = FALSE]))
}
mus<-unlist(muss)[order(as.integer(names(unlist(muss))))]
vsmus<-unlist(vsmuss)[order(as.integer(names(unlist(vsmuss))))]
vsnvs <- unlist(vsnvss)
rmse_full <- sqrt(mean((df$siri-mus)^2))
rmse_proj <- sqrt(mean((df$siri-vsmus)^2))
save(vsnvs, rmse_full, rmse_proj, file = "bodyfat_kfoldcv.RData")
