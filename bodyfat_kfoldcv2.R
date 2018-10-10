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

set.seed(1513306866)
noise <- array(rnorm(87*n), c(n,87))
dfr<-cbind(df,noise=noise)
formula2<-paste(formula,"+",paste(colnames(dfr[,20:106]), collapse = "+"))
p <- 100

p0 <- 5 # prior guess for the number of relevant variables
tau0 <- p0/(p-p0) * 1/sqrt(n)
rhs_prior <- hs(global_scale=tau0)

set.seed(1513306866)
perm <- sample.int(n)
K <- 20
idx <- ceiling(seq(from = 1, to = n, length.out = K + 1))
bin <- .bincode(perm, breaks = idx, right = FALSE, include.lowest = TRUE)

fitrhs2 <- stan_glm(formula2, data = dfr, prior=rhs_prior, QR=TRUE, 
                    seed=1513306866, refresh=0)
muss <- list()
vsmuss <- list()
vsnvss <- list()
vsnvss2 <- list()
vsnvss3 <- list()
fitcvs <- list()
for (k in 1:K) {
    message("Fitting model ", k, " out of ", K)
    omitted <- which(bin == k)
    fit_k <- update(
        object = fitrhs2,
        data = dfr[-omitted,, drop=FALSE],
        weights = NULL,
        refresh = 0
    )
    muss[[k]] <-
        colMeans(posterior_linpred(fit_k,
                                   newdata = dfr[omitted, , drop = FALSE]))
    fit_cvvs_k <- cv_varsel(fit_k, method='forward', cv_method='LOO',
                            nloo = length(which(bin != k)), nv_max=10,
                            verbose = FALSE)
    fitcvs[[k]] <- fit_cvvs_k
}
for (k in 1:K) {
    omitted <- which(bin == k)
    fit_cvvs_k <- fitcvs[[k]]
    print(nvk <- suggest_size(fit_cvvs_k, alpha=0.1))
    vsnvss[[k]] <- nvk
    fit_cvvs_k$vind[1:nvk]
    proj_k <- project(fit_cvvs_k, nv = nvk, ns = 4000)
    vsmuss[[k]] <-
        colMeans(proj_linpred(proj_k, xnew = dfr[omitted, , drop = FALSE]))
}
mus<-unlist(muss)[order(as.integer(names(unlist(muss))))]
vsmus<-unlist(vsmuss)[order(as.integer(names(unlist(vsmuss))))]
vsnvs2 <- unlist(vsnvss)
rmse_full2 <- sqrt(mean((df$siri-mus)^2))
(rmse_proj2 <- sqrt(mean((df$siri-vsmus)^2)))
save(vsnvs2, rmse_full2, rmse_proj2, file = "bodyfat_kfoldcv2.RData")
