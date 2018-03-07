## Tutorial on model assesment, selection and inference after selection

Example notebooks in R using [rstanarm](https://cran.r-project.org/package=rstanarm), [rstan](https://cran.r-project.org/package=rstan), [bayesplot](https://cran.r-project.org/package=bayesplot), [loo](https://cran.r-project.org/package=loo), [projpred](https://cran.r-project.org/package=projpred)

### Video

* [Model selection tutorial at StanCon 2018 Asilomar](https://www.youtube.com/watch?v=FUROJM3u5HQ)

### Slides

* [Model selection tutorial slides](modelselection_tutorial_slides.pdf)
* [Regularized horseshoe slides](regularizedhorseshoe_slides.pdf)

### Outline of the tutorial and links to notebooks
* Basics of predictive performance estimation
* When cross-validation is not needed
  * Simple model we trust - [betablockers](https://rawgit.com/avehtari/modelselection_tutorial/master/betablockers.html)
* When cross-validation is useful
  * We don't trust the model - [roaches](https://rawgit.com/avehtari/modelselection_tutorial/master/roaches.html)
  * Complex model with posterior dependencies - [collinear](https://rawgit.com/avehtari/modelselection_tutorial/master/collinear.html)
* On accuracy of cross-validation
* Cross-validation and hierarchical models
* When cross-validation is not enough
  * large number of models - [diabetes](https://rawgit.com/avehtari/modelselection_tutorial/master/diabetes.html)
* loo 2.0 (coming soon)
* Projection predictive model selection
  * [collinear](https://rawgit.com/avehtari/modelselection_tutorial/master/collinear.html)
  * [diabetes](https://rawgit.com/avehtari/modelselection_tutorial/master/diabetes.html)
  * [projpred](https://github.com/stan-dev/projpred)

### Additional demos added after the tutorial

* projpred examples
  * collinearity - [mesquite](https://rawgit.com/avehtari/modelselection_tutorial/master/mesquite.html)
  * random data vs original data - [candy](https://rawgit.com/avehtari/modelselection_tutorial/master/candy.html)
  * stability of projpred - [bodyfat](https://rawgit.com/avehtari/modelselection_tutorial/master/bodyfat.html)
  * [winequality-red](https://rawgit.com/avehtari/modelselection_tutorial/master/winequality-red.html)
* See also [projpred quick start vignette](https://rawgit.com/stan-dev/projpred/master/vignettes/quickstart.html)

### References

* Gelman, A., Hwang, J., and Vehtari, A. (2014). Understanding
  predictive information criteria for Bayesian models. Statistics and
  Computing, 24(6):997–1016.
  [Preprint](http://www.stat.columbia.edu/~gelman/research/published/waic_understand3.pdf)
* Piironen, J. and Vehtari, A. (2016), Comparison of Bayesian
  predictive methods for model selection, Statistics and Computing
  27(3), 711–735. [Online](https://doi.org/10.1007/s11222-016-9649-y)
* Piironen, J., and Vehtari, A. (2017). On the hyperprior choice for
  the global shrinkage parameter in the horseshoe prior. Proceedings
  of the 20th International Conference on Artificial Intelligence and
  Statistics, PMLR 54:905-913.
  [Online](http://proceedings.mlr.press/v54/piironen17a.html)
* Piironen, J., and Vehtari, A. (2017). Sparsity information and
  regularization in the horseshoe and other shrinkage priors. In
  Electronic Journal of Statistics, 11(2):5018-5051.
  [Online](https://projecteuclid.org/euclid.ejs/1513306866)
* Piironen, J., and Vehtari, A. (2018). Iterative supervised principal
  components. Proceedings of the 21th International Conference on
  Artificial Intelligence and Statistics, accepted for
  publication.
  [arXiv preprint arXiv:1710.06229](https://arxiv.org/abs/1710.06229)
* Vehtari, A., Gelman, A., Gabry, J. (2017). Practical Bayesian model
  evaluation using leave-one-out cross-validation and WAIC. Statistics
  and Computing.  27(5):1413–1432. [arXiv
  preprint](http://arxiv.org/abs/1507.04544).
* Vehtari, A., Gelman, A., Gabry, J. (2017). Pareto smoothed
  importance sampling. [arXiv
  preprint](http://arxiv.org/abs/1507.02646).
* Vehtari, A., Mononen, T., Tolvanen, V., and Winther, O. (2016).
  Bayesian leave-one-out cross-validation approximations for Gaussian
  latent variable models. JMLR, 17(103):1–38.
  [Online](http://jmlr.org/papers/v17/14-540.html)
* Vehtari, A. and Ojanen, J.: 2012, A survey of Bayesian predictive
  methods for model assessment, selection and comparison, Statistics
  Surveys 6, 142–228. [Online](https://doi.org/10.1214/12-SS102)
* Yao, Y., Vehtari, A., Simpson, D., and Gelman, A. (2017). Using
  stacking to average Bayesian predictive distributions. In Bayesian
  Analysis, doi:10.1214/17-BA1091,
  [Online](https://projecteuclid.org/euclid.ba/1516093227)
