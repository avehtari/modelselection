## Model assesment, selection and inference after selection

Example notebooks in R using [rstanarm](https://cran.r-project.org/package=rstanarm), [rstan](https://cran.r-project.org/package=rstan), [bayesplot](https://cran.r-project.org/package=bayesplot), [loo](https://cran.r-project.org/package=loo), [projpred](https://cran.r-project.org/package=projpred). 

### Talks

* Model assessment, comparison and selection at Master class in Bayesian statistics, CIRM, Marseille
  - [Slides](slides_model_assesment_selection.pdf)
  - [Video](https://library.cirm-math.fr/Record.htm?idlist=157&record=19285597124910037799)
* Model assessmen and model selection aka Basics of cross-validation tutorial at StanCon 2018 Helsinki
  - [Slides](Vehtari_StanHEL_CV.pdf)
  - [Video](https://www.youtube.com/watch?v=hpr8pxqkCH8&t=0s&list=PLuwyh42iHquU4hUBQs20hkBsKSMrp6H0J&index=9)
  - [Code for figures](https://avehtari.github.io/modelselection/cv_basics.html)
* Regularized horseshoe talk at StanCon 2018 Asilomar
  - [Slides](regularizedhorseshoe_slides.pdf)
  - [Video](https://www.youtube.com/watch?v=umk7eOkt5k8)
* Model selection tutorial at StanCon 2018 Asilomar
  - [Slides](modelselection_tutorial_slides.pdf)
  - [Video](https://www.youtube.com/watch?v=FUROJM3u5HQ)

### Outline of the StanCon 2018 Asilomar tutorial and links to notebooks
* Basics of predictive performance estimation
* When cross-validation is not needed
  * Simple model we trust - [betablockers](https://avehtari.github.io/modelselection/betablockers.html)
* When cross-validation is useful
  * We don't trust the model - [roaches](https://avehtari.github.io/modelselection/roaches.html)
  * Complex model with posterior dependencies - [collinear](https://avehtari.github.io/modelselection/collinear.html)
* On accuracy of cross-validation
* Cross-validation and hierarchical models
* When cross-validation is not enough
  * large number of models - [diabetes](https://avehtari.github.io/modelselection/diabetes.html)
* [loo 2.0](http://mc-stan.org/loo/)
* Projection predictive model selection
  * [collinear](https://avehtari.github.io/modelselection/collinear.html)
  * [diabetes](https://avehtari.github.io/modelselection/diabetes.html)
  * [projpred](https://github.com/stan-dev/projpred)

### Additional case studies

* [loo 2.0](http://mc-stan.org/loo/)
  * [Bayesian Stacking and Pseudo-BMA weights using the loo package](http://mc-stan.org/loo/articles/loo2-weights.html)
  * [Leave-one-out cross-validation for non-factorizable models](http://mc-stan.org/loo/articles/loo2-non-factorizable.html)
  * [Approximate leave-future-out cross-validation for time series models](http://mc-stan.org/loo/articles/loo2-lfo.html)
* Projection predictive model selection (projpred) examples
  * collinearity - [mesquite](https://avehtari.github.io/modelselection/mesquite.html)
  * random data vs original data - [candy](https://avehtari.github.io/modelselection/candy.html)
  * [winequality-red](https://avehtari.github.io/modelselection/winequality-red.html)
  * [bodyfat](https://avehtari.github.io/modelselection/bodyfat.html)
  * See also [projpred quick start vignette](https://rawgit.com/stan-dev/projpred/master/vignettes/quickstart.html)
* [LOO-R^2](https://avehtari.github.io/bayes_R2/bayes_R2.html)

### See also
* [When LOO and other cross-validation approaches are valid](http://andrewgelman.com/2018/08/03/loo-cross-validation-approaches-valid/)
* [Parsimonious principle vs integration over all uncertainties](http://andrewgelman.com/2018/07/26/parsimonious-principle-vs-integration-uncertainties/)
* [Comments on Limitations of Bayesian Leave-One-Out Cross-Validation for Model Selection](http://andrewgelman.com/2018/06/05/comments-limitations-bayesian-leave-one-cross-validation-model-selection/)
* [I am the supercargo](http://andrewgelman.com/2018/06/21/i-am-the-supercargo/)
* Nice horseshoe example in [Bayes Sparse Regression case study by Michael Betancourt](https://betanalpha.github.io/assets/case_studies/bayes_sparse_regression.html)

### References

* Bürkner, P.-C., Gabry, J., Vehtari, A. (2018). Leave-one-out
  cross-validation for non-factorizable normal
  models. [arXiv:1810.10559](https://arxiv.org/abs/arXiv:1810.10559)
* Gelman, A., Hwang, J., and Vehtari, A. (2014). Understanding
  predictive information criteria for Bayesian models. Statistics and
  Computing, 24(6):997–1016.
  [Preprint](http://www.stat.columbia.edu/~gelman/research/published/waic_understand3.pdf)
* Piironen, J., Paasiniemi, M., and Vehtari, A. (2018). Projective
  Inference in High-dimensional Problems: Prediction and Feature
  Selection. [arXiv:1810.02406](https://arxiv.org/abs/arXiv:1810.02406)
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
* Williams, D. R., Piironen, J., Vehtari, A., and Rast,
  P. (2018). Bayesian estimation of Gaussian graphical models with
  projection predictive selection. [arXiv:1801.05725](https://arxiv.org/abs/1801.05725)
* Yao, Y., Vehtari, A., Simpson, D., and Gelman, A. (2017). Using
  stacking to average Bayesian predictive distributions. In Bayesian
  Analysis, doi:10.1214/17-BA1091,
  [Online](https://projecteuclid.org/euclid.ba/1516093227)
