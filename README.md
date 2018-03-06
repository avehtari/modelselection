## Tutorial on model assesment, selection and inference after selection

Example notebooks in R using [rstanarm](https://cran.r-project.org/package=rstanarm), [rstan](https://cran.r-project.org/package=rstan), [bayesplot](https://cran.r-project.org/package=bayesplot), [loo](https://cran.r-project.org/package=loo), [projpred](https://github.com/stan-dev/projpred)

### Slides

* [Model selection tutorial slides](modelselection_tutorial_slides.pdf)
* [Regularized Horseshoe slides](regularizedhorseshoe_slides.pdf)

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
  * stability of projpred [bodyfat](https://rawgit.com/avehtari/modelselection_tutorial/master/bodyfat.html)
  * [winequality-red](https://rawgit.com/avehtari/modelselection_tutorial/master/winequality-red.html)
* See also [projpred quick start vignette](https://rawgit.com/stan-dev/projpred/master/vignettes/quickstart.html)
