## Tutorial on model assesment, selection and inference after selection

Example notebooks in R using [rstanarm](https://cran.r-project.org/package=rstanarm), [rstan](https://cran.r-project.org/package=rstan), [bayesplot](https://cran.r-project.org/package=bayesplot), [loo](https://cran.r-project.org/package=loo), [projpred](https://github.com/stan-dev/projpred)

### Outline

[Slides](modelselection_tutorial_slides.pdf)

* Basics of predictive performance estimation
* When cross-validation is not needed
  * Simple model we trust - [betablockers](https://rawgit.com/avehtari/modelselection_tutorial/master/betablockers.html)
* When cross-validation is useful
  * We don't trust the model - [roaches](https://rawgit.com/avehtari/modelselection_tutorial/master/roaches.html)
  * Complex model with posterior dependencies - [colinear](https://rawgit.com/avehtari/modelselection_tutorial/master/colinear.html)
* On accuracy of cross-validation
* Cross-validation and hierarchical models
* When cross-validation is not enough
  * large number of models - [diabetes](https://rawgit.com/avehtari/modelselection_tutorial/master/diabetes.html)
* loo 2.0 (coming soon)
* [projpred](https://github.com/stan-dev/projpred)
