# Examples of Creating Simulated Data

## Overview
This repository contains functions for simulated data, where independent variables (features) are continuous and the dependent variable (outcome or response) is binary (e.g., case-control). These simulated datasets may be used for comparing/contrasting different machine learning algorithms (e.g., comparing performance). The necessary package that must be installed for simulated data is `privateEC`, which has a workhorse function `createSimulation()` that is used for all simulations.

------------------


## Installing `privateEC`

```{r install-privateEC, eval = FALSE}
library(devtools)
# build_vignettes is optional and takes a bit longer to install
install_github("insilico/privateEC", dependencies = TRUE, build_vignettes = TRUE)
library(privateEC)                 # load library
vignette("pEC_nCV_MainEffect")     # see vignette examples
vignette("pEC_nCV_Interactions")
vignette("RealDataExample")
```


------------------


## Usage
The main function (`sim_mixed_fn()`) used in examples within this repository is defined here:

`analysis/scripts/main-effect_plus_interaction-effect_continuous-features_simulation.R`

The following example will generate a dataset with 100 samples and 100 independent variables. The data are balanced with respect to the outcome (`pct.imbalance = 0.5`), and 10% (`pct.signals`) of the independent variables will be functionally associated with the outcome. The strength of main effects can be controlled with `main.bias`, where larger values produce stronger main effects. On the other hand, the strength of interaction effects can be controlled with `interaction.bias`, where smaller values produce stronger interaction effects. Functional independent variables will either have main effect or interaction effect but NOT both; all other independent variables are random Gaussian noise. The proportion of functional independent variables involved in interactions is controlled with `pct.mixed`, which also determines the proportion with main effect (`1 - pct.mixed`). In this example, 5 features will have main effect, 5 will have interaction effects, and 90 will be random noise. These data can be used to assess the performance of a model in classification and/or in detecting functional features.

```{r create-sim}
sim_mixed_fn(seed             = 1234,       # seed for reproducibility
             num.samples      = 100,        # number of samples
             num.variables    = 100,        # number of independent variables
             pct.imbalance    = 0.5,        # proportion positives out of all samples
             pct.signals      = 0.1,        # proportion functionally associated with outcome out of all independent variables
             main.bias        = 0.8,        # strength of main effects (larger value --> stronger effect)
             interaction.bias = 0.4,        # strength of interaction effects (smaller value --> stronger effect)
             pct.mixed        = 0.5,        # proportion interaction effect out of all functional independent variables
             label            = "class") |> # name of outcome or response
             as_tibble()
             
# A tibble: 100 × 101
   simvar1 simvar2 simvar3 simvar4 simvar5 simvar6 simvar7 simvar8 simvar9
     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
 1  -0.209 -1.34   -0.645  -2.23    -0.564    7.37    7.70    8.50    8.87
 2  -0.202 -0.682  -1.35    0.614   -0.624    7.39    7.49    6.82    6.53
 3  -0.116 -0.898   0.337  -1.26     0.601    5.01    4.91    4.88    4.11
 4   1.19  -0.749   1.61    1.45    -0.527    5.19    4.94    4.43    4.68
 5  -0.276  0.516  -1.07    0.476    0.896    7.31    7.14    6.77    5.86
 6  -1.13   0.0841  0.908  -1.16     1.08     6.32    6.28    7.12    6.61
 7   0.474  1.47    0.0368  0.361    0.511    5.64    5.56    6.58    6.40
 8  -0.250 -0.865   0.350  -0.0804  -1.39     7.25    7.36    6.89    6.30
 9   0.223  1.20    0.831   0.662    0.679    7.43    8.57    8.50    9.16
10   0.424  1.35   -1.38    0.693   -0.140    6.42    6.26    4.98    5.83
# ℹ 90 more rows
# ℹ 92 more variables: simvar10 <dbl>, var1 <dbl>, var2 <dbl>,
#   var3 <dbl>, var4 <dbl>, var5 <dbl>, var6 <dbl>, var7 <dbl>,
#   var8 <dbl>, var9 <dbl>, var10 <dbl>, var11 <dbl>, var12 <dbl>,
#   var13 <dbl>, var14 <dbl>, var15 <dbl>, var16 <dbl>, var17 <dbl>,
#   var18 <dbl>, var19 <dbl>, var20 <dbl>, var21 <dbl>, var22 <dbl>,
#   var23 <dbl>, var24 <dbl>, var25 <dbl>, var26 <dbl>, var27 <dbl>, …
# ℹ Use `print(n = ...)` to see more rows
```


------------------


## **HTML Report on Posit Connect Cloud**

Many more detailed examples are given here:

<https://connect.posit.cloud/bdawkins99/content/0191ec46-cb88-f248-3035-743f8cb6ea26>


------------------


## **Shiny App on Posit Connect Cloud**

Create your own downloadable simulated dataset with this Shiny app:

**shinyapps.io (less buggy)**
<https://ew68xk-bryan.shinyapps.io/data_simulation/>

**Posit Connect Cloud (somewhat buggy)**
<https://connect.posit.cloud/bdawkins99/content/0191d42b-e634-a7cf-8f3b-564ffb047d4a>


