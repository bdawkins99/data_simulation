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
The main function (`sim_mixed_fn()`) used in examples within this repository is defined here: `analysis/scripts/main-effect_plus_interaction-effect_continuous-features_simulation.R`

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
```


------------------


## **HTML Report on Posit Connect Cloud**

Many more detailed examples are given here:

<https://connect.posit.cloud/bdawkins99/content/0191ec46-cb88-f248-3035-743f8cb6ea26>


------------------


## **Shiny App on Posit Connect Cloud**

Create your own downloadable simulated dataset with this Shiny app:

<https://connect.posit.cloud/bdawkins99/content/0191d42b-e634-a7cf-8f3b-564ffb047d4a>


