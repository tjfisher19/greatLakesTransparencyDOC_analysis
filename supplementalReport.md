# Supplemental Document
Thomas Fisher

<script src="supplementalReport_files/libs/kePrint-0.0.1/kePrint.js"></script>
<link href="supplementalReport_files/libs/lightable-0.0.1/lightable.css" rel="stylesheet" />


- [Caveats](#caveats)
- [Dissolved Organic Carbon
  analysis](#dissolved-organic-carbon-analysis)
  - [Residuals analysis](#residuals-analysis)
  - [Measures of model fit](#measures-of-model-fit)
  - [Model coefficients](#model-coefficients)
  - [Plots of fitted model](#plots-of-fitted-model)
- [1% UV depth analysis](#uv-depth-analysis)
  - [Residual analysis](#residual-analysis)
  - [Measure of model fit](#measure-of-model-fit)
  - [Model coefficients](#model-coefficients-1)
  - [Plots of fitted model](#plots-of-fitted-model-1)
- [1% PAR depth analysis](#par-depth-analysis)
  - [Residual analysis](#residual-analysis-1)
  - [Measures of model fit](#measures-of-model-fit-1)
  - [Model coefficients](#model-coefficients-2)
  - [Plots of the fitted model](#plots-of-the-fitted-model)

## Caveats

A few important things to remember when looking at this report

- We used mixed effect models, so some of the standard regression
  measures and diagnostics are not verbatim the same; *e.g.*, marginal
  and conditional
  ![R^2](https://latex.codecogs.com/svg.latex?R%5E2 "R^2") instead of
  the standard coefficient of determination.
- The response variable in each model has been transformed with a
  base-10 logarithm.
  - So linear models are reported, but for a log-response.
  - In the original units, we effectively are fitting an exponential or
    power function.
- The covariate input, DOC, has also been log-transformed as it is right
  skewed and this helps meet the linearity assumption.
- We fit models with interaction terms, so the main effects can only be
  interpreted with respect to the interaction terms.
- A backward selection algorithm was implemented to choose the *best*
  model using the Bayesian Information Criteria (BIC). This document
  reports that chosen model.

## Dissolved Organic Carbon analysis

The backward selection procedure chose the following model

![\begin{multline}
\log\_{10}(DOC) = \beta_0 + \beta_1\times Year+ \beta_2\times Summer + \beta_3\times Fall + \beta_4\times Non\text{-}embayment +\\
\beta_6\times Summer\times Non\text{-}embayment + \beta_7\times Fall\times Non\text{-}embayment
\end{multline}](https://latex.codecogs.com/svg.latex?%5Cbegin%7Bmultline%7D%0A%5Clog_%7B10%7D%28DOC%29%20%3D%20%5Cbeta_0%20%2B%20%5Cbeta_1%5Ctimes%20Year%2B%20%5Cbeta_2%5Ctimes%20Summer%20%2B%20%5Cbeta_3%5Ctimes%20Fall%20%2B%20%5Cbeta_4%5Ctimes%20Non%5Ctext%7B-%7Dembayment%20%2B%5C%5C%0A%5Cbeta_6%5Ctimes%20Summer%5Ctimes%20Non%5Ctext%7B-%7Dembayment%20%2B%20%5Cbeta_7%5Ctimes%20Fall%5Ctimes%20Non%5Ctext%7B-%7Dembayment%0A%5Cend%7Bmultline%7D "\begin{multline}
\log_{10}(DOC) = \beta_0 + \beta_1\times Year+ \beta_2\times Summer + \beta_3\times Fall + \beta_4\times Non\text{-}embayment +\\
\beta_6\times Summer\times Non\text{-}embayment + \beta_7\times Fall\times Non\text{-}embayment
\end{multline}")

where the
![\beta_0](https://latex.codecogs.com/svg.latex?%5Cbeta_0 "\beta_0")
term is a random effect to account for the variability at each site
(similar to a block term in experimental design). The variable
![Year](https://latex.codecogs.com/svg.latex?Year "Year") measures times
since the beginning of data collection. The variables
![Summer](https://latex.codecogs.com/svg.latex?Summer "Summer"),
![Fall](https://latex.codecogs.com/svg.latex?Fall "Fall"), and
![Non\text{-}embayment](https://latex.codecogs.com/svg.latex?Non%5Ctext%7B-%7Dembayment "Non\text{-}embayment")
are indicator, or dummy, variables indicating the observation occurred
in that season or water type.

### Residuals analysis

First, we take a look at some residual plots before proceeding to look
at the model

![](supplementalReport_files/figure-commonmark/unnamed-chunk-3-1.png)

Nothing overly concerning here.

### Measures of model fit

Some overall model measures

|   N |  df | Resid.SE |       AIC |       BIC | Marginal R2 | Conditional R2 |
|----:|----:|---------:|----------:|----------:|------------:|---------------:|
| 665 | 656 |    0.068 | -1397.356 | -1356.858 |       0.089 |          0.778 |

We note that the fixed effects in the model do not capture much of the
variability in DOC (the marginal
![R^2](https://latex.codecogs.com/svg.latex?R%5E2 "R^2")). This
indicates that the DOC levels are varying quite a bit by the site.

### Model coefficients

We can look at the coefficients for the fixed effects in the fitted
model (or expected value of the random effect)… Recall the response here
is the
![\log\_{10}](https://latex.codecogs.com/svg.latex?%5Clog_%7B10%7D "\log_{10}")
of the 1% PAR Depth (m)

| Term                                | Estimate |    SE | t.stat |      df | p.value |
|:------------------------------------|---------:|------:|-------:|--------:|--------:|
| (Intercept)                         |    0.350 | 0.023 | 15.359 |  77.292 |   0.000 |
| Year                                |    0.002 | 0.000 |  5.181 | 604.348 |   0.000 |
| SeasonSummer                        |   -0.005 | 0.010 | -0.561 | 598.842 |   0.575 |
| SeasonFall                          |   -0.069 | 0.010 | -7.165 | 599.091 |   0.000 |
| WaterBodyNon-embayment              |   -0.099 | 0.032 | -3.045 |  69.135 |   0.003 |
| SeasonSummer:WaterBodyNon-embayment |    0.021 | 0.014 |  1.526 | 598.606 |   0.127 |
| SeasonFall:WaterBodyNon-embayment   |    0.068 | 0.013 |  5.103 | 599.148 |   0.000 |

and the two standard deviation measures associated with the model

| Group    | Term              | Estimate |
|:---------|:------------------|---------:|
| SiteID   | sd\_\_(Intercept) |    0.121 |
| Residual | sd\_\_Observation |    0.068 |

### Plots of fitted model

And a plot of the fitted model (response is log-transformed)

![](supplementalReport_files/figure-commonmark/unnamed-chunk-7-1.png)

And on the original units

![](supplementalReport_files/figure-commonmark/unnamed-chunk-8-1.png)

Note, these are exponential fits, but the predicted values on the
logarithmic scale (a linear model) do not vary all that much, so the
fitted models on the original scale appears almost linear.

## 1% UV depth analysis

Our backward selection procedure chose the following model

![\begin{multline}
\log\_{10}(UV) = \beta_0 + \beta_1\log\_{10}(DOC) + \beta_2\times Summer + \beta_3\times Fall + \beta_4\times Non\text{-}embayment + \beta_5\times River +\\
\beta_6\times Summer\times Non\text{-}embayment + \beta_7\times Fall\times Non\text{-}embayment + \\
\beta_8\log\_{10}(DOC)\times Non\text{-}embayment + \beta_9\times Summer\times River + \beta\_{10}\times Fall\times River
\end{multline}](https://latex.codecogs.com/svg.latex?%5Cbegin%7Bmultline%7D%0A%5Clog_%7B10%7D%28UV%29%20%3D%20%5Cbeta_0%20%2B%20%5Cbeta_1%5Clog_%7B10%7D%28DOC%29%20%2B%20%5Cbeta_2%5Ctimes%20Summer%20%2B%20%5Cbeta_3%5Ctimes%20Fall%20%2B%20%5Cbeta_4%5Ctimes%20Non%5Ctext%7B-%7Dembayment%20%2B%20%5Cbeta_5%5Ctimes%20River%20%2B%5C%5C%0A%5Cbeta_6%5Ctimes%20Summer%5Ctimes%20Non%5Ctext%7B-%7Dembayment%20%2B%20%5Cbeta_7%5Ctimes%20Fall%5Ctimes%20Non%5Ctext%7B-%7Dembayment%20%2B%20%5C%5C%0A%5Cbeta_8%5Clog_%7B10%7D%28DOC%29%5Ctimes%20Non%5Ctext%7B-%7Dembayment%20%2B%20%5Cbeta_9%5Ctimes%20Summer%5Ctimes%20River%20%2B%20%5Cbeta_%7B10%7D%5Ctimes%20Fall%5Ctimes%20River%0A%5Cend%7Bmultline%7D "\begin{multline}
\log_{10}(UV) = \beta_0 + \beta_1\log_{10}(DOC) + \beta_2\times Summer + \beta_3\times Fall + \beta_4\times Non\text{-}embayment + \beta_5\times River +\\
\beta_6\times Summer\times Non\text{-}embayment + \beta_7\times Fall\times Non\text{-}embayment + \\
\beta_8\log_{10}(DOC)\times Non\text{-}embayment + \beta_9\times Summer\times River + \beta_{10}\times Fall\times River
\end{multline}")

where the
![\beta_0](https://latex.codecogs.com/svg.latex?%5Cbeta_0 "\beta_0")
term is a random effect to account for the variability at each site
(similar to a block term in experimental design). The variables
![Summer](https://latex.codecogs.com/svg.latex?Summer "Summer"),
![Fall](https://latex.codecogs.com/svg.latex?Fall "Fall"),
![Non\text{-}embayment](https://latex.codecogs.com/svg.latex?Non%5Ctext%7B-%7Dembayment "Non\text{-}embayment")
and ![River](https://latex.codecogs.com/svg.latex?River "River") would
all be indicator, or dummy, variables indicating the observation
occurred in that season, water type or in the presence of a river.

### Residual analysis

First, we take a look at some residual plots before proceeding to look
at the model output

![](supplementalReport_files/figure-commonmark/unnamed-chunk-10-1.png)

Nothing overly concerning here. Some minor deviation from Normality but
regression methods are robust to some minor deviations from normality.

### Measure of model fit

Some overall model measures

|   N |  df | Resid.SE |      AIC |      BIC | Marginal R2 | Conditional R2 |
|----:|----:|---------:|---------:|---------:|------------:|---------------:|
| 631 | 618 |    0.113 | -710.842 | -653.027 |        0.58 |          0.848 |

Here, the fixed effects (which include DOC) explain about 57% of the
variability. After including the site level variation, the overall model
explains about 84% of the variability in the log of 1% UV depth.

### Model coefficients

The fixed effects in the fitted model (or expected value of the random
effect)… Recall the response here is the
![\log\_{10}](https://latex.codecogs.com/svg.latex?%5Clog_%7B10%7D "\log_{10}")
of the 1% UV Depth (m)

| Term                                | Estimate |    SE |  t.stat |      df | p.value |
|:------------------------------------|---------:|------:|--------:|--------:|--------:|
| (Intercept)                         |    0.690 | 0.043 |  15.887 | 169.499 |   0.000 |
| log10(DOC)                          |   -1.603 | 0.089 | -18.086 | 484.283 |   0.000 |
| SeasonSummer                        |    0.163 | 0.018 |   9.055 | 563.116 |   0.000 |
| SeasonFall                          |    0.102 | 0.018 |   5.577 | 587.324 |   0.000 |
| WaterBodyNon-embayment              |   -0.022 | 0.063 |  -0.343 | 221.121 |   0.732 |
| RiverPresentRiver                   |   -0.047 | 0.046 |  -1.024 |  77.776 |   0.309 |
| SeasonSummer:WaterBodyNon-embayment |   -0.072 | 0.024 |  -3.006 | 562.515 |   0.003 |
| SeasonFall:WaterBodyNon-embayment   |   -0.152 | 0.024 |  -6.381 | 579.500 |   0.000 |
| log10(DOC):WaterBodyNon-embayment   |    0.995 | 0.148 |   6.711 | 618.659 |   0.000 |
| SeasonSummer:RiverPresentRiver      |   -0.024 | 0.026 |  -0.934 | 562.444 |   0.351 |
| SeasonFall:RiverPresentRiver        |    0.090 | 0.025 |   3.612 | 564.407 |   0.000 |

and the two standard deviation measures associated with the model

| Group    | Term              | Estimate |
|:---------|:------------------|---------:|
| SiteID   | sd\_\_(Intercept) |    0.150 |
| Residual | sd\_\_Observation |    0.113 |

### Plots of fitted model

Here is a plot of the fitted linear model. Both the response (1% UV
Depth) and covariate (DOC) are on a base-10 logarithm scale.

![](supplementalReport_files/figure-commonmark/unnamed-chunk-14-1.png)

Here is the same model with the response in the original units.

![](supplementalReport_files/figure-commonmark/unnamed-chunk-15-1.png)

and here is a version where both the response and DOC are in the
original units.

![](supplementalReport_files/figure-commonmark/unnamed-chunk-16-1.png)

## 1% PAR depth analysis

The backward selection procedure chose the following model

![\begin{multline}
\log\_{10}(PAR) = \beta_0 + \beta_1\log\_{10}(DOC) + \beta_2\times Huron + \beta_3\times Erie + \beta_4\times Ontario + \\
\beta_5\times Summer + \beta_6\times Fall + \beta_7\times Non\text{-}embayment + \beta_8\log\_{10}(DOC)\times Non\text{-}embayment
\end{multline}](https://latex.codecogs.com/svg.latex?%5Cbegin%7Bmultline%7D%0A%5Clog_%7B10%7D%28PAR%29%20%3D%20%5Cbeta_0%20%2B%20%5Cbeta_1%5Clog_%7B10%7D%28DOC%29%20%2B%20%5Cbeta_2%5Ctimes%20Huron%20%2B%20%5Cbeta_3%5Ctimes%20Erie%20%2B%20%5Cbeta_4%5Ctimes%20Ontario%20%2B%20%5C%5C%0A%5Cbeta_5%5Ctimes%20Summer%20%2B%20%5Cbeta_6%5Ctimes%20Fall%20%2B%20%5Cbeta_7%5Ctimes%20Non%5Ctext%7B-%7Dembayment%20%2B%20%5Cbeta_8%5Clog_%7B10%7D%28DOC%29%5Ctimes%20Non%5Ctext%7B-%7Dembayment%0A%5Cend%7Bmultline%7D "\begin{multline}
\log_{10}(PAR) = \beta_0 + \beta_1\log_{10}(DOC) + \beta_2\times Huron + \beta_3\times Erie + \beta_4\times Ontario + \\
\beta_5\times Summer + \beta_6\times Fall + \beta_7\times Non\text{-}embayment + \beta_8\log_{10}(DOC)\times Non\text{-}embayment
\end{multline}")

where the
![\beta_0](https://latex.codecogs.com/svg.latex?%5Cbeta_0 "\beta_0")
term is a random effect to account for the variability at each site
(similar to a block term in experimental design). The variables
![Huron](https://latex.codecogs.com/svg.latex?Huron "Huron"),
![Erie](https://latex.codecogs.com/svg.latex?Erie "Erie"),
![Ontario](https://latex.codecogs.com/svg.latex?Ontario "Ontario")
![Summer](https://latex.codecogs.com/svg.latex?Summer "Summer"),
![Fall](https://latex.codecogs.com/svg.latex?Fall "Fall"), and
![OpenWater](https://latex.codecogs.com/svg.latex?OpenWater "OpenWater")
are indicator, or dummy, variables indicating the observation occurred
in that lake, season, or water type.

### Residual analysis

First, we take a look at some residual plots before proceeding to look
at the model

![](supplementalReport_files/figure-commonmark/unnamed-chunk-18-1.png)

Nothing overly concerning here. Some minor deviation from Normality but
regression methods are robust to some minor deviations from normality.

### Measures of model fit

Some overall model measures

|   N |  df | Resid.SE |      AIC |      BIC | Marginal R2 | Conditional R2 |
|----:|----:|---------:|---------:|---------:|------------:|---------------:|
| 665 | 654 |    0.134 | -616.314 | -566.817 |       0.524 |           0.67 |

The fixed effect in the chosen model explain about 52% of the
variability in the logarithm of PAR depth. When accounting for the
variability within each site, the model explains about 66% of the
varibility.

### Model coefficients

The fixed effects in the fitted model (or expected value of the random
effect)… Recall the response here is the
![\log\_{10}](https://latex.codecogs.com/svg.latex?%5Clog_%7B10%7D "\log_{10}")
of the PAR Depth (m)

| Term                              | Estimate |    SE |  t.stat |      df | p.value |
|:----------------------------------|---------:|------:|--------:|--------:|--------:|
| (Intercept)                       |    1.400 | 0.034 |  40.606 | 113.193 |   0.000 |
| log10(DOC)                        |   -1.066 | 0.075 | -14.252 | 345.611 |   0.000 |
| LakeHuron                         |    0.130 | 0.037 |   3.460 |  60.493 |   0.001 |
| LakeErie                          |   -0.170 | 0.052 |  -3.284 |  59.056 |   0.002 |
| LakeOntario                       |    0.021 | 0.047 |   0.450 |  64.145 |   0.654 |
| SeasonSummer                      |    0.031 | 0.013 |   2.317 | 599.690 |   0.021 |
| SeasonFall                        |   -0.055 | 0.013 |  -4.138 | 616.868 |   0.000 |
| WaterBodyNon-embayment            |   -0.116 | 0.057 |  -2.042 | 207.474 |   0.042 |
| log10(DOC):WaterBodyNon-embayment |    0.646 | 0.149 |   4.344 | 630.684 |   0.000 |

and the two standard deviation measures associated with the model

| Group    | Term              | Estimate |
|:---------|:------------------|---------:|
| SiteID   | sd\_\_(Intercept) |    0.089 |
| Residual | sd\_\_Observation |    0.134 |

### Plots of the fitted model

And a plot of the fitted linear model (log of PAR with log of DOC

![](supplementalReport_files/figure-commonmark/unnamed-chunk-22-1.png)

and with PAR in the original units

![](supplementalReport_files/figure-commonmark/unnamed-chunk-23-1.png)

and another version with DOC and PAR in their original units.

![](supplementalReport_files/figure-commonmark/unnamed-chunk-24-1.png)
