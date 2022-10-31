HW 9 Template
================
Mark Lai
2022-10-31

``` r
library(here)  # makes reading data more consistent
library(tidyverse)  # for data manipulation and plotting
library(haven)  # for importing SPSS/SAS/Stata data
library(lme4)  # for multilevel analysis
library(boot)  # for bootstrap CI
library(bootmlm)  # for multilevel bootstrap
library(performance)  # for ICC and R^2
library(sjPlot)  # for plotting
library(parameters)  # for kr results
library(modelsummary)  # for making tables
theme_set(theme_classic() +
    theme(panel.grid.major.y = element_line(color = "grey92")))
```

# Revised Prospectus

You can separate your prospectus and preliminary analysis in different
files, if needed. Please Include:

- Sufficient background to understand your research
- Research questions clearly defined
- A description of the nesting structure of your data
- Your plan of data analysis

# Preliminary Analysis

## Import Data

``` r
happy_dat <- read_sav(here("data_files", "happy_combined.sav"))
# Cluster means
happy_dat <- happy_dat %>%
    group_by(country) %>%
    mutate(income_cm = mean(income)) %>%
    ungroup()
```

## Variable Summary

- `CountryID`: Country ID
- `country`: Country’s name
- `income`: Income level (0-*least income* to 9-*most income*)
- `happy`: Feel happy (1-*not happy* to 4-*very happy*)

``` r
# By Country
datasummary(country ~ (income + happy) * (N + Mean + SD + Histogram),
            data = happy_dat)
```

| country     | income / N | income / Mean | income / SD | income / Histogram | overall happy / N | overall happy / Mean | overall happy / SD | overall happy / Histogram |
|:------------|-----------:|--------------:|------------:|-------------------:|------------------:|---------------------:|-------------------:|--------------------------:|
| Argentina   |        113 |          3.08 |        1.99 |          ▂▆▅▇▄▂▃▁▁ |               113 |                 2.95 |               0.83 |                      ▁▃▇▄ |
| Austria     |        162 |          4.23 |        2.45 |         ▁▅▇▆▆▄▄▅▂▃ |               162 |                 3.25 |               0.66 |                       ▁▇▅ |
| Belarus     |        121 |          3.45 |        1.77 |           ▁▄▅▇▆▄▃▁ |               121 |                 2.53 |               0.61 |                        ▇▇ |
| Belgium     |        206 |          4.64 |        2.56 |         ▃▂▃▆▇▄▅▄▃▃ |               206 |                 3.31 |               0.62 |                       ▁▇▅ |
| Brazil      |        182 |          1.67 |        1.86 |            ▆▇▃▁▁▁▁ |               182 |                 2.98 |               0.66 |                       ▂▇▂ |
| Bulgaria    |        118 |          3.70 |        1.75 |           ▁▁▃▃▇▃▃▁ |               118 |                 2.45 |               0.82 |                      ▂▇▆▂ |
| Canada      |        180 |          5.44 |        2.60 |         ▂▂▃▃▅▆▅▆▃▇ |               180 |                 3.08 |               0.78 |                       ▃▇▅ |
| Chile       |        181 |          3.90 |        2.58 |         ▄▄▃▇▆▃▄▂▃▂ |               181 |                 2.99 |               0.82 |                      ▁▄▇▅ |
| China       |        108 |          2.50 |        1.86 |            ▁▄▇▁▁▂▁ |               108 |                 2.94 |               0.83 |                       ▅▇▅ |
| Czech-Slov  |        187 |          4.36 |        2.06 |          ▃▃▆▇▅▃▃▂▁ |               187 |                 2.68 |               0.64 |                       ▃▇▁ |
| Denmark     |        112 |          4.24 |        2.70 |         ▅▅▆▆▆▇▇▅▁▅ |               112 |                 3.38 |               0.57 |                       ▁▇▆ |
| Estonia     |        110 |          3.03 |        1.34 |              ▃▄▇▄▂ |               110 |                 2.59 |               0.67 |                       ▁▄▇ |
| Finland     |         70 |          7.07 |        2.40 |           ▁▁ ▁▁▆▆▇ |                70 |                 3.09 |               0.53 |                       ▁▇▂ |
| France      |        117 |          4.05 |        3.00 |         ▆▇▆▄▄▆▂▄▄▅ |               117 |                 3.17 |               0.67 |                       ▁▇▃ |
| Hungary     |        142 |          3.66 |        1.94 |           ▂▃▃▅▇▆▃▂ |               142 |                 2.63 |               0.89 |                      ▂▄▇▂ |
| India       |        306 |          2.92 |        2.30 |          ▇▄▆▅▃▄▃▂▁ |               306 |                 2.86 |               0.76 |                      ▁▃▇▂ |
| Ireland     |        118 |          5.58 |        2.47 |         ▁▁▁▃▄▇▂▇▂▆ |               118 |                 3.32 |               0.61 |                       ▁▇▅ |
| Italy       |        192 |          2.49 |        1.29 |             ▁▂▇▄▂▁ |               192 |                 2.98 |               0.57 |                       ▁▇▁ |
| Japan       |         87 |          4.57 |        2.82 |         ▂▄▅▅▅▆▄▂▁▇ |                87 |                 2.97 |               0.67 |                       ▂▇▂ |
| Latvia      |         96 |          2.47 |        1.31 |             ▁▇▇▆▄▂ |                96 |                 2.57 |               0.59 |                        ▅▇ |
| Mexico      |        187 |          3.35 |        2.09 |         ▁▃▇▃▄▂▂▁▁▁ |               187 |                 2.98 |               0.72 |                       ▄▇▄ |
| Moscow      |        114 |          4.32 |        1.75 |           ▂▃▅▅▅▇▁▁ |               114 |                 2.57 |               0.76 |                      ▁▅▇▁ |
| N. Ireland  |         32 |          4.97 |        3.00 |         ▁▇▂▂▅▁▃▅▆▅ |                32 |                 3.25 |               0.67 |                        ▇▄ |
| Netherlands |        114 |          4.32 |        2.85 |         ▇▄▄▆▄▇▆▃▃▅ |               114 |                 3.33 |               0.66 |                        ▇▅ |
| Nigeria     |         89 |          4.48 |        2.53 |         ▃▃▃▃▇▄▆▅▃▁ |                89 |                 3.09 |               0.87 |                       ▅▅▇ |
| Norway      |        155 |          4.56 |        2.90 |         ▄▇▇▆▅▆▄▆▅▇ |               155 |                 3.16 |               0.59 |                       ▁▇▃ |
| Poland      |        129 |          3.66 |        1.76 |           ▂▂▂▄▇▅▂▁ |               129 |                 3.00 |               0.48 |                       ▁▇▁ |
| Portugal    |        146 |          3.49 |        2.69 |         ▂▅▇▅▂▂▁▁▁▃ |               146 |                 2.79 |               0.69 |                       ▃▇▁ |
| Romania     |        147 |          3.71 |        1.79 |           ▁▂▃▄▇▅▂▁ |               147 |                 2.68 |               0.71 |                      ▁▄▇▁ |
| Russia      |        197 |          3.51 |        2.18 |         ▁▂▇▄▃▁▂▁▁▁ |               197 |                 2.56 |               0.68 |                       ▇▇▁ |
| S. Africa   |        328 |          3.48 |        2.38 |           ▃▅▇▁▃▅▃▅ |               328 |                 2.98 |               0.83 |                      ▁▃▇▄ |
| Spain       |        435 |          3.36 |        2.06 |         ▃▃▆▇▇▄▃▂ ▁ |               435 |                 3.06 |               0.65 |                       ▂▇▃ |
| Sweden      |        107 |          3.82 |        3.57 |             ▇ ▁▁▂▄ |               107 |                 3.29 |               0.55 |                       ▁▇▄ |
| Switzerland |        142 |          3.91 |        2.79 |           ▆▆▅▇▅▄▄▃ |               142 |                 3.28 |               0.58 |                       ▁▇▄ |
| Turkey      |        139 |          3.14 |        1.59 |           ▁▁▅▇▅▂ ▁ |               139 |                 3.06 |               0.86 |                      ▁▂▇▅ |
| U.S.A.      |        202 |          3.38 |        1.82 |            ▂▃▄▇▆▃▄ |               202 |                 3.29 |               0.67 |                       ▁▇▅ |
| Uk          |        133 |          5.05 |        2.94 |         ▃▂▃▄▄▃▃▅▃▇ |               133 |                 3.26 |               0.65 |                       ▁▇▅ |
| W. Germany  |        222 |          3.15 |        2.53 |         ▄▇▇▄▃▂▂▂▁▂ |               222 |                 3.00 |               0.59 |                       ▁▇▂ |

## Intraclass Correlation

``` r
m0 <- lmer(happy ~ (1 | country), data = happy_dat)
performance::icc(m0)
```

    ## # Intraclass Correlation Coefficient
    ## 
    ##     Adjusted ICC: 0.121
    ##   Unadjusted ICC: 0.121

## Model

Level 1:

$$happy_{ij} = \beta_{0j} + \beta_{1j} income_{ij} + e_{ij}$$

Level 2:

$$
  \begin{align}
    \beta_{0j} & = \gamma_{00} + \gamma_{01} incomecm_{j} + u_{0j}     \\
    \beta_{1j} & = \gamma_{10} + u_{1j}
  \end{align}
$$

``` r
m1 <- lmer(happy ~ income + income_cm + (income | country),
           data = happy_dat)
```

## Results

``` r
msummary(m1,
         estimate = c("{estimate} [{conf.low}, {conf.high}]"),
         statistic = NULL,  # suppress the extra rows for SEs
         shape = effect + term ~ model,
         title = "Table 1: Model coefficients")
```

|        |                                 |         Model 1         |
|:-------|:--------------------------------|:-----------------------:|
| fixed  | (Intercept)                     | 2.580 \[2.260, 2.899\]  |
|        | income                          | 0.047 \[0.032, 0.062\]  |
|        | income_cm                       | 0.060 \[-0.019, 0.139\] |
| random | SD (Intercept country)          |          0.324          |
|        | SD (income country)             |          0.039          |
|        | Cor (Intercept\~income country) |         -0.719          |
|        | SD (Observations)               |          0.682          |
|        | Num.Obs.                        |          5926           |
|        | R2 Marg.                        |          0.037          |
|        | R2 Cond.                        |          0.159          |
|        | AIC                             |         12458.7         |
|        | BIC                             |         12505.5         |
|        | ICC                             |           0.1           |
|        | RMSE                            |          0.68           |

Table 1: Model coefficients

``` r
plot_model(m1, type = "pred", pred.type = "re",
           terms = "income", show.data = TRUE,
           jitter = 0.1, dot.size = 0.2)
```

![](hw9_template_files/figure-gfm/fig-m1-1.png)<!-- -->

Table 1 shows the fixed-effect coefficients. We found evidence for the
positive association, averaged across countries, between income and
happiness at the individual level, $\gamma_{10}$ = 0.047, *SE* = 0.0077,
*t*(35) = 6.06, *p* \< .001. The contextual effect was not significant,
$\gamma_{01}$ = 0.06, *SE* = 0.04, *t*(38) = 1.49, *p* = .144 (see the
Figure). The marginal $R^2$ (Nakagawa et al., 2017) for the model was
estimated to be 0.037, 95% bootstrap CI \[0.02, 0.07\].
