# EHRsmoker

EHRsmoker: Improving Electronic Health Records (EHR) Smoking History Data Through Rule-based  Longitudinal Smoothing and scoring.

The algorithm identifies and corrects inconsistencies in structured smoking records by comparing smoking status, quit dates, and packs (a.k.a., pack-years, packs per day) across a patient’s timeline to identify implausible patterns. Using a state transition graph that defines plausible versus implausible status changes, the algorithm evaluates each person's full longitudinal smoking data to determine the most likely transition sequence. For quantitative values, the algorithm applies a back-calculation approach that starts from reliable pack-year values and uses smoking status and packs per day information to estimate and replace missing or inconsistent measurements.

## Install EHRsmoker package
```{r}
library(remotes)
install_github("HanLabCollaboration/EHRsmoker", force = TRUE)
```

## Loading EHRsmoker and other R packages
Load required libraries

```{r}
suppressMessages(library(ggplot2))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
suppressMessages(library(ComplexHeatmap))
suppressMessages(library(circlize))
suppressMessages(library(pbapply))
suppressMessages(library(purrr))
```

