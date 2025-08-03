# EHRsmoker

EHRsmoker: Improving Electronic Health Records (EHR) Smoking History Data Through Rule-based  Longitudinal Smoothing and scoring.

The algorithm identifies and corrects inconsistencies in structured smoking records by comparing smoking status, quit dates, and packs (a.k.a., pack-years, packs per day) across a patient’s timeline to identify implausible patterns. Using a state transition graph that defines plausible versus implausible status changes, the algorithm evaluates each person's full longitudinal smoking data to determine the most likely transition sequence. For quantitative values, the algorithm applies a back-calculation approach that starts from reliable pack-year values and uses smoking status and packs per day information to estimate and replace missing or inconsistent measurements.

### Install EHRsmoker package
```{r}
library(remotes)
install_github("HanLabCollaboration/EHRsmoker", force = TRUE)
```

### (Optional): Install other packages if it is not installed
ComplexHeatmap from Bioconnductor
```{r}
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
```
Other package from CRAN
```{r}
packages_to_install <- c("ggplot2", "tidyverse", "lubridate", "pbapply")
for (pkg in packages_to_install) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
    }
}
```

### Loading need R packages
Load required libraries
```{r}
library(ggplot2)
library(tidyverse)
library(lubridate)
library(pbapply)
library(ComplexHeatmap)
library(EHRsmoker)
```
### Read in Example Data
We created a small cohort example data by sampling 500 patients with more than 10 observations. Patient MRN has been de-identified. Example data can be downloaded in this repository. 

```{r}
all <- read.csv("EHRsmoker_example_data.csv")
head(all)
```



















