# EHRsmoker

EHRsmoker: Improving Electronic Health Records (EHR) Smoking History Data Through Rule-based  Longitudinal Smoothing and scoring.
Mengrui Zhang, Ingrid Luo, Bo Gu, Aparajita Khan, Summer Han

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
<img width="925" height="426" alt="Screenshot 2025-08-02 at 9 02 54 PM" src="https://github.com/user-attachments/assets/eafb0307-a43e-48c8-a670-75e285b09cc5" />

Print the variable names
```{r}
colnames(all)
```
<img width="925" height="145" alt="Screenshot 2025-08-02 at 9 03 31 PM" src="https://github.com/user-attachments/assets/1cfe21ea-b8d0-442f-8902-2e200d6a477c" />

### Step1: Cleaning and Preprocessing

In the example data, "smoking_tob_use" and "tobacco_use" are two variables contain smoking status information. We can generate a table to different answer levels in the two variables.

```{r}
table(all$smoking_tob_use, all$tobacco_user)
```
<img width="927" height="220" alt="Screenshot 2025-08-02 at 9 04 11 PM" src="https://github.com/user-attachments/assets/1b3fb924-8736-48cb-8b7a-34c0c91d6e0d" />

Next, since different data has different names recorded for smoking status, we need to prepare a list to groups different answers into standardized smoking status names. Leftover answer levels will be classified into "Unknown".

```{r}
match_status <- list(Never = c("Never", "Passive"),
                    Current = c("Yes", "Some Days", "Every Day"),
                    Former = c("Former", "Quit"))
```

Now, we can run the cleaning and pre-processing for the data. 
```{r}
data_all <- preprocess(all, match_status, id = "mrn", note_date = "noted_date", 
                        quit_date = "smoking_quit_date",
                        smoking_status = c("smoking_tob_use", "tobacco_user"), merge_status = "smoking_status",
                        pack_days = "TOBACCO_PAK_PER_DY", pack_years = c("tobacco_pck_yrs_old"), other = NA)

```

The output of this function is a list containing each patient as separate data frame. The columns are also standardized for all patients. The column names should contain: 

1. id: The patient ids.
2. note_date: Recorded date for the observation.
3. smoking_status: Standardized smoking status. If input multiple columns for smoking status, the function will merge and overwrite with input column orders. 
4. quit_data: Record indicates the patient quit data.
5. pack_days: Record contain values for pack per days. 
6. pack_years: Record contain values for pack per years. 

Convert to list format (Make sure the time_format is correct).
```{r}
data_list <- make_sorted_list(data_all, time_format = "%Y-%m-%d")
```
Each data frame in the list will be sort and order by note date. 

First 10 rows of one patient example. 
```{r}
data <- data_list[["ID227943"]]
data[1:10, ]
```
<img width="930" height="238" alt="Screenshot 2025-08-02 at 9 13 24 PM" src="https://github.com/user-attachments/assets/fa905ac0-0fdf-4ed3-bb2e-a921c44f43f2" />





















