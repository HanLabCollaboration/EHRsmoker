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

### Step2: Record-level smoking status inconsistency

In this step, we will identify the inconsistency in record-level for smoking status. Correction will be also be made. 

```{r}
correct_list <- record_smoking(data_list, quit_date = "quit_date",
                            note_date = "note_date", percent = 0.1)
```

percent: In one patient, the minimum percent of non-zero records of quantitative variables (e.g. pack-years or pack-days). 

First 10 rows of one patient example (After Correction). 
```{r}
data <- correct_list[["ID227943"]]
data[1:10, ]
```
<img width="932" height="241" alt="Screenshot 2025-08-02 at 9 14 23 PM" src="https://github.com/user-attachments/assets/5e62fac9-d576-4655-8905-c14a8b9d2121" />

### Step3: Longitudinal-level smoking status inconsistency

This step identify the inconsistency in longitudinal-level across different records. Making the longitudinal changes in smoking status are all correct. Incorrect smoking status changes are:

1. Current smoker to Never smoker
2. Former smoker to Never smoker

All other changes are considered as plausible changes. 

```{r}
correct_list <- longitudinal_smoking(correct_list, percent = 0.25)
```

percent: In one patient, the minimum percent of Former records of smoking status to be consider validate. 
First 10 rows of one patient example (After Correction). 
```{r}
data <- correct_list[["ID227943"]]
data[1:10, ]
```
<img width="926" height="240" alt="Screenshot 2025-08-02 at 9 15 09 PM" src="https://github.com/user-attachments/assets/f142d797-0e80-4974-9f5a-7b28f66acc14" />

## Step4: Longitudinal-level quantitative (pack-years/pack-days) inconsistency
After processing smoking status, here we smooth the quantitative variables in the dataset. 

```{r}
correct_list <- longitudinal_packs(correct_list, cluster_col = "inconsistent")
```
The function will return back smoothed quantitative variables for pack-years and pack-days. 

### Step5: Visulization and Validation

The package provide two types of visualization. 
1. With patient line plots for multiple variables (columns). 
2. Across patient heat map plots for smoking status.

Heat Map:
Before the Correction: 
```{r}
patients <- c("ID100764", "ID116326", "ID132413", "ID148109", "ID160894", 
              "ID206040", "ID213798", "ID226331", "ID227943", "ID232723", 
              "ID234792", "ID237480", "ID249829", "ID271218", "ID301459", 
              "ID302546", "ID31489", "ID351731", "ID719876", "ID733648", 
              "ID775389", "ID782713", "ID81615")

gra <- heatmap_status(data_list, patients, cluster_rows = FALSE, cluster_columns = FALSE, 
                      show_row_names = TRUE, show_column_names = TRUE)
gra
```
<img width="682" height="482" alt="Screenshot 2025-08-02 at 9 18 03 PM" src="https://github.com/user-attachments/assets/cd8f6644-cde8-4405-87d2-6de1b1e71fe5" />

After the Correction: 
```{r}
patients <- c("ID100764", "ID116326", "ID132413", "ID148109", "ID160894", 
              "ID206040", "ID213798", "ID226331", "ID227943", "ID232723", 
              "ID234792", "ID237480", "ID249829", "ID271218", "ID301459", 
              "ID302546", "ID31489", "ID351731", "ID719876", "ID733648", 
              "ID775389", "ID782713", "ID81615")

gra <- heatmap_status(correct_list, patients, cluster_rows = FALSE, cluster_columns = FALSE, 
                      show_row_names = TRUE, show_column_names = TRUE)
gra
```
<img width="684" height="475" alt="Screenshot 2025-08-02 at 9 18 22 PM" src="https://github.com/user-attachments/assets/76d834ff-e8cb-43eb-8489-a0596f3ba294" />

Line plot (Before Correction):
```{r}
gra <- scatter_plot(data_list, patient = "ID148567", variable = "pack_years")
gra
```
<img width="682" height="484" alt="Screenshot 2025-08-02 at 9 15 56 PM" src="https://github.com/user-attachments/assets/755b22be-eac0-445b-9133-e1ce93729143" />

Line plot (After Correction):
```{r}
gra <- scatter_plot(correct_list, patient = "ID148567", variable = "pack_years")
gra
```
<img width="674" height="483" alt="Screenshot 2025-08-02 at 9 16 29 PM" src="https://github.com/user-attachments/assets/14501b86-a8b1-4b3e-b8a3-d64f40988427" />




















