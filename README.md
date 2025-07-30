# EHRsmoker

EHRsmoker: Improving Electronic Health Records (EHR) Smoking History Data Through Rule-based  Longitudinal Smoothing and scoring.

The algorithm identifies and corrects inconsistencies in structured smoking records by comparing smoking status, quit dates, and packs (a.k.a., pack-years, packs per day) across a patient’s timeline to identify implausible patterns. Using a state transition graph that defines plausible versus implausible status changes, the algorithm evaluates each person's full longitudinal smoking data to determine the most likely transition sequence. For quantitative values, the algorithm applies a back-calculation approach that starts from reliable pack-year values and uses smoking status and packs per day information to estimate and replace missing or inconsistent measurements.<img width="468" height="144" alt="image" src="https://github.com/user-attachments/assets/3b19edac-5d90-4d18-9acd-c0e237e9b7df" />


