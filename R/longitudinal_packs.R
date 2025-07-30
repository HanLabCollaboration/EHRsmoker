#' @title Longitudinal-level quantitative Correction
#' @description \code{longitudinal_packs()} Corrects the longitudinal quantitative variables
#'              (e.g. pack-years or pack-days) for each patient.
#' @param data_list a list of data frames contains patient's smoking data.
#' @param cluster_col a character string indicating the column name used to identify clusters of inconsistent records,
#'                   default is "inconsistent".
#' @param year_cap a numeric value indicating the maximum cap for pack-years, default is 20.
#' @param day_cap a numeric value indicating the maximum cap for pack-days, default is 200.
#' @param per_years a numeric value indicating the threshold for pack-years increase per year for current smokers, default is 10.
#' @param per_years_current a numeric value indicating the threshold for pack-years increase per year for non-current smokers, default is 2.
#' @return A list of corrected data frames for each patient.
#' @export


##############################################################
####### Longitudinal-level quantitative Correction #########
##############################################################
# main function
longitudinal_packs <- function(data_list, cluster_col = "inconsistent", year_cap = 20, day_cap = 200,
                               per_years = 10, per_years_current = 2) {
  # Check if data_list is a list
  if (!is.list(data_list)) {
    stop("data_list must be a list")
  }
  # Check if all dataframes in the list have the same structure
  if (!all(sapply(data_list, function(x) all(colnames(x) == colnames(data_list[[1]]))))) {
    stop("All dataframes in the list must have the same structure")
  }

  # Check if all data frames in data_list contain the required columns
  if (!all(sapply(data_list, function(x) all(c("smoking_status", "pack_years", "pack_days") %in% colnames(x))))) {
    stop("All data frames in data_list must contain smoking_status, pack_years, and pack_days columns")
  }

  message("Clean and Cap all quantitative variables..")
  data_list <- pbapply::pblapply(data_list, cap_clean_packs, year_cap = year_cap, day_cap = day_cap)
  data_list <- data_list[sapply(data_list, function(x) dim(x)[1]) > 0]
  message("Identifying inconsistent in quantitative variables..")
  data_list <- pbapply::pblapply(data_list, check_inconsistent_packs, per_years = per_years, per_years_current = per_years_current)

  message("Select each patients best quantitative interval as baseline..")
  data_list <- pbapply::pblapply(data_list, select_cluster, cluster_col = cluster_col)

  message("Perform historical correction..")
  data_list <- pbapply::pblapply(data_list, calculate_historical)

  message("Perform forward correction..")
  data_list <- pbapply::pblapply(data_list, calculate_forward)

  message("Adjust Smoking status based on the corrected quantitative variables..")
  data_list <- pbapply::pblapply(data_list, fix_never_smokers)
  return(data_list)
}


# for former/current smoker, if pack year is 0, change to NA
# if pack per year >= 20, change to 20
# if pack per day >= 200, change to 200

# data <- post
fill_pack_days <- function(data) {
  data <- data %>% mutate(
    # Create temporary column for positive pack per day values only
    TMP_POSITIVE_PPD = ifelse(pack_days > 0, pack_days, NA_real_)
  ) %>%
    # Fill forward with most recent positive value
    fill(TMP_POSITIVE_PPD, .direction = "down") %>%
    # If no positive value before, fill backward
    fill(TMP_POSITIVE_PPD, .direction = "up") %>%
    # If still NA after filling, use default value of 1
    mutate(
      TMP_POSITIVE_PPD = ifelse(is.na(TMP_POSITIVE_PPD), 1, TMP_POSITIVE_PPD),
      # Replace NA in original column with filled value
      pack_days = ifelse(is.na(pack_days), TMP_POSITIVE_PPD, pack_days)
    ) %>%
    select(-TMP_POSITIVE_PPD)
  return(data)
}


cap_clean_packs <- function(data, year_cap = 20, day_cap = 200) {
  data <- data[!data$smoking_status %in% "Unknown", ]
  # for former/current smoker, if pack year is 0, change to NA
  data$pack_years[data$smoking_status %in% c("Former", "Current") & data$pack_years == 0] <- NA
  # Never smokers: pack-years/pack-days set to NA
  data$pack_years[data$smoking_status == "Never"] <- NA
  data$pack_days[data$smoking_status == "Never"] <- NA

  # Former/never smokers: pack per day set to 0
  data$pack_days[data$smoking_status %in% c("Former", "Never") & is.na(data$pack_days)] <- 0

  # Cap pack_years and pack_days
  data$pack_years[data$pack_years > year_cap] <- year_cap
  data$pack_days[data$pack_days > day_cap] <- day_cap

  # add pack days
  data <- fill_pack_days(data)
  return(data)
}




# loop through two consecutive records
# Does any of the following exist:
# 1. Decrease in pack years?
# 2. Increase pack years / year > m (default m = 10) if pair has “Current”?
# 3. Increase pack years / year > n (default n = 2) if pair doesn’t have “Current”?
# add a new colnum and Flag the second record as inconsistent

# Skip all NA and calculate pack years difference between first and second record:
#   If any pair includes a “Current” smoking status, and if diff > p (default p = 10), or
# If the pair doesn’t have a ”Current” smoking status, and if diff > q (default q = 2), or
# For any pair of records, if pack years decreases from first to second record
# Flag the second record as inconsistent
# Keep looping until the last record
# data <- dat
# per_years = 10
# per_years_current = 2
# data <- temp
# i <- 1

check_inconsistent_packs <- function(data, per_years = 10, per_years_current = 2) {
  data$inconsistent <- NA
  if (nrow(data) <= 1) {
    return(data)
  }
  data <- data[!duplicated(data[ ,c("id", "note_date")]), ]
  pack_idx <- c()
  for (i in 1:(nrow(data) - 1)) {
    current_pack_years <- data$pack_years[i]
    next_pack_years <- data$pack_years[i + 1]
    if (is.na(current_pack_years) || is.na(next_pack_years)) {
      next
    }

    if (next_pack_years < current_pack_years) {
      data$inconsistent[i + 1] <- 1
      pack_idx <- c(pack_idx, i + 1)
      next
    }

    # Calculate the difference in pack years per year
    if (data$smoking_status[i] == "Current" || data$smoking_status[i + 1] == "Current"){
      pack_years_diff_per_year <- (next_pack_years - current_pack_years) / as.numeric(difftime(data$note_date[i + 1],
                                                                                               data$note_date[i], units = "days")) * 365
      if (pack_years_diff_per_year >= per_years) {
        data$inconsistent[i + 1] <- 1
        pack_idx <- c(pack_idx, i + 1)
      } else {
        data$inconsistent[i + 1] <- 0
      }
    } else if (data$smoking_status[i] != "Current" && data$smoking_status[i + 1] != "Current") {
      pack_years_diff_per_year <- (next_pack_years - current_pack_years) / as.numeric(difftime(data$note_date[i + 1],
                                                                                               data$note_date[i], units = "days")) * 365
      if (pack_years_diff_per_year >= per_years_current) {
        data$inconsistent[i + 1] <- 1
        pack_idx <- c(pack_idx, i + 1)
      } else {
        data$inconsistent[i + 1] <- 0
      }
    }
  }
  data$pack_years[pack_idx] <- NA
  return(data)
}



select_cluster <- function(dat, cluster_col = "inconsistent") {
  # Check if the specified column exists in the data
  if (!cluster_col %in% colnames(dat)) {
    stop(paste("Column", cluster_col, "not found in the data"))
  }

  dat$is_clean = dat[[cluster_col]] == 0 | is.na(dat[[cluster_col]])
  dat$run_id = cumsum(c(TRUE, diff(dat$is_clean) != 0))
  dat$run_id = ifelse(dat$is_clean, dat$run_id, NA)

  dat <- dat %>%
    mutate(
      run_length = ifelse(!is.na(run_id), n(), 0),
      run_start_date = ifelse(!is.na(run_id), min(note_date), NA),
      run_end_date = ifelse(!is.na(run_id), max(note_date), NA),
      run_date_diff = as.numeric(run_end_date - run_start_date)
    )

  dat <- dat %>%
    mutate(
      max_run_length = max(run_length, na.rm = TRUE),
      # For ties, get max date difference
      max_date_diff_for_longest = max(run_date_diff[run_length == max_run_length], na.rm = TRUE),
      # Mark the best run
      is_reliable = !is.na(run_id) &
        run_length == max_run_length &
        run_date_diff == max_date_diff_for_longest,

      first_reliable_pos = suppressWarnings(min(which(is_reliable))),
      first_reliable_pos = ifelse(is.infinite(first_reliable_pos), NA, first_reliable_pos),

      # Also mark the last false before the first true as part of best run
      is_reliable = is_reliable |
        (!is.na(first_reliable_pos) &
           row_number() == (first_reliable_pos - 1) &
           !is_clean)
    ) %>%
    select(-c(is_clean, run_id, run_length, run_start_date, run_end_date,
              run_date_diff, max_run_length, max_date_diff_for_longest, first_reliable_pos))

  dat$pack_years <- ifelse(dat$is_reliable == FALSE,
                                           NA_real_,
                                           dat$pack_years)
  return(dat)
}


# History calculation
# patient_data <- result
calculate_historical <- function(patient_data) {
  n <- nrow(patient_data)
  if (n == 1) {
    return(patient_data)
  }
  result <- patient_data
  # Start from the end and work backwards
  for (i in (n-1):1) {  # from second-to-last row to first row
    if (is.na(result$pack_years[i])) {
      # Find the next non-NA value
      next_idx <- i + 1
      while(next_idx <= n && is.na(result$pack_years[next_idx])) {
        next_idx <- next_idx + 1
      }

      if (next_idx <= n) {  # If we found a non-NA value
        # Calculate date difference in years
        date_diff <- as.numeric(
          difftime(result$note_date[next_idx],
                   result$note_date[i],
                   units = "days")
        ) / 365.25

        # Calculate historical pack years
        result$pack_years[i] <-
          result$pack_years[next_idx] -
          (date_diff * result$pack_days[i])
      }
    }
  }
  return(result)
}


# Forward calculation
calculate_forward <- function(patient_data){
  n <- nrow(patient_data)
  # If patient has only one record, return the original data unchanged
  if (n == 1) {
    return(patient_data)
  }

  result <- patient_data
  # Find indices of non-NA values
  nonNA_indices <- which(!is.na(result$pack_years))
  # Only proceed if we have any non-NA values
  if (length(nonNA_indices) > 0) {
    # Get the last non-NA index
    last_nonNA_idx <- max(nonNA_indices)

    # Only proceed with forward fill if we have any rows after the last non-NA
    if (last_nonNA_idx < n) {
      # Start with the first NA after last non-NA
      for (i in seq(from = last_nonNA_idx + 1, to = n)) {
        # Use the immediately previous value (whether original or calculated)
        prev_value <- result$pack_years[i-1]

        # Calculate date difference from previous point
        date_diff <- as.numeric(
          difftime(result$note_date[i],
                   result$note_date[i-1],
                   units = "days")
        ) / 365.25

        # Calculate forward pack years from previous value
        new_value <- prev_value + (date_diff * result$pack_years[i])

        # Only update if the new value is greater than or equal to previous value
        result$pack_years[i] <- max(new_value, prev_value)
      }
    }
  }
  return(result)
}



# Fix never smoker status with non-zero pack years
fix_never_smokers <- function(dat) {
  # Step 1: Identify never smokers with non-zero pack years
  never_with_packs <- dat %>%
    filter(smoking_status == "Never" & !is.na(pack_years))

  # If none found, return original data
  if(nrow(never_with_packs) == 0) {
    return(dat)
  }

  # Step 2: For each identified case, get surrounding records and analyze
  for(i in 1:nrow(never_with_packs)) {
    # current_mrn <- never_with_packs$mrn[i]
    current_date <- never_with_packs$note_date[i]
    current_index <- which(dat$note_date == current_date)

    # # Get patient's records
    # patient_data <- dat[dat$mrn == current_mrn,]

    # Find max pack per day for this patient
    max_ppd <- max(dat$pack_days, na.rm = TRUE)

    # Find previous and next records if they exist
    if(current_index > 1) {
      prev_record <- dat[current_index-1,]
      prev_status <- prev_record$smoking_status
    } else {
      prev_status <- NA
    }

    if(current_index < nrow(dat)) {
      next_record <- dat[current_index+1,]

      # Calculate year difference
      year_diff <- as.numeric(difftime(next_record$note_date, current_date, units = "days")) / 365.25

      # Check if pack-years increase exceeds physical possibility
      if(next_record$pack_years - max_ppd * year_diff > 0) {

        # Fix the smoking status
        if(!is.na(prev_status) && prev_status == "Current") {
          dat$smoking_status[current_index] <- "Current"
        } else {
          dat$smoking_status[current_index] <- "Former"
        }
      }
    }
  }
  return(dat)
}





























