#' @title Record-Level Smoking Status
#' @description \code{record_smoking()} Identifies the record-level inconsistency in smoking status.
#' @param data_list a list of data frames contains patient's smoking data.
#' @param quit_date a string indicating the column name for quit date, default is "quit_date".
#' @param note_date a string indicating the column name for note date, default is "note_date".
#' @param percent a numeric value indicating the percentage of non-zero records quantitative variables (e.g. pack-years or pack-days)
#'               to determine whether to classify a patient as "Never" or "Former" smoker, default is 0.1.
#'
#' @return A list of corrected data frames for each patient.
#' @export


# main function
record_smoking <- function(data_list, quit_date = "quit_date",
                           note_date = "note_date", percent = 0.1) {
  # Check if data_list is a list
  if (!is.list(data_list)) {
    stop("data_list must be a list")
  }

  # Check if all dataframes in the list have the same structure
  if (!all(sapply(data_list, function(x) all(colnames(x) == colnames(data_list[[1]]))))) {
    stop("All dataframes in the list must have the same structure")
  }

  # add pack_years columns to all dataframes if they do not exist
  if (!all(sapply(data_list, function(x) "pack_years" %in% colnames(x)))) {
    message("pack_years is missing, Adding pack_years column as NA to all patients")
    data_list <- lapply(data_list, function(x) {
      if (!"pack_years" %in% colnames(x)) {
        x$pack_years <- NA
      }
      return(x)
    })
  }

  # add pack_days columns to all dataframes if they do not exist
  if (!all(sapply(data_list, function(x) "pack_days" %in% colnames(x)))) {
    message("pack_days is missing, Adding pack_days column as NA to all patients")
    data_list <- lapply(data_list, function(x) {
      if (!"pack_days" %in% colnames(x)) {
        x$pack_days <- NA
      }
      return(x)
    })
  }

  # Step 1: Align smoking status with quit dates:
  message("Aligning smoking status with quit dates")
  data_list <- pbapply::pblapply(data_list, status_byquit, quit_date = quit_date, note_date = note_date)
  # Step 2: Aligning smoking status and smoking intensity:
  message("Aligning smoking status and smoking intensity")
  data_list <- pbapply::pblapply(data_list, status_bypacks, percent = percent)
  return(data_list)
}

# ##############################################################

# Step 1: Align smoking status with quit dates:
status_byquit <- function(data, quit_date = "quit_date", note_date = "note_date") {
  if ("smoking_status" %in% colnames(data) == FALSE) {
    stop("smoking_status column not found")
  }

  if ("quit_date" %in% colnames(data) == FALSE) {
    stop("quit_date column not found")
  }

  quit <- NA
  for (i in 1:nrow(data)) {
    if (!is.na(data$quit_date[i])) {
      quit <- data$quit_date[i]
    }
    if (!is.na(quit)) {
      data[1:i, ]$quit_date[data[1:i, ]$note_date >= quit & data[1:i, ]$smoking_status == "Never"] <- quit
      data[1:i, ]$smoking_status[data[1:i, ]$note_date >= quit & data[1:i, ]$smoking_status == "Never"] <- "Former"
    }
  }
  return(data)
}




# Step 2: Aligning smoking status and smoking intensity:
status_bypacks <- function(data, percent = 0.1) {
  if ("smoking_status" %in% colnames(data) == FALSE) {
    stop("smoking_status column not found")
  }

  data$pack_days[is.na(data$pack_days)] <- 0
  data$pack_years[is.na(data$pack_years)] <- 0

  # count percent of non-zero pack years records
  percent_non_zero <- sum(data$pack_years != 0 | data$pack_days != 0) / nrow(data)

  for (i in 1:nrow(data)) {
    if (data$smoking_status[i] == "Never" && (data$pack_years[i] != 0 || data$pack_days[i] != 0)) {
      # Find the most recent non-never smoker status before the current date
      recent_status <- tail(data$smoking_status[data$note_date < data$note_date[i] & data$smoking_status != "Never"], 1)
      # Go forward
      if (length(recent_status) == 0 || is.na(recent_status)) {
          recent_status <- head(data$smoking_status[data$note_date > data$note_date[i] & data$smoking_status != "Never"], 1)
      }
      # replace value
      if (length(recent_status) > 0) {
        data$smoking_status[i] <- recent_status
      } else if (length(recent_status) == 0 || is.na(recent_status)) {
        if (percent_non_zero < percent) {
          data$smoking_status[i] <- "Never"
        } else {
          data$smoking_status[i] <- "Former"
        }
      }
    }
  }
  return(data)
}





