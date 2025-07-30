#' @title Longitudinal Smoking Status Correction
#' @description \code{longitudinal_smoking()} Corrects the longitudinal smoking status for each patient.
#' @param data_list a list of data frames contains patient's smoking data.
#' @param percent a numeric value indicating the percentage of non-zero records quantitative variables (e.g. pack-years or pack-days)
#'               to determine whether to classify a patient as "Never" or "Former" smoker, default is 0.25.
#' @return A list of corrected data frames for each patient.
#' @export


##############################################################
####### Longitudinal-level Smoking Status Correction #########
##############################################################
# main function
longitudinal_smoking <- function(data_list, percent = 0.25) {
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

  # Step 1: create a status matrix for each patient.
  message("Creating status matrix for each patient")
  idmatrix <- pbapply::pblapply(data_list, status_matrix)
  ind <- pbapply::pblapply(idmatrix, status_locate)
  ind <- unlist(ind)
  ind <- names(ind[which(ind == TRUE)])
  message("There are ", length(ind), " patients with illogic smoking status transition")
  subset_list <- data_list[ind]

  # Step 2: correct the smoking status on subset
  message("Correcting smoking status for patients with illogic transition")
  subset_list <- pbapply::pblapply(subset_list, status_longitudinal, percent = percent)

  # Step 3: put the corrected data back to the original list
  for (i in 1:length(data_list)) {
    if (names(data_list)[i] %in% names(subset_list)) {
      data_list[[i]] <- subset_list[[names(data_list)[i]]]
    }
  }
  return(data_list)
}


# Correct smoking status with illogic orders:
# 1. generate a status matrix for each patient.
status_matrix <- function(data) {
  if (nrow(data) >= 2) {
    ordered_states <- c('Never', 'Former', 'Current')
    transition_matrix <- matrix(0, nrow = length(ordered_states), ncol = length(ordered_states),
                                dimnames = list(ordered_states, ordered_states))
    for (i in 1:(nrow(data) - 1)) {
      current_state <- data[i, "smoking_status"][[1]]
      next_state <- data[i + 1, "smoking_status"][[1]]
      if (current_state %in% rownames(transition_matrix) && next_state %in% colnames(transition_matrix)) {
        transition_matrix[current_state, next_state] <- transition_matrix[current_state, next_state] + 1
      }
    }
    return(transition_matrix)
  } else {
    # message("Data must have at least 2 rows to create a transition matrix, return nothing")
  }
}


# 2. check the status matrix for each patient.
# colnames(transition_matrix)
status_locate <- function(status_matrix){
  if (is.null(status_matrix)) {
    return(FALSE)
  } else {
    if (status_matrix[2, 1] != 0 || status_matrix[3, 1] != 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# for (i in ind){
#   print(i)
#   data <- subset_list[[i]]
#   correct <- status_longitudinal(data)
# }

# data <- subset_list[[3]]
# percent = 0.25
# correct <- status_longitudinal(data, percent = 0.25)
# 3. Correct the smoking status:
status_longitudinal <- function(data, percent = 0.25) {
  if (nrow(data) < 2) {
    message("Data must have at least 2 rows to correct illogic location, return unchanged")
    return(data)
  }

  # percent of former records
  sum_former <- sum(data$smoking_status %in% c("Former", "Current"))

  if (sum_former > 0) {
    percent_former <- sum(data$smoking_status %in% c("Former", "Current")) / nrow(data)
  } else {
    percent_former = 0
  }
  mat <- status_matrix(data)

  # ind <- 1
  # i <- 5
  while (mat[2, 1] != 0 || mat[3, 1] != 0){
    # If there are any transitions from Former or Current to Never
    # Check if the patient has any non-zero pack-years/days in all previous records
    for (i in 1:(nrow(data) - 1)) {
      # print(i)
      current_state <- data[i, "smoking_status"]
      next_state <- data[i + 1, "smoking_status"]
      if (current_state %in% c('Former', 'Current') && next_state == 'Never') {

        # Find the most recent quit date in all previous records
        temp_quit <- data$quit_date[data$note_date <= data$note_date[i] & data$smoking_status != "Never"]
        if (length(temp_quit) > 0 && any(!is.na(temp_quit))) {
          most_recent_quit <- max(temp_quit, na.rm = TRUE)
        } else {
          most_recent_quit <- NA
        }

        # If the most recent quit date is not NA and is before the current note date, change the next state to Former
        if (!is.na(most_recent_quit) && most_recent_quit < data$note_date[i + 1]) {
          data$smoking_status[i + 1] <- 'Former'
          next
        }

        # Any Non-zero pack-years/days in all previous records?
        pre_year <- sum(data$pack_years[data$note_date <= data$note_date[i] & data$smoking_status != "Never"], na.rm = TRUE)
        pre_day <- sum(data$pack_days[data$note_date <= data$note_date[i] & data$smoking_status != "Never"], na.rm = TRUE)

        if (pre_year > 0 || pre_day > 0) {
          data$smoking_status[i + 1] <- current_state
        } else if (percent_former > percent) {
          data$smoking_status[i + 1] <- 'Former'
        } else if (percent_former <= percent) {
          data$smoking_status[i] <- 'Never'
        }
        break
      }
    }
    mat <- status_matrix(data)
    # ind <- ind + 1
  }
  return(data)
}


# # i <- 11
# for (i in 1:(nrow(data) - 1)) {
#   current_state <- data[i, "smoking_status"]
#   next_state <- data[i + 1, "smoking_status"]
#   if (current_state %in% c('Former', 'Current') && next_state == 'Never') {
#     # Any Non-zero pack-years/days in all previous records?
#     pre_year <- sum(data$pack_years[data$note_date <= data$note_date[i] & data$smoking_status != "Never"])
#     pre_day <- sum(data$pack_days[data$note_date <= data$note_date[i] & data$smoking_status != "Never"])
#
#     if (pre_year > 0 || pre_day > 0) {
#       data$smoking_status[i + 1] <- current_state
#     } else if (percent_former < percent) {
#       data$smoking_status[i] <- 'Never'
#     }
#   }


