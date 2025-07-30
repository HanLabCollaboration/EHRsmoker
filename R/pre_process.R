#' @title Pre-process Smoking Data
#' @description \code{preprocess()} Pre-processes smoking data for a single patient.
#' @param data a data frame containing smoking data for a single patient.
#' @param match_status a data frame containing the mapping of smoking status.
#' @param id a string indicating the column name for patient ID, default is "mrn".
#' @param note_date a string indicating the column name for note date, default is "noted_date".
#' @param quit_date a string indicating the column name for quit date, default is "smoking_quit_date".
#' @param smoking_status a character vector indicating the column names for smoking status, default is c("smoking_tob_use", "tobacco_user").
#' @param merge_status a string indicating the column name for merged smoking status, default is "smoking_status".
#' @param pack_days a string indicating the column name for pack days, default is "TOBACCO_PAK_PER_DY".
#' @param pack_years a string indicating the column name for pack years, default is "tobacco_pck_yrs_old".
#' @param other a string indicating any other column names to be included, default is NA.
#' @return A list of corrected data frames for each patient.
#' @export


##############################################################
################### Pre-processing ###########################
##############################################################
# main function
preprocess <- function(data, match_status, id = "mrn", note_date = "noted_date", quit_date = "smoking_quit_date",
                       smoking_status = c("smoking_tob_use", "tobacco_user"), merge_status = "smoking_status",
                       pack_days = "TOBACCO_PAK_PER_DY", pack_years = "tobacco_pck_yrs_old", time_format = "%m/%d/%y",
                       other = NA){
  # Check if the required columns are not NA
  required_cols <- c(id, note_date, smoking_status)
  if (any(is.na(required_cols))) {
    stop("Required columns cannot be NA. Please provide valid column names.")
  }

  # warning message if NA in one of the pack_days, pack_years, or quit_date variable
  optional_cols <- c(quit_date, pack_days, pack_years)
  if (any(is.na(optional_cols))) {
    warning("Optional columns should be provided if they are not NA to improve accuracy.")
  }

  message("Pre-processing structured smoking data")
  message("Select needed columns")
  data <- select_col(data, id, note_date, smoking_status, quit_date, pack_days, pack_years, other)
  message("Cleaning smoking status columns")
  data <- merge_status(data, match_status, merge_status, smoking_status)
  message("Renaming columns and cleaning up")
  data <- col_rename(data, id, note_date, merge_status, quit_date, pack_days, pack_years)
  return(data)
}


make_sorted_list <- function(data, time_format = "%m/%d/%y"){
  message("Splitting and sorting data by patient ID")
  data <- id_split_sort(data, time_format)
  return(data)
}


# Small functions for pre-processing
select_col <- function(data, id, note_date, smoking_status, quit_date = NA, pack_days = NA, pack_years = NA, other = NA){
  if (is.na(pack_days) == FALSE && is.na(pack_years) == TRUE){
    stop("pack_years must be provided if pack_days is provided")
  }
  message("Subset required columns for structured data")
  data <- data[ , colnames(data) %in% c(id, note_date, smoking_status, quit_date, pack_days, pack_years)]
  return(data)
}


merge_status <- function(data, match_status, merge_status = "smoking_status",
                         smoking_status = c("smoking_tob_use", "tobacco_user")){
  message("Creating smoking status column")
  data[[merge_status]] <- NA
  for (c in smoking_status){
    message(paste0("Processing column: ", c))
    data[[merge_status]][data[[c]] %in% match_status$Never] <- "Never"
    data[[merge_status]][data[[c]] %in% match_status$Current] <- "Current"
    data[[merge_status]][data[[c]] %in% match_status$Former] <- "Former"
  }
  data[[merge_status]][is.na(data$smoking_status)] <- "Unknown"
  return(data)
}


col_rename <- function(data, id, note_date, merge_status, quit_date,
                       pack_days, pack_years){
  message("Renaming columns")
  replace_names = c(id = id, note_date = note_date, smoking_status = merge_status,
                    quit_date = quit_date, pack_days = pack_days, pack_years = pack_years)
  replace_names <- replace_names[!is.na(replace_names)]
  data <- data[, match(replace_names, colnames(data))]
  colnames(data) <- names(replace_names)
  data$id <- paste0("ID", data[["id"]])
  return(data)
}


id_split_sort <- function(data, time_format = time_format){
  message("Convert date variables.")
  data$note_date <- as.Date(data$note_date, format = time_format)
  data$quit_date <- as.Date(data$quit_date, format = time_format)

  message("Ordering data by patient ID and note date")
  data <- data %>% arrange(id, note_date)
  data <- data[!duplicated(data), ]
  data <- data[!is.na(data$note_date), ]  # Remove rows with NA in note_date

  message("Splitting data by patient ID")
  out <- split(data, f = data$id)
  return(out)
}


# packs_NAtoZero <- function(data){
#   message("Converting NA to 0 in pack_years and pack_days")
#   if ("pack_years" %in% colnames(data) == FALSE || "pack_days" %in% colnames(data) == FALSE){
#     stop("pack_years or pack_days column not found")
#     stop("Please run functions to build constructured smoking data first")
#   }
#   if ("smoking_status" %in% colnames(data) == FALSE){
#     stop("smoking_status column not found")
#     stop("Please run functions to build constructured smoking data first")
#   }
#   data$pack_days[is.na(data$pack_days)] <- 0
#   data$pack_years[is.na(data$pack_years)] <- 0
#   return(data)
# }










