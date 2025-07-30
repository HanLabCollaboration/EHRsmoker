#'  @title Format Diagnostic Records
#'  @description \code{format_diag()} formats diagnostic records by extracting relevant columns and renaming them.
#'  @param diag_data A data frame containing diagnostic records.
#'  @param match_status A data frame containing the mapping of smoking status.
#'  @param id A string indicating the column name for patient ID (default is "mrn").
#'  @param note_date A string indicating the column name for note date (default is "ct_scan_date").
#'  @param smoking_status A string indicating the column name for smoking status (default is "smoking").
#'  @param quit_date A string indicating the column name for quit date (default is NULL).
#'  @param pack_days A string indicating the column name for pack days (default is NULL).
#'  @param pack_years A string indicating the column name for pack years (default is NULL).
#'  @return A data frame with formatted diagnostic records containing columns for patient ID, note date, smoking status, quit date, pack days, and pack years.

add_diag_data <- function(smoking_data, diag_data, ...) {
  if (is.null(diag_data) || nrow(diag_data) == 0) {
    message("No diagnostic data provided, returning original smoking data")
    return(smoking_data)
  }

  message("Formatting diagnostic records")
  diag_data <- format_diag(diag_data, ...)

  # Merge diagnostic data with smoking data
  if (identical(colnames(smoking_data), colnames(diag_data)) == FALSE) {
    stop("Smoking data and diagnostic data must have the same column names after formatting.")
  } else {
    message("Merging diagnostic data with smoking data")
    merged_data <- rbind(smoking_data, diag_data)

  }
  # Return the merged data
  return(merged_data)
}



format_diag <- function(diag_data, match_status = match_status, id = "mrn", note_date = "ct_scan_date",
                        smoking_status = "smoking", quit_date = NULL, pack_days = NULL,
                        pack_years = NULL){
  diag_data$id <- paste0("ID", diag_data[[id]])
  diag_data$note_date <- as.Date(diag_data[[note_date]], format = "%m/%d/%Y")

  if (is.null(smoking_status) == FALSE){
    diag_data <- merge_status(diag_data, match_status = match_status, merge_status = "smoking_status",
                              smoking_status = smoking_status)
  } else {
    diag_data$smoking_status <- NA
  }

  if (is.null(quit_date) == FALSE){
    diag_data$quit_date <- diag_data[[quit_date]]
  } else {
    diag_data$quit_date <- NA
  }

  if (is.null(pack_days) == FALSE){
    diag_data$pack_days <- diag_data[[pack_days]]
  } else {
    diag_data$pack_days <- NA
  }

  if (is.null(pack_years) == FALSE){
    diag_data$pack_years <- diag_data[[pack_years]]
  } else {
    diag_data$pack_years <- NA
  }
  diag_data <- diag_data[ , c("id", "note_date", "smoking_status", "quit_date", "pack_days", "pack_years")]
  return(diag_data)
}





