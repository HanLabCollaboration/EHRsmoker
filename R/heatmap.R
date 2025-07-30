#' @title Heatmap of Smoking Status Over Time
#' @description Creates a heatmap of smoking status for multiple patients over time.
#' @param data_list A list containing data frames for each patient, where each data frame includes columns for note date and smoking status.
#' @param patients A vector of patient identifiers for whom the heatmap is to be generated.
#' @param ... Additional arguments passed to the Heatmap function from the ComplexHeatmap package.
#' @return A heatmap object representing the smoking status of patients over time.
#' @export  

# Heatmap plot
heatmap_status <- function(data_list, patients, ...){
  plot_list <- data_list[patients]
  plot_list <- lapply(plot_list, to_month)
  timepoint <- unique(unlist(lapply(plot_list, function(x) as.character(x$month))))
  timepoint <- sort(timepoint)

  mat <- matrix(NA, nrow = length(patients), ncol = length(timepoint))
  rownames(mat) <- patients
  colnames(mat) <- timepoint

  # m <- names(plot_list)[1]
  for (m in names(plot_list)){
    temp <- plot_list[[m]]
    temp <- temp[ , c("month", "status")]
    temp <- temp[match(timepoint, as.character(temp$month)), ]
    mat[m, ] <- temp[["status"]]
  }

  colnames(mat) <- sub("-[^-]+$", "", colnames(mat))
  Heatmap(mat, name = "Smoking Status", col = color, na_col = "#F0F0F0", ...)
}

get_mode <- function(v) {
  unv <- unique(v)
  unv[which.max(tabulate(match(v, unv)))]
}


to_month <- function(data){
  data$month <- floor_date(data$note_date, "month")
  result <- data %>%
    group_by(month) %>%
    summarise(status = get_mode(smoking_status))
  return(result)
}



