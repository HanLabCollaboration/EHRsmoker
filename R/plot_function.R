#######################################################
### Heatmap & Scatter plot
#######################################################
color <- c("Never" = "#8ecfc9", "Current" = "#fa7f6f", "Former" = "#ffbe7a", "Unknown" = "#999999")
scatter_plot <- function(data_list, patient, variable = "pack_years", smoking_status = "smoking_status"){
  plot_data <- data_list[[patient]]
  plot_data <- plot_data[ , c("note_date", variable, smoking_status)]
  colnames(plot_data) <- c("note_date", "variable", "smoking_status")
  ggplot(plot_data, aes(x=note_date, y=variable, color = smoking_status)) +
    geom_line(color = "black") +
    geom_point() + theme_classic() + ggtitle(patient) + ylab(variable) + xlab("Note Date") +
    scale_color_manual(values = color) +
    theme(axis.text = element_text(size = 10), axis.title = element_text(size = 13), title = element_text(size = 15),
          legend.title = element_blank(), legend.text = element_text(size = 15))
}



# Select mode for each patient in one month
# data <- plot_list[[1]]

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









