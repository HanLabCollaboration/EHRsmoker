#'@title Scatter Plot of Smoking Data
#'@description Creates a scatter plot of smoking data for a specific patient, showing the variable (e.g., pack years) over time, colored by smoking status.
#'@param data_list A list containing data frames for each patient, where each data frame includes columns for note date,
#'                 the variable of interest (e.g., pack years), and smoking status.
#'@param patient A string specifying the patient identifier for whom the plot is to be generated.
#'@param variable A string specifying the variable to be plotted (default is "pack_years").
#'@param smoking_status A string specifying the column name for smoking status (default is "smoking_status").
#'@return A ggplot object representing the scatter plot.
#'@export

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


