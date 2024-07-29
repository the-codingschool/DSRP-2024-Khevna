library(utils)
library(ggcorrplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(gridExtra)
library(paletteer)


setwd("C:/Users/Tony/OneDrive/Documents/tonyR/DSRP-2024-Khevna/tonyWork")
data <- read.csv("../data/breast_cancer_classification_data.csv")

numericalData = select(data,c("radius_mean","texture_mean","perimeter_mean",
                              "area_mean","smoothness_mean","compactness_mean",
                              "concavity_mean","concave.points_mean","symmetry_mean",
                              "fractal_dimension_mean","radius_se","texture_se","perimeter_se",
                              "area_se","smoothness_se","compactness_se","concavity_se",
                              "concave.points_se","symmetry_se","fractal_dimension_se",
                              "radius_worst","texture_worst","perimeter_worst","area_worst",
                              "smoothness_worst","compactness_worst","concavity_worst",
                              "concave.points_worst","symmetry_worst","fractal_dimension_worst"))

correlation_matrix <- cor(numericalData,use = "complete.obs")

ggcorrplot(correlation_matrix)

plot_list <- list()
for (variable in c("radius_mean","texture_mean","perimeter_mean",
                   "area_mean","smoothness_mean","compactness_mean",
                   "concavity_mean","concave.points_mean","symmetry_mean",
                   "fractal_dimension_mean","radius_se","texture_se","perimeter_se",
                   "area_se","smoothness_se","compactness_se","concavity_se",
                   "concave.points_se","symmetry_se","fractal_dimension_se",
                   "radius_worst","texture_worst","perimeter_worst","area_worst",
                   "smoothness_worst","compactness_worst","concavity_worst",
                   "concave.points_worst","symmetry_worst","fractal_dimension_worst")) {
  p <- ggplot(data, aes_string(x="diagnosis",y = variable)) + 
    geom_boxplot() + 
    geom_jitter(aes(color=diagnosis),width = 0.2, height = 0, alpha = 0.6) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),  # Remove x-axis text
          axis.ticks.x = element_blank(),  # Remove x-axis ticks
          legend.position = "none")
  plot_list[[variable]] <- p
}
do.call(grid.arrange, c(plot_list, ncol = 10))