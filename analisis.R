library(e1071)
library(dplyr)
library(readxl)
library(xtable)
library(corrplot)
library(ggplot2)

##### FUNCTIONS #####

dataframe_to_latex_files <- function (dataframe, ncolumns, table_name) {
  label_array <- colnames(dataframe)
  i <- 1
  while((i-1) * ncolumns < length(label_array)) {
    aux_name <- paste(table_name, "_", i, sep="")
    outfile_name <- paste(aux_name, ".tex", sep="")
    first_index <- (i-1) * ncolumns + 1
    last_index <- min(i * ncolumns, length(label_array))
    latex_table <- xtable(dataframe[label_array[first_index:last_index]],
                          caption=gsub("_", " ", aux_name), display=rep('f', ncolumns+1))
    print.xtable(latex_table, type="latex", file=outfile_name, scalebox=0.6)
    i <- i + 1
  }
}

generate_measurements_dataframe <- function (dataframe) {
  label_array <- names(dataframe)
  measurement_array <- c("Media","Varianza","Des.Típica","CV","Mínimo","Percentil-25","Mediana","Percentil-75","Máximo","Skewness","Kurtosis")
  descriptive_values <- data.frame(matrix("", ncol = 0, nrow = length(measurement_array)))
  row.names(descriptive_values) <- measurement_array
  
  for (label in label_array) {
    row_data <- pull(dataframe, label)
    
    row_mean <- mean(row_data)
    row_sd <- sd(row_data)
    new_column <- c(
      row_mean,
      row_sd * row_sd,
      row_sd,
      (row_sd / abs(row_mean)),
      min(row_data),
      quantile(row_data, 0.25),
      median(row_data),
      quantile(row_data, 0.75),
      max(row_data),
      skewness(row_data),
      kurtosis(row_data))
    descriptive_values[label] <- new_column
  }
  
  descriptive_values
}

generate_graph <- function (graph, filename, enable_output=TRUE) {
  if(enable_output) {
    pdf(filename)
  }
  graph
  if(enable_output) {
    dev.off()
  }
}

##### GLOBAL VARIABLES #####
enable_graph_export = TRUE

##### MAIN #####

# Read data
data <- read_excel("datos.xlsx")

# Generate data example
dataframe_to_latex_files(data[1:5,], 9, "tabla_de_datos_de_ejemplo")

# Filter only numerical data
num_data <- data[-c(1,2)]

# Create table with descriptive analisis values
descriptive_values <- generate_measurements_dataframe(num_data)

# Create LATEX files with descriptive analisis
dataframe_to_latex_files(descriptive_values, 8, "tabla_de_analisis_descriptivo")

# Create correlation matrix
corr_matrix <- cor(num_data)

# Crete PDF file with correlation matrix
generate_graph(corrplot.mixed(corr_matrix, tl.pos='lt', tl.cex=0.7, lower.col = "black", number.cex=0.5),
             "matriz_de_correlacion.pdf", enable_graph_export)

# Create histograms for VENTAS
generate_graph(hist(num_data$VENTAS, breaks=10),
             "histograma_de_ventas.pdf", enable_graph_export)
  
#---------
# La condición de dentro de la región de rechazo no se cumple, por lo que no estamos
# obligados a rechazar la hipótesis inicial, pero no obliga que sea cierta