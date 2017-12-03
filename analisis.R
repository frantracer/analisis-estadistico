library(e1071)
library(dplyr)
library(readxl)
library(xtable)
library(corrplot)
library(ggplot2)
library(lmtest)

##### FUNCTIONS #####

p_ <- function(...) {
  paste(..., sep="")
}

n_ <- function(string) {
  tolower(iconv(gsub(" ", "_", string), to="ASCII//TRANSLIT"))
}

dataframe_to_latex_files <- function (dataframe, ncolumns, table_name, var_format='f') {
  label_array <- colnames(dataframe)
  i <- 1
  while((i-1) * ncolumns < length(label_array)) {
    aux_name <- p_(table_name, "_", i)
    outfile_name <- p_(n_(aux_name), ".tex")
    first_index <- (i-1) * ncolumns + 1
    last_index <- min(i * ncolumns, length(label_array))
    latex_table <- xtable(dataframe[label_array[first_index:last_index]],
                          caption=gsub("_", " ", aux_name), display=rep(var_format, ncolumns+1))
    print.xtable(latex_table, type="latex", file=outfile_name, scalebox=0.6)
    i <- i + 1
  }
}

table_to_latex_file <- function (datatable, table_name, var_format='f') {
  outfile_name <- p_(table_name, ".tex")
  latex_table <- xtable(datatable, caption=gsub("_", " ", table_name), display=rep(var_format, dim(datatable)[2]+1))
  print.xtable(latex_table, type="latex", file=outfile_name, scalebox=0.6)
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

generate_graph <- function (graph_function, filename, enable_output=TRUE) {
  if(enable_output) {
    filename <- n_(p_(filename,".pdf"))
    pdf(filename)
  }
  graph_function()
  if(enable_output) {
    dev.off()
  }
}

correlated_variables_in_matrix <- function(correlation_matrix, threshold) {
  nrows <- dim(correlation_matrix)[1]
  ncols <- dim(correlation_matrix)[2]
  relevant_vars <- c()
  for(i in 1:nrows) {
    for(j in i:ncols) {
      if(correlation_matrix[i,j] >= threshold && i != j) {
        relevant_vars <- c(relevant_vars, rownames(correlation_matrix)[i], colnames(correlation_matrix)[j])
      }
    }
  }
  relevant_vars <- unique(relevant_vars)
  
  relevant_vars
}

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
generate_graph(function () { corrplot.mixed(corr_matrix, tl.pos='lt', tl.cex=0.7, lower.col = "black", number.cex=0.5) },
             "matriz_de_correlacion")

# Look for the most relevant variables
relevant_vars <- correlated_variables_in_matrix(corr_matrix, 0.8)

# Create boxplots for relevant variables
for (variable in relevant_vars) {
  data_row <- pull(num_data, variable)
  label_name <- tolower(n_(variable))
  generate_graph(
    function () { boxplot(data_row, xlab=variable) },
    p_("diagrama_de_cajas_de_",label_name))
}

# Create histograms for relevant variables
for (variable in relevant_vars) {
  data_row <- pull(num_data, variable)
  data_min <- min(data_row)
  data_max <- max(data_row)
  interval <- ((data_max - data_min) / sqrt(length(data_row)))
  label_name <- tolower(n_(variable))
  generate_graph(
    function () { hist(data_row, breaks=seq(data_min, data_max+interval, interval), xlim=c(data_min-interval, data_max+interval),
         col="blue", main=p_("Histograma de ",variable), xlab=variable, ylab="Frecuencia") },
    p_("histograma_de_",label_name))
}

# Relevant variables dispersion plot
generate_graph(function() { pairs(num_data[relevant_vars]) }, "matriz_de_dispersion")

# Create contingency table with relevant variables
## VENTAS
data_row <- pull(num_data, "VENTAS")
data_min <- min(data_row)
data_max <- max(data_row)
interval <- ((data_max - data_min) / sqrt(length(data_row)))
data_intervals_ventas <- as.integer((pull(num_data, "VENTAS") - data_min) / interval) + 1

## EMPLEADOS
empleados_a_empresa <- function (n) {
  empresa <- ""
  if ( n >= 250 ) {
    empresa <- 'grande'
  } else if ( n >= 50 ) {
    empresa <- 'mediana'
  } else if ( n >= 10 ) {
    empresa <- 'pequeña'
  } else if ( n >= 0 ) {
    empresa <- 'micro'
  }
  empresa
}

data_interval_empleados <- sapply(pull(num_data, "NÚMERO DE EMPLEADOS"), empleados_a_empresa)

## Generate matrix
contingency_matrix <- table(data_intervals_ventas, data_interval_empleados)
table_to_latex_file(contingency_matrix, "matriz_de_contingencia_ventas_empleados")

# Linear regression model
for (var_index in seq(1,length(relevant_vars))) {
  # Generate linear model
  var_name <- relevant_vars[var_index]
  model_variable <- p_("`",var_name,"`")
  linear_expression <- paste( sapply(relevant_vars[-var_index], function(x) { p_("`",x,"`") } ), sep='+' )
  model_formula <- p_(model_variable,"~",linear_expression)
  linear_model <- lm(model_formula, data=num_data)
  
  # Plot linear model
  lm_summary <- summary(linear_model)
  multiple_r_squared <- lm_summary$r.squared
  intercept_pvalue <- lm_summary$coefficients[1,4]
  variable_pvalue <- lm_summary$coefficients[2,4]
  generate_graph(function() {
    x_var <- relevant_vars[-var_index][1]
    y_var <- var_name
    plot(pull(num_data, x_var),  pull(num_data, y_var), xlab=x_var, ylab=y_var)
    abline(linear_model, col='red')
    c <- coef(linear_model)
    mtext(p_(y_var,' = ',c[2],' * ',x_var,' + ',c[1]), 3, line=2, cex=0.8)
  }, p_("modelo_lineal_para_",var_name))
  
  # Get residuals of the model
  model_residuals <- resid(linear_model)
  
  # Diagnosis of the lineal model
  ## Plot residuals dispersion
  generate_graph(function() {
    plot(pull(num_data, var_name), model_residuals, xlab=var_name, ylab="Residuos")
  }, p_("residuos_de_modelo_lineal_para_",var_name))
  
  ## Residuals normality
  shapiro_test_pvalue <- as.numeric(shapiro.test(model_residuals)["p.value"])
  
  ## Residuals independence
  dw_test_pvalue <- as.numeric(dwtest(linear_model)["p.value"])
  
  # Create table with results
  measurement_array <- c("Multiple R-squared", "Intercepto p-value", "Variable entrada p-value", "Shapiro Test residuos p-value","Durbin-Watson Test residuos p-value")
  lm_test_pvalues <- data.frame(matrix("", ncol = 0, nrow = length(measurement_array)))
  row.names(lm_test_pvalues) <- measurement_array
  lm_test_pvalues[var_name] <- c(multiple_r_squared, intercept_pvalue, variable_pvalue, shapiro_test_pvalue, dw_test_pvalue)
  dataframe_to_latex_files(lm_test_pvalues, 1, p_("estadisticos_modelo_lineal_de_", var_name),var_format = 'e')
}

# Descriptive analysis per subset
subsets = c('Madrid', 'Barcelona')
label_group = "PROVINCIA"
relevant_vars <- c()
for (subset in subsets) {
  num_data <- data[pull(data, label_group) == subset, ][-c(1,2)]
  
  descriptive_values <- generate_measurements_dataframe(num_data)
  dataframe_to_latex_files(descriptive_values, 8, p_("tabla_de_analisis_descriptivo_de_", subset))
  
  corr_matrix <- cor(num_data)
  generate_graph(function () { corrplot.mixed(corr_matrix, tl.pos='lt', tl.cex=0.7, lower.col = "black", number.cex=0.5) },
                 p_("matriz_de_correlacion_de_",subset))
  
  relevant_vars <- union(relevant_vars, correlated_variables_in_matrix(corr_matrix, 0.8))
}

subset_data <- data[pull(data, label_group) %in% subsets, ]
for (variable in relevant_vars) {
  data_row <- pull(num_data, variable)
  label_name <- tolower(n_(variable))
  generate_graph(
    function () { boxplot(as.formula(p_("`",variable,"`~`",label_group,"`")), data=subset_data, xlab=variable) },
    p_("diagrama_de_cajas_de_",label_name,"_por_",label_group))
}

# Statistical hypothesis testing for equality between subsets
ventas_madrid <- pull(data[pull(data, label_group) == 'Madrid', ], 'VENTAS')
ventas_barna <- pull(data[pull(data, label_group) == 'Barcelona', ], 'VENTAS')
print(shapiro.test(ventas_madrid))
print(shapiro.test(ventas_barna))
var_test <- var.test(ventas_madrid, ventas_barna, ratio=1, alternative="two.sided", conf.level=0.05)
print(var_test)

# Linear regression model per subset
measurement_array <- c("Multiple R-squared", "Intercepto p-value", "Variable entrada p-value", "Shapiro Test residuos p-value","Durbin-Watson Test residuos p-value")
lm_test_pvalues <- data.frame(matrix("", ncol = 0, nrow = length(measurement_array)))
row.names(lm_test_pvalues) <- measurement_array

target_var <- 'VENTAS'
input_var <- 'PRODUCTIVIDAD'
model_formula <- as.formula(p_(target_var,"~",input_var))
for (subset in subsets) {
  linear_model <- lm(model_formula, data=num_data)
  
  # Plot linear model
  lm_summary <- summary(linear_model)
  multiple_r_squared <- lm_summary$r.squared
  intercept_pvalue <- lm_summary$coefficients[1,4]
  variable_pvalue <- lm_summary$coefficients[2,4]
  generate_graph(function() {
    x_var <- input_var
    y_var <- target_var
    plot(pull(num_data, x_var),  pull(num_data, y_var), xlab=n_(x_var), ylab=n_(y_var), main=subset)
    abline(linear_model, col='red')
    c <- coef(linear_model)
    mtext(p_(y_var,' = ',c[2],' * ',x_var,' + ',c[1]), 3, line=3, cex=0.8)
  }, p_("modelo_lineal_de_",target_var,"_para_",subset))
  
  # Get residuals of the model
  model_residuals <- resid(linear_model)
  
  # Diagnosis of the lineal model
  ## Plot residuals dispersion
  generate_graph(function() {
    plot(pull(num_data, target_var), model_residuals, xlab=target_var, ylab="Residuos", main=subset)
  }, p_("residuos_de_modelo_lineal_de_",target_var,"_para_",subset))
  
  ## Residuals normality and independence tests
  shapiro_test_pvalue <- as.numeric(shapiro.test(model_residuals)["p.value"])
  dw_test_pvalue <- as.numeric(dwtest(linear_model)["p.value"])
  
  lm_test_pvalues[subset] <- c(multiple_r_squared, intercept_pvalue, variable_pvalue, shapiro_test_pvalue, dw_test_pvalue)
}

# Create table with results per subset
dataframe_to_latex_files(lm_test_pvalues, 2, p_("estadisticos_modelo_lineal_por_provincia_de_", target_var), var_format = 'e')
