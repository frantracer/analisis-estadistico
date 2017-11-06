library(readxl)
library(xtable)
library(corrplot)

data <- read_excel("datos.xlsx")
data_numeric <- data[-2]

print.xtable(xtable(t(summary(data_numeric))), type="latex", file="my_summary.tex", floating.environment="sidewaystable")

corr_matrix <- cor(data_numeric)

pdf("correlation_matrix.pdf",width=6,height=6)
corrplot(corr_matrix)
dev.off
