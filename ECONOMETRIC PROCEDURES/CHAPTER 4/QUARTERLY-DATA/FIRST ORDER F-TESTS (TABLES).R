#F-test, first order serial autocorrelation of cross-country residuals
#Summary statistics:

library(tidyverse)

#SWITZERLAND

ftest_1stord_model1t.CH <- 
  sum.model1t.ch.ssvs$res$p.res %>%
  as.data.frame() %>%
  rownames_to_column(var = "p-values") %>%
  rename("Frequency" = `# p-values`) %>%
  rename("Percentage" = `in %`) %>%
  mutate("Percentage" = paste0("(", Percentage, ")")) %>%
  unite("Model 1", c("Frequency", "Percentage"), sep = " ")

ftest_1stord_model2t.CH <- 
  sum.model2t.ch.ssvs$res$p.res %>%
  as.data.frame() %>%
  rownames_to_column(var = "p-values") %>%
  rename("Frequency" = `# p-values`) %>%
  rename("Percentage" = `in %`) %>%
  mutate("Percentage" = paste0("(", Percentage, ")")) %>%
  unite("Model 2", c("Frequency", "Percentage"), sep = " ")

ftest_1stord_model3t.CH <- 
  sum.model3t.ch.ssvs$res$p.res %>%
  as.data.frame() %>%
  rownames_to_column(var = "p-values") %>%
  rename("Frequency" = `# p-values`) %>%
  rename("Percentage" = `in %`) %>%
  mutate("Percentage" = paste0("(", Percentage, ")")) %>%
  unite("Model 3", c("Frequency", "Percentage"), sep = " ")

ftest_1stord_model4t.CH <- 
  sum.model4t.ch.ssvs$res$p.res %>%
  as.data.frame() %>%
  rownames_to_column(var = "p-values") %>%
  rename("Frequency" = `# p-values`) %>%
  rename("Percentage" = `in %`) %>%
  mutate("Percentage" = paste0("(", Percentage, ")")) %>%
  unite("Model 4", c("Frequency", "Percentage"), sep = " ")

ftest_1stord_CH_Q <- cbind(
  ftest_1stord_model1t.CH,
  ftest_1stord_model2t.CH[,-1],
  ftest_1stord_model3t.CH[,-1],
  ftest_1stord_model4t.CH[,-1]
)

colnames(ftest_1stord_CH_Q) <- c("p-values", "Model 1", "Model 2", "Model 3", "Model 4")

# BRAZIL

ftest_1stord_model1t.BR <- 
  sum.model1t.br.ssvs$res$p.res %>%
  as.data.frame() %>%
  rownames_to_column(var = "p-values") %>%
  rename("Frequency" = `# p-values`) %>%
  rename("Percentage" = `in %`) %>%
  mutate("Percentage" = paste0("(", Percentage, ")")) %>%
  unite("Model 5", c("Frequency", "Percentage"), sep = " ")

ftest_1stord_model3t.BR <- 
  sum.model3t.br.ssvs$res$p.res %>%
  as.data.frame() %>%
  rownames_to_column(var = "p-values") %>%
  rename("Frequency" = `# p-values`) %>%
  rename("Percentage" = `in %`) %>%
  mutate("Percentage" = paste0("(", Percentage, ")")) %>%
  unite("Model 6", c("Frequency", "Percentage"), sep = " ")

ftest_1stord_BR_Q <- cbind(
  ftest_1stord_model1t.BR,
  ftest_1stord_model3t.BR[,-1]
)

colnames(ftest_1stord_BR_Q) <- c("p-values", "Model 1", "Model 3")

###

dput(ftest_1stord_BR_Q)

colnames(ftest_1stord_BR_Q)
