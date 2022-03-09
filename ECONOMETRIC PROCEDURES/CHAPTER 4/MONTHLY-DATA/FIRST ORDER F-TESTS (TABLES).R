#F-test, first order serial autocorrelation of cross-country residuals
#Summary statistics:

library(tidyverse)

#SWITZERLAND

ftest_1stord_model5t.CH <- 
  sum.model5t.ch.ssvs$res$p.res %>%
  as.data.frame() %>%
  rownames_to_column(var = "p-values") %>%
  rename("Frequency" = `# p-values`) %>%
  rename("Percentage" = `in %`) %>%
  mutate("Percentage" = paste0("(", Percentage, ")")) %>%
  unite("Model 5", c("Frequency", "Percentage"), sep = " ")

ftest_1stord_model6t.CH <- 
  sum.model6t.ch.ssvs$res$p.res %>%
  as.data.frame() %>%
  rownames_to_column(var = "p-values") %>%
  rename("Frequency" = `# p-values`) %>%
  rename("Percentage" = `in %`) %>%
  mutate("Percentage" = paste0("(", Percentage, ")")) %>%
  unite("Model 6", c("Frequency", "Percentage"), sep = " ")

ftest_1stord_CH_M <- cbind(
  ftest_1stord_model5t.CH,
  ftest_1stord_model6t.CH[,-1]
)

colnames(ftest_1stord_CH_M) <- c("p-values", "Model 5", "Model 6")

# BRAZIL

ftest_1stord_model5t.BR <- 
  sum.model5t.br.ssvs$res$p.res %>%
  as.data.frame() %>%
  rownames_to_column(var = "p-values") %>%
  rename("Frequency" = `# p-values`) %>%
  rename("Percentage" = `in %`) %>%
  mutate("Percentage" = paste0("(", Percentage, ")")) %>%
  unite("Model 5", c("Frequency", "Percentage"), sep = " ")

ftest_1stord_model6t.BR <- 
  sum.model6t.br.ssvs$res$p.res %>%
  as.data.frame() %>%
  rownames_to_column(var = "p-values") %>%
  rename("Frequency" = `# p-values`) %>%
  rename("Percentage" = `in %`) %>%
  mutate("Percentage" = paste0("(", Percentage, ")")) %>%
  unite("Model 6", c("Frequency", "Percentage"), sep = " ")

ftest_1stord_model7t.BR <- 
  sum.model7t.br.ssvs$res$p.res %>%
  as.data.frame() %>%
  rownames_to_column(var = "p-values") %>%
  rename("Frequency" = `# p-values`) %>%
  rename("Percentage" = `in %`) %>%
  mutate("Percentage" = paste0("(", Percentage, ")")) %>%
  unite("Model 7", c("Frequency", "Percentage"), sep = " ")

ftest_1stord_model8t.BR <- 
  sum.model8t.br.ssvs$res$p.res %>%
  as.data.frame() %>%
  rownames_to_column(var = "p-values") %>%
  rename("Frequency" = `# p-values`) %>%
  rename("Percentage" = `in %`) %>%
  mutate("Percentage" = paste0("(", Percentage, ")")) %>%
  unite("Model 8", c("Frequency", "Percentage"), sep = " ")

ftest_1stord_BR_M <- cbind(
  ftest_1stord_model5t.BR,
  ftest_1stord_model6t.BR[,-1],
  ftest_1stord_model7t.BR[,-1],
  ftest_1stord_model8t.BR[,-1]
)

colnames(ftest_1stord_BR_M) <- c("p-values", "Model 5", "Model 6", "Model 7", "Model 8")

###

dput(ftest_1stord_BR_M)

colnames(ftest_1stord_BR_M)
