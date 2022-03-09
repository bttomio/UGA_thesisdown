library(stringr)
library(tidyverse)

# SWITZERLAND

CD_perc_model1t.ch.svss <- str_extract_all(sum.model1t.ch.ssvs$CD$perc,"\\(?[0-9,.%]+\\)?", simplify = T) %>% 
  str_replace_all(., "\\)", "") %>%
  str_replace_all(., "\\(", "")
CD_perc_model2t.ch.svss <- str_extract_all(sum.model2t.ch.ssvs$CD$perc,"\\(?[0-9,.%]+\\)?", simplify = T) %>% 
  str_replace_all(., "\\)", "") %>%
  str_replace_all(., "\\(", "")
CD_perc_model3t.ch.svss <- str_extract_all(sum.model3t.ch.ssvs$CD$perc,"\\(?[0-9,.%]+\\)?", simplify = T) %>% 
  str_replace_all(., "\\)", "") %>%
  str_replace_all(., "\\(", "")
CD_perc_model4t.ch.svss <- str_extract_all(sum.model4t.ch.ssvs$CD$perc,"\\(?[0-9,.%]+\\)?", simplify = T) %>% 
  str_replace_all(., "\\)", "") %>%
  str_replace_all(., "\\(", "")

CD_perc_CH <- data.frame("Model 1" = CD_perc_model1t.ch.svss[[4]],
                         "Model 2" = CD_perc_model2t.ch.svss[[4]],
                         "Model 3" = CD_perc_model3t.ch.svss[[4]],
                         "Model 4" = CD_perc_model4t.ch.svss[[4]])

colnames(CD_perc_CH) <- c("Model 1", "Model 2", "Model 3", "Model 4")

# BRAZIL

CD_perc_model1t.br.svss <- str_extract_all(sum.model1t.br.ssvs$CD$perc,"\\(?[0-9,.%]+\\)?", simplify = T) %>% 
  str_replace_all(., "\\)", "") %>%
  str_replace_all(., "\\(", "")
CD_perc_model3t.br.svss <- str_extract_all(sum.model3t.br.ssvs$CD$perc,"\\(?[0-9,.%]+\\)?", simplify = T) %>% 
  str_replace_all(., "\\)", "") %>%
  str_replace_all(., "\\(", "")

CD_perc_BR <- data.frame("Model 1" = CD_perc_model1t.br.svss[[4]],
                         "Model 3" = CD_perc_model3t.br.svss[[4]])

colnames(CD_perc_BR) <- c("Model 1", "Model 3")
