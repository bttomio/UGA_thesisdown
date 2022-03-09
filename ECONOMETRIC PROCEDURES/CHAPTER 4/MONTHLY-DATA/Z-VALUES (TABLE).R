library(stringr)
library(tidyverse)

# SWITZERLAND

CD_perc_model5t.ch.svss <- str_extract_all(sum.model5t.ch.ssvs$CD$perc,"\\(?[0-9,.%]+\\)?", simplify = T) %>% 
  str_replace_all(., "\\)", "") %>%
  str_replace_all(., "\\(", "")
CD_perc_model6t.ch.svss <- str_extract_all(sum.model6t.ch.ssvs$CD$perc,"\\(?[0-9,.%]+\\)?", simplify = T) %>% 
  str_replace_all(., "\\)", "") %>%
  str_replace_all(., "\\(", "")

CD_perc_CH <- data.frame("Model 5" = CD_perc_model5t.ch.svss[[4]],
                         "Model 6" = CD_perc_model6t.ch.svss[[4]])

colnames(CD_perc_CH) <- c("Model 5", "Model 6")

# BRAZIL

CD_perc_model5t.br.svss <- str_extract_all(sum.model5t.br.ssvs$CD$perc,"\\(?[0-9,.%]+\\)?", simplify = T) %>% 
  str_replace_all(., "\\)", "") %>%
  str_replace_all(., "\\(", "")
CD_perc_model6t.br.svss <- str_extract_all(sum.model6t.br.ssvs$CD$perc,"\\(?[0-9,.%]+\\)?", simplify = T) %>% 
  str_replace_all(., "\\)", "") %>%
  str_replace_all(., "\\(", "")
CD_perc_model7t.br.svss <- str_extract_all(sum.model7t.br.ssvs$CD$perc,"\\(?[0-9,.%]+\\)?", simplify = T) %>% 
  str_replace_all(., "\\)", "") %>%
  str_replace_all(., "\\(", "")
CD_perc_model8t.br.svss <- str_extract_all(sum.model8t.br.ssvs$CD$perc,"\\(?[0-9,.%]+\\)?", simplify = T) %>% 
  str_replace_all(., "\\)", "") %>%
  str_replace_all(., "\\(", "")

CD_perc_BR <- data.frame("Model 5" = CD_perc_model5t.br.svss[[4]],
                         "Model 6" = CD_perc_model6t.br.svss[[4]],
                         "Model 7" = CD_perc_model7t.br.svss[[4]],
                         "Model 8" = CD_perc_model8t.br.svss[[4]])

colnames(CD_perc_BR) <- c("Model 5", "Model 6", "Model 7", "Model 8")
