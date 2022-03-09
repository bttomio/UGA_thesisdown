library(BGVAR)
library(tidyverse)
library(glue)

# PIP FOR CH ####

# MODEL 1

PIPCH.model1t.ch.ssvs <- model1t.ch.ssvs$cc.results$PIP$PIP.cc$CH %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPCH.model1t.ch.ssvs$Variable <-  glue("${PIPCH.model1t.ch.ssvs$Variable}$")

PIPCH.model1t.ch.ssvs[] <- lapply(PIPCH.model1t.ch.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPCH.model1t.ch.ssvs[] <- lapply(PIPCH.model1t.ch.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPCH.model1t.ch.ssvs[] <- lapply(PIPCH.model1t.ch.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPCH.model1t.ch.ssvs[] <- lapply(PIPCH.model1t.ch.ssvs, function(x) {gsub("NaN", "", x)})

PIPCH.model1t.ch.ssvs$Variable <- stringr::str_replace(PIPCH.model1t.ch.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPCH.model1t.ch.ssvs)

# NP ONLY
# Rows representing the associated explanatory variables
# NP as explanatory to other variables
PIPCHNP.row.model1t.ch.ssvs <- PIPCH.model1t.ch.ssvs[5,] 
# Other variables as explanatory to NP
PIPCHNP.col.model1t.ch.ssvs <- PIPCH.model1t.ch.ssvs[c(0:6),c(1,6)] 
colnames(PIPCHNP.col.model1t.ch.ssvs) <- c("Variable", "Model 1")

# MODEL 2

PIPCH.model2t.ch.ssvs <- model2t.ch.ssvs$cc.results$PIP$PIP.cc$CH %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPCH.model2t.ch.ssvs$Variable <-  glue("${PIPCH.model2t.ch.ssvs$Variable}$")

PIPCH.model2t.ch.ssvs[] <- lapply(PIPCH.model2t.ch.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPCH.model2t.ch.ssvs[] <- lapply(PIPCH.model2t.ch.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPCH.model2t.ch.ssvs[] <- lapply(PIPCH.model2t.ch.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPCH.model2t.ch.ssvs[] <- lapply(PIPCH.model2t.ch.ssvs, function(x) {gsub("NaN", "", x)})

PIPCH.model2t.ch.ssvs$Variable <- stringr::str_replace(PIPCH.model2t.ch.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPCH.model2t.ch.ssvs)

# NP ONLY
# Rows representing the associated explanatory variables
# NP as explanatory to other variables
PIPCHNP.row.model2t.ch.ssvs <- PIPCH.model2t.ch.ssvs[5,] 
# Other variables as explanatory to NP
PIPCHNP.col.model2t.ch.ssvs <- PIPCH.model2t.ch.ssvs[c(0:6),c(1,6)]
colnames(PIPCHNP.col.model2t.ch.ssvs) <- c("Variable", "Model 2")

# MODEL 3

PIPCH.model3t.ch.ssvs <- model3t.ch.ssvs$cc.results$PIP$PIP.cc$CH %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPCH.model3t.ch.ssvs$Variable <-  glue("${PIPCH.model3t.ch.ssvs$Variable}$")

PIPCH.model3t.ch.ssvs[] <- lapply(PIPCH.model3t.ch.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPCH.model3t.ch.ssvs[] <- lapply(PIPCH.model3t.ch.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPCH.model3t.ch.ssvs[] <- lapply(PIPCH.model3t.ch.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPCH.model3t.ch.ssvs[] <- lapply(PIPCH.model3t.ch.ssvs, function(x) {gsub("NaN", "", x)})

PIPCH.model3t.ch.ssvs$Variable <- stringr::str_replace(PIPCH.model3t.ch.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPCH.model3t.ch.ssvs)

# NP ONLY
# Rows representing the associated explanatory variables
# NP as explanatory to other variables
PIPCHNP.row.model3t.ch.ssvs <- PIPCH.model3t.ch.ssvs[8,] 
# Other variables as explanatory to NP
PIPCHNP.col.model3t.ch.ssvs <- PIPCH.model3t.ch.ssvs[c(0:9),c(1,9)]
colnames(PIPCHNP.col.model3t.ch.ssvs) <- c("Variable", "Model 3")

# MODEL 4

PIPCH.model4t.ch.ssvs <- model4t.ch.ssvs$cc.results$PIP$PIP.cc$CH %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPCH.model4t.ch.ssvs$Variable <-  glue("${PIPCH.model4t.ch.ssvs$Variable}$")

PIPCH.model4t.ch.ssvs[] <- lapply(PIPCH.model4t.ch.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPCH.model4t.ch.ssvs[] <- lapply(PIPCH.model4t.ch.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPCH.model4t.ch.ssvs[] <- lapply(PIPCH.model4t.ch.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPCH.model4t.ch.ssvs[] <- lapply(PIPCH.model4t.ch.ssvs, function(x) {gsub("NaN", "", x)})

PIPCH.model4t.ch.ssvs$Variable <- stringr::str_replace(PIPCH.model4t.ch.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPCH.model4t.ch.ssvs)

# NP ONLY
# Rows representing the associated explanatory variables
# NP as explanatory to other variables
PIPCHNP.row.model4t.ch.ssvs <- PIPCH.model4t.ch.ssvs[8,] 
# Other variables as explanatory to NP
PIPCHNP.col.model4t.ch.ssvs <- PIPCH.model4t.ch.ssvs[c(0:9),c(1,9)]
colnames(PIPCHNP.col.model4t.ch.ssvs) <- c("Variable", "Model 4")

# MERGE ALL
# NP as explanatory to other variables
PIPCHNP.row.model1234t.ch.ssvs <- rbind(PIPCHNP.row.model1t.ch.ssvs, PIPCHNP.row.model2t.ch.ssvs)
PIPCHNP.row.model1234t.ch.ssvs <- bind_rows(PIPCHNP.row.model1234t.ch.ssvs, PIPCHNP.row.model3t.ch.ssvs)
PIPCHNP.row.model1234t.ch.ssvs <- bind_rows(PIPCHNP.row.model1234t.ch.ssvs, PIPCHNP.row.model4t.ch.ssvs)
PIPCHNP.row.model1234t.ch.ssvs <- dplyr::rename(PIPCHNP.row.model1234t.ch.ssvs, Model = Variable) 
PIPCHNP.row.model1234t.ch.ssvs <- PIPCHNP.row.model1234t.ch.ssvs[,c(1:2, 8, 9, 11, 10, 7, 3:6)]
rownames(PIPCHNP.row.model1234t.ch.ssvs) <- NULL
PIPCHNP.row.model1234t.ch.ssvs[1,1] <- "(1)"
PIPCHNP.row.model1234t.ch.ssvs[2,1] <- "(2)"
PIPCHNP.row.model1234t.ch.ssvs[3,1] <- "(3)"
PIPCHNP.row.model1234t.ch.ssvs[4,1] <- "(4)"
dput(PIPCHNP.row.model1234t.ch.ssvs)
# Other variables as explanatory to NP
PIPCHNP.col.model1234t.ch.ssvs <- merge(PIPCHNP.col.model1t.ch.ssvs, PIPCHNP.col.model2t.ch.ssvs, by = "Variable", all = T)
PIPCHNP.col.model1234t.ch.ssvs <- merge(PIPCHNP.col.model1234t.ch.ssvs, PIPCHNP.col.model3t.ch.ssvs, by = "Variable", all = T)
PIPCHNP.col.model1234t.ch.ssvs <- merge(PIPCHNP.col.model1234t.ch.ssvs, PIPCHNP.col.model4t.ch.ssvs, by = "Variable", all = T)
PIPCHNP.col.model1234t.ch.ssvs <- PIPCHNP.col.model1234t.ch.ssvs[c(4, 1, 5, 10, 7, 9, 6, 3, 2, 8),]
colnames(PIPCHNP.col.model1234t.ch.ssvs) <- c("Variable", "(1)", "(2)", "(3)", "(4)")
dput(PIPCHNP.col.model1234t.ch.ssvs)

# PIP FOR BRAZIL ####

# MODEL 1

PIPCH.model1t.br.ssvs <- model1t.br.ssvs$cc.results$PIP$PIP.cc$BR %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPCH.model1t.br.ssvs$Variable <-  glue("${PIPCH.model1t.br.ssvs$Variable}$")

PIPCH.model1t.br.ssvs[] <- lapply(PIPCH.model1t.br.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPCH.model1t.br.ssvs[] <- lapply(PIPCH.model1t.br.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPCH.model1t.br.ssvs[] <- lapply(PIPCH.model1t.br.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPCH.model1t.br.ssvs[] <- lapply(PIPCH.model1t.br.ssvs, function(x) {gsub("NaN", "", x)})

PIPCH.model1t.br.ssvs$Variable <- stringr::str_replace(PIPCH.model1t.br.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPCH.model1t.br.ssvs)

# NP ONLY
# Rows representing the associated explanatory variables
# NP as explanatory to other variables
PIPCHNP.row.model1t.br.ssvs <- PIPCH.model1t.br.ssvs[5,] 
# Other variables as explanatory to NP
PIPCHNP.col.model1t.br.ssvs <- PIPCH.model1t.br.ssvs[c(0:6),c(1,6)] 
colnames(PIPCHNP.col.model1t.br.ssvs) <- c("Variable", "Model 1")

# MODEL 3

PIPCH.model3t.br.ssvs <- model3t.br.ssvs$cc.results$PIP$PIP.cc$BR %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPCH.model3t.br.ssvs$Variable <-  glue("${PIPCH.model3t.br.ssvs$Variable}$")

PIPCH.model3t.br.ssvs[] <- lapply(PIPCH.model3t.br.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPCH.model3t.br.ssvs[] <- lapply(PIPCH.model3t.br.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPCH.model3t.br.ssvs[] <- lapply(PIPCH.model3t.br.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPCH.model3t.br.ssvs[] <- lapply(PIPCH.model3t.br.ssvs, function(x) {gsub("NaN", "", x)})

PIPCH.model3t.br.ssvs$Variable <- stringr::str_replace(PIPCH.model3t.br.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPCH.model3t.br.ssvs)

# NP ONLY
# Rows representing the associated explanatory variables
# NP as explanatory to other variables
PIPCHNP.row.model3t.br.ssvs <- PIPCH.model3t.br.ssvs[8,] 
# Other variables as explanatory to NP
PIPCHNP.col.model3t.br.ssvs <- PIPCH.model3t.br.ssvs[c(0:9),c(1,9)]
colnames(PIPCHNP.col.model3t.br.ssvs) <- c("Variable", "Model 3")

# MERGE ALL
# NP as explanatory to other variables
PIPCHNP.row.model13t.br.ssvs <- bind_rows(PIPCHNP.row.model1t.br.ssvs, PIPCHNP.row.model3t.br.ssvs)
PIPCHNP.row.model13t.br.ssvs <- dplyr::rename(PIPCHNP.row.model13t.br.ssvs, Model = Variable) 
PIPCHNP.row.model13t.br.ssvs <- PIPCHNP.row.model13t.br.ssvs[,c(1:2, 8, 9, 11, 10, 7, 3:6)]
rownames(PIPCHNP.row.model13t.br.ssvs) <- NULL
PIPCHNP.row.model13t.br.ssvs[1,1] <- "(1)"
PIPCHNP.row.model13t.br.ssvs[2,1] <- "(3)"
dput(PIPCHNP.row.model13t.br.ssvs)
# Other variables as explanatory to NP
PIPCHNP.col.model13t.br.ssvs <- merge(PIPCHNP.col.model1t.br.ssvs, PIPCHNP.col.model3t.br.ssvs, by = "Variable", all = T)
PIPCHNP.col.model13t.br.ssvs <- PIPCHNP.col.model13t.br.ssvs[c(4, 1, 5, 10, 7, 9, 6, 3, 2, 8),]
colnames(PIPCHNP.col.model13t.br.ssvs) <- c("Variable", "(1)", "(3)")
dput(PIPCHNP.col.model13t.br.ssvs)
