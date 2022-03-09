library(BGVAR)
library(tidyverse)
library(glue)

# PIP FOR CH ####

# MODEL 5

PIPCH.model5t.ch.ssvs <- model5t.ch.ssvs$cc.results$PIP$PIP.cc$CH %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPCH.model5t.ch.ssvs$Variable <-  glue("${PIPCH.model5t.ch.ssvs$Variable}$")

PIPCH.model5t.ch.ssvs[] <- lapply(PIPCH.model5t.ch.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPCH.model5t.ch.ssvs[] <- lapply(PIPCH.model5t.ch.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPCH.model5t.ch.ssvs[] <- lapply(PIPCH.model5t.ch.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPCH.model5t.ch.ssvs[] <- lapply(PIPCH.model5t.ch.ssvs, function(x) {gsub("NaN", "", x)})

PIPCH.model5t.ch.ssvs$Variable <- stringr::str_replace(PIPCH.model5t.ch.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPCH.model5t.ch.ssvs)

# NP ONLY
# Rows representing the associated explanatory variables
# NP as explanatory to other variables
PIPCHNP.row.model5t.ch.ssvs <- PIPCH.model5t.ch.ssvs[6,] 
# Other variables as explanatory to NP
PIPCHNP.col.model5t.ch.ssvs <- PIPCH.model5t.ch.ssvs[c(0:7),c(1,7)] 
colnames(PIPCHNP.col.model5t.ch.ssvs) <- c("Variable", "Model 5")

# MODEL 6

PIPCH.model6t.ch.ssvs <- model6t.ch.ssvs$cc.results$PIP$PIP.cc$CH %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPCH.model6t.ch.ssvs$Variable <-  glue("${PIPCH.model6t.ch.ssvs$Variable}$")

PIPCH.model6t.ch.ssvs[] <- lapply(PIPCH.model6t.ch.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPCH.model6t.ch.ssvs[] <- lapply(PIPCH.model6t.ch.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPCH.model6t.ch.ssvs[] <- lapply(PIPCH.model6t.ch.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPCH.model6t.ch.ssvs[] <- lapply(PIPCH.model6t.ch.ssvs, function(x) {gsub("NaN", "", x)})

PIPCH.model6t.ch.ssvs$Variable <- stringr::str_replace(PIPCH.model6t.ch.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPCH.model6t.ch.ssvs)

# NP ONLY
# Rows representing the associated explanatory variables
# NP as explanatory to other variables
PIPCHNP.row.model6t.ch.ssvs <- PIPCH.model6t.ch.ssvs[6,] 
# Other variables as explanatory to NP
PIPCHNP.col.model6t.ch.ssvs <- PIPCH.model6t.ch.ssvs[c(0:7),c(1,7)]
colnames(PIPCHNP.col.model6t.ch.ssvs) <- c("Variable", "Model 6")

# MERGE ALL
# NP as explanatory to other variables
PIPCHNP.row.model56t.ch.ssvs <- rbind(PIPCHNP.row.model5t.ch.ssvs, PIPCHNP.row.model6t.ch.ssvs)
PIPCHNP.row.model56t.ch.ssvs <- dplyr::rename(PIPCHNP.row.model56t.ch.ssvs, Model = Variable) 
PIPCHNP.row.model56t.ch.ssvs <- PIPCHNP.row.model56t.ch.ssvs[,c(1:3, 8, 4:7)]
rownames(PIPCHNP.row.model56t.ch.ssvs) <- NULL
PIPCHNP.row.model56t.ch.ssvs[1,1] <- "(5)"
PIPCHNP.row.model56t.ch.ssvs[2,1] <- "(6)"
dput(PIPCHNP.row.model56t.ch.ssvs)
# Other variables as explanatory to NP
PIPCHNP.col.model56t.ch.ssvs <- merge(PIPCHNP.col.model5t.ch.ssvs, PIPCHNP.col.model6t.ch.ssvs, by = "Variable", all = T)
PIPCHNP.col.model56t.ch.ssvs <- PIPCHNP.col.model56t.ch.ssvs[c(7, 4, 6, 3, 2, 1, 5),]
colnames(PIPCHNP.col.model56t.ch.ssvs) <- c("Variable", "(5)", "(6)")
dput(PIPCHNP.col.model56t.ch.ssvs)

# PIP FOR BRAZIL ####

# MODEL 5

PIPCH.model5t.br.ssvs <- model5t.br.ssvs$cc.results$PIP$PIP.cc$BR %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPCH.model5t.br.ssvs$Variable <-  glue("${PIPCH.model5t.br.ssvs$Variable}$")

PIPCH.model5t.br.ssvs[] <- lapply(PIPCH.model5t.br.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPCH.model5t.br.ssvs[] <- lapply(PIPCH.model5t.br.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPCH.model5t.br.ssvs[] <- lapply(PIPCH.model5t.br.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPCH.model5t.br.ssvs[] <- lapply(PIPCH.model5t.br.ssvs, function(x) {gsub("NaN", "", x)})

PIPCH.model5t.br.ssvs$Variable <- stringr::str_replace(PIPCH.model5t.br.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPCH.model5t.br.ssvs)

# NP ONLY
# Rows representing the associated explanatory variables
# NP as explanatory to other variables
PIPCHNP.row.model5t.br.ssvs <- PIPCH.model5t.br.ssvs[6,] 
# Other variables as explanatory to NP
PIPCHNP.col.model5t.br.ssvs <- PIPCH.model5t.br.ssvs[c(0:7),c(1,7)] 
colnames(PIPCHNP.col.model5t.br.ssvs) <- c("Variable", "Model 5")

# MODEL 6

PIPCH.model6t.br.ssvs <- model6t.br.ssvs$cc.results$PIP$PIP.cc$BR %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPCH.model6t.br.ssvs$Variable <-  glue("${PIPCH.model6t.br.ssvs$Variable}$")

PIPCH.model6t.br.ssvs[] <- lapply(PIPCH.model6t.br.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPCH.model6t.br.ssvs[] <- lapply(PIPCH.model6t.br.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPCH.model6t.br.ssvs[] <- lapply(PIPCH.model6t.br.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPCH.model6t.br.ssvs[] <- lapply(PIPCH.model6t.br.ssvs, function(x) {gsub("NaN", "", x)})

PIPCH.model6t.br.ssvs$Variable <- stringr::str_replace(PIPCH.model6t.br.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPCH.model6t.br.ssvs)

# NP ONLY
# Rows representing the associated explanatory variables
# NP as explanatory to other variables
PIPCHNP.row.model6t.br.ssvs <- PIPCH.model6t.br.ssvs[6,] 
# Other variables as explanatory to NP
PIPCHNP.col.model6t.br.ssvs <- PIPCH.model6t.br.ssvs[c(0:7),c(1,7)]
colnames(PIPCHNP.col.model6t.br.ssvs) <- c("Variable", "Model 6")

# MODEL 7

PIPCH.model7t.br.ssvs <- model7t.br.ssvs$cc.results$PIP$PIP.cc$BR %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPCH.model7t.br.ssvs$Variable <-  glue("${PIPCH.model7t.br.ssvs$Variable}$")

PIPCH.model7t.br.ssvs[] <- lapply(PIPCH.model7t.br.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPCH.model7t.br.ssvs[] <- lapply(PIPCH.model7t.br.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPCH.model7t.br.ssvs[] <- lapply(PIPCH.model7t.br.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPCH.model7t.br.ssvs[] <- lapply(PIPCH.model7t.br.ssvs, function(x) {gsub("NaN", "", x)})

PIPCH.model7t.br.ssvs$Variable <- stringr::str_replace(PIPCH.model7t.br.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPCH.model7t.br.ssvs)

# NP ONLY
# Rows representing the associated explanatory variables
# NP as explanatory to other variables
PIPCHNP.row.model7t.br.ssvs <- PIPCH.model7t.br.ssvs[6,] 
# Other variables as explanatory to NP
PIPCHNP.col.model7t.br.ssvs <- PIPCH.model7t.br.ssvs[c(0:7, 26),c(1,7)]
colnames(PIPCHNP.col.model7t.br.ssvs) <- c("Variable", "Model 7")

# MODEL 8

PIPCH.model8t.br.ssvs <- model8t.br.ssvs$cc.results$PIP$PIP.cc$BR %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPCH.model8t.br.ssvs$Variable <-  glue("${PIPCH.model8t.br.ssvs$Variable}$")

PIPCH.model8t.br.ssvs[] <- lapply(PIPCH.model8t.br.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPCH.model8t.br.ssvs[] <- lapply(PIPCH.model8t.br.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPCH.model8t.br.ssvs[] <- lapply(PIPCH.model8t.br.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPCH.model8t.br.ssvs[] <- lapply(PIPCH.model8t.br.ssvs, function(x) {gsub("NaN", "", x)})

PIPCH.model8t.br.ssvs$Variable <- stringr::str_replace(PIPCH.model8t.br.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPCH.model8t.br.ssvs)

# NP ONLY
# Rows representing the associated explanatory variables
# NP as explanatory to other variables
PIPCHNP.row.model8t.br.ssvs <- PIPCH.model8t.br.ssvs[6,] 
# Other variables as explanatory to NP
PIPCHNP.col.model8t.br.ssvs <- PIPCH.model8t.br.ssvs[c(0:7, 26),c(1,7)]
colnames(PIPCHNP.col.model8t.br.ssvs) <- c("Variable", "Model 8")

# MERGE ALL
# NP as explanatory to other variables
PIPCHNP.row.model5678t.br.ssvs <- rbind(PIPCHNP.row.model5t.br.ssvs, PIPCHNP.row.model6t.br.ssvs)
PIPCHNP.row.model5678t.br.ssvs <- bind_rows(PIPCHNP.row.model5678t.br.ssvs, PIPCHNP.row.model7t.br.ssvs)
PIPCHNP.row.model5678t.br.ssvs <- bind_rows(PIPCHNP.row.model5678t.br.ssvs, PIPCHNP.row.model8t.br.ssvs)
PIPCHNP.row.model5678t.br.ssvs <- dplyr::rename(PIPCHNP.row.model5678t.br.ssvs, Model = Variable) 
PIPCHNP.row.model5678t.br.ssvs <- PIPCHNP.row.model5678t.br.ssvs[,c(1:3, 8, 4:6, 7, 9)]
rownames(PIPCHNP.row.model5678t.br.ssvs) <- NULL
PIPCHNP.row.model5678t.br.ssvs[1,1] <- "(5)"
PIPCHNP.row.model5678t.br.ssvs[2,1] <- "(6)"
PIPCHNP.row.model5678t.br.ssvs[3,1] <- "(7)"
PIPCHNP.row.model5678t.br.ssvs[4,1] <- "(8)"
dput(PIPCHNP.row.model5678t.br.ssvs)
# Other variables as explanatory to NP
PIPCHNP.col.model5678t.br.ssvs <- merge(PIPCHNP.col.model5t.br.ssvs, PIPCHNP.col.model6t.br.ssvs, by = "Variable", all = T)
PIPCHNP.col.model5678t.br.ssvs <- merge(PIPCHNP.col.model5678t.br.ssvs, PIPCHNP.col.model7t.br.ssvs, by = "Variable", all = T)
PIPCHNP.col.model5678t.br.ssvs <- merge(PIPCHNP.col.model5678t.br.ssvs, PIPCHNP.col.model8t.br.ssvs, by = "Variable", all = T)
PIPCHNP.col.model5678t.br.ssvs <- PIPCHNP.col.model5678t.br.ssvs[c(8, 5, 7, 4, 2, 1, 6, 3),]
colnames(PIPCHNP.col.model5678t.br.ssvs) <- c("Variable", "(5)", "(6)", "(7)", "(8)")
dput(PIPCHNP.col.model5678t.br.ssvs)
