#Posterior inclusion probabilities across countries, average

# SWITZERLAND

# MODEL 1

PIPAV.model1t.ch.ssvs <- model1t.ch.ssvs$cc.results$PIP$PIP.avg %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

library(glue)
PIPAV.model1t.ch.ssvs$Variable <-  glue("${PIPAV.model1t.ch.ssvs$Variable}$")

PIPAV.model1t.ch.ssvs[] <- lapply(PIPAV.model1t.ch.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPAV.model1t.ch.ssvs[] <- lapply(PIPAV.model1t.ch.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPAV.model1t.ch.ssvs[] <- lapply(PIPAV.model1t.ch.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPAV.model1t.ch.ssvs[] <- lapply(PIPAV.model1t.ch.ssvs, function(x) {gsub("NaN", "", x)})

PIPAV.model1t.ch.ssvs$Variable <- stringr::str_replace(PIPAV.model1t.ch.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPAV.model1t.ch.ssvs)

# MODEL 2

PIPAV.model2t.ch.ssvs <- model2t.ch.ssvs$cc.results$PIP$PIP.avg %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPAV.model2t.ch.ssvs$Variable <-  glue("${PIPAV.model2t.ch.ssvs$Variable}$")

PIPAV.model2t.ch.ssvs[] <- lapply(PIPAV.model2t.ch.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPAV.model2t.ch.ssvs[] <- lapply(PIPAV.model2t.ch.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPAV.model2t.ch.ssvs[] <- lapply(PIPAV.model2t.ch.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPAV.model2t.ch.ssvs[] <- lapply(PIPAV.model2t.ch.ssvs, function(x) {gsub("NaN", "", x)})

PIPAV.model2t.ch.ssvs$Variable <- stringr::str_replace(PIPAV.model2t.ch.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPAV.model2t.ch.ssvs)

# MODEL 3

PIPAV.model3t.ch.ssvs <- model3t.ch.ssvs$cc.results$PIP$PIP.avg %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPAV.model3t.ch.ssvs$Variable <-  glue("${PIPAV.model3t.ch.ssvs$Variable}$")

PIPAV.model3t.ch.ssvs[] <- lapply(PIPAV.model3t.ch.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPAV.model3t.ch.ssvs[] <- lapply(PIPAV.model3t.ch.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPAV.model3t.ch.ssvs[] <- lapply(PIPAV.model3t.ch.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPAV.model3t.ch.ssvs[] <- lapply(PIPAV.model3t.ch.ssvs, function(x) {gsub("NaN", "", x)})

PIPAV.model3t.ch.ssvs$Variable <- stringr::str_replace(PIPAV.model3t.ch.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPAV.model3t.ch.ssvs)

# MODEL 4

PIPAV.model4t.ch.ssvs <- model4t.ch.ssvs$cc.results$PIP$PIP.avg %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPAV.model4t.ch.ssvs$Variable <-  glue("${PIPAV.model4t.ch.ssvs$Variable}$")

PIPAV.model4t.ch.ssvs[] <- lapply(PIPAV.model4t.ch.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPAV.model4t.ch.ssvs[] <- lapply(PIPAV.model4t.ch.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPAV.model4t.ch.ssvs[] <- lapply(PIPAV.model4t.ch.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPAV.model4t.ch.ssvs[] <- lapply(PIPAV.model4t.ch.ssvs, function(x) {gsub("NaN", "", x)})

PIPAV.model4t.ch.ssvs$Variable <- stringr::str_replace(PIPAV.model4t.ch.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPAV.model4t.ch.ssvs)

# BRAZIL

# MODEL 1

PIPAV.model1t.br.ssvs <- model1t.br.ssvs$cc.results$PIP$PIP.avg %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPAV.model1t.br.ssvs$Variable <-  glue("${PIPAV.model1t.br.ssvs$Variable}$")

PIPAV.model1t.br.ssvs[] <- lapply(PIPAV.model1t.br.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPAV.model1t.br.ssvs[] <- lapply(PIPAV.model1t.br.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPAV.model1t.br.ssvs[] <- lapply(PIPAV.model1t.br.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPAV.model1t.br.ssvs[] <- lapply(PIPAV.model1t.br.ssvs, function(x) {gsub("NaN", "", x)})

PIPAV.model1t.br.ssvs$Variable <- stringr::str_replace(PIPAV.model1t.br.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPAV.model1t.br.ssvs)

# MODEL 3

PIPAV.model3t.br.ssvs <- model3t.br.ssvs$cc.results$PIP$PIP.avg %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPAV.model3t.br.ssvs$Variable <-  glue("${PIPAV.model3t.br.ssvs$Variable}$")

PIPAV.model3t.br.ssvs[] <- lapply(PIPAV.model3t.br.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPAV.model3t.br.ssvs[] <- lapply(PIPAV.model3t.br.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPAV.model3t.br.ssvs[] <- lapply(PIPAV.model3t.br.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPAV.model3t.br.ssvs[] <- lapply(PIPAV.model3t.br.ssvs, function(x) {gsub("NaN", "", x)})

PIPAV.model3t.br.ssvs$Variable <- stringr::str_replace(PIPAV.model3t.br.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPAV.model3t.br.ssvs)
