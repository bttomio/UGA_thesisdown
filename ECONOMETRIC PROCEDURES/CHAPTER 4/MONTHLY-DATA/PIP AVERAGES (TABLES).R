#Posterior inclusion probabilities across countries, average

library(tidyverse)

# SWITZERLAND

# MODEL 5

PIPAV.model5t.ch.ssvs <- model5t.ch.ssvs$cc.results$PIP$PIP.avg %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

library(glue)
PIPAV.model5t.ch.ssvs$Variable <-  glue("${PIPAV.model5t.ch.ssvs$Variable}$")

PIPAV.model5t.ch.ssvs[] <- lapply(PIPAV.model5t.ch.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPAV.model5t.ch.ssvs[] <- lapply(PIPAV.model5t.ch.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPAV.model5t.ch.ssvs[] <- lapply(PIPAV.model5t.ch.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPAV.model5t.ch.ssvs[] <- lapply(PIPAV.model5t.ch.ssvs, function(x) {gsub("NaN", "", x)})

PIPAV.model5t.ch.ssvs$Variable <- stringr::str_replace(PIPAV.model5t.ch.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPAV.model5t.ch.ssvs)

# MODEL 6

PIPAV.model6t.ch.ssvs <- model6t.ch.ssvs$cc.results$PIP$PIP.avg %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPAV.model6t.ch.ssvs$Variable <-  glue("${PIPAV.model6t.ch.ssvs$Variable}$")

PIPAV.model6t.ch.ssvs[] <- lapply(PIPAV.model6t.ch.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPAV.model6t.ch.ssvs[] <- lapply(PIPAV.model6t.ch.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPAV.model6t.ch.ssvs[] <- lapply(PIPAV.model6t.ch.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPAV.model6t.ch.ssvs[] <- lapply(PIPAV.model6t.ch.ssvs, function(x) {gsub("NaN", "", x)})

PIPAV.model6t.ch.ssvs$Variable <- stringr::str_replace(PIPAV.model6t.ch.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPAV.model6t.ch.ssvs)

# BRAZIL

# MODEL 5

PIPAV.model5t.br.ssvs <- model5t.br.ssvs$cc.results$PIP$PIP.avg %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPAV.model5t.br.ssvs$Variable <-  glue("${PIPAV.model5t.br.ssvs$Variable}$")

PIPAV.model5t.br.ssvs[] <- lapply(PIPAV.model5t.br.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPAV.model5t.br.ssvs[] <- lapply(PIPAV.model5t.br.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPAV.model5t.br.ssvs[] <- lapply(PIPAV.model5t.br.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPAV.model5t.br.ssvs[] <- lapply(PIPAV.model5t.br.ssvs, function(x) {gsub("NaN", "", x)})

PIPAV.model5t.br.ssvs$Variable <- stringr::str_replace(PIPAV.model5t.br.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPAV.model5t.br.ssvs)

# MODEL 6

PIPAV.model6t.br.ssvs <- model6t.br.ssvs$cc.results$PIP$PIP.avg %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPAV.model6t.br.ssvs$Variable <-  glue("${PIPAV.model6t.br.ssvs$Variable}$")

PIPAV.model6t.br.ssvs[] <- lapply(PIPAV.model6t.br.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPAV.model6t.br.ssvs[] <- lapply(PIPAV.model6t.br.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPAV.model6t.br.ssvs[] <- lapply(PIPAV.model6t.br.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPAV.model6t.br.ssvs[] <- lapply(PIPAV.model6t.br.ssvs, function(x) {gsub("NaN", "", x)})

PIPAV.model6t.br.ssvs$Variable <- stringr::str_replace(PIPAV.model6t.br.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPAV.model6t.br.ssvs)

# MODEL 7

PIPAV.model7t.br.ssvs <- model7t.br.ssvs$cc.results$PIP$PIP.avg %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPAV.model7t.br.ssvs$Variable <-  glue("${PIPAV.model7t.br.ssvs$Variable}$")

PIPAV.model7t.br.ssvs[] <- lapply(PIPAV.model7t.br.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPAV.model7t.br.ssvs[] <- lapply(PIPAV.model7t.br.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPAV.model7t.br.ssvs[] <- lapply(PIPAV.model7t.br.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPAV.model7t.br.ssvs[] <- lapply(PIPAV.model7t.br.ssvs, function(x) {gsub("NaN", "", x)})

PIPAV.model7t.br.ssvs$Variable <- stringr::str_replace(PIPAV.model7t.br.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPAV.model7t.br.ssvs)

# MODEL 8

PIPAV.model8t.br.ssvs <- model8t.br.ssvs$cc.results$PIP$PIP.avg %>%
  as.data.frame() %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 2), nsmall = 2))) %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "Variable")

PIPAV.model8t.br.ssvs$Variable <-  glue("${PIPAV.model8t.br.ssvs$Variable}$")

PIPAV.model8t.br.ssvs[] <- lapply(PIPAV.model8t.br.ssvs, function(x) {gsub("lag1", "{t-1}", x)})
#PIPAV.model8t.br.ssvs[] <- lapply(PIPAV.model8t.br.ssvs, function(x) {gsub("lag2", "{t-2}", x)})
PIPAV.model8t.br.ssvs[] <- lapply(PIPAV.model8t.br.ssvs, function(x) {gsub("[*]", "^*", x)})
PIPAV.model8t.br.ssvs[] <- lapply(PIPAV.model8t.br.ssvs, function(x) {gsub("NaN", "", x)})

PIPAV.model8t.br.ssvs$Variable <- stringr::str_replace(PIPAV.model8t.br.ssvs$Variable, '\\^\\*\\^\\*', '\\^{\\*\\*}')

dput(PIPAV.model8t.br.ssvs)
