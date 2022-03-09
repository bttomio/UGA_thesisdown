# Average pairwise cross-country correlation of country model residuals

library(tidyverse)

# SWITZERLAND

aveg_pairwise_corr.model5t.ch.ssvs <- sum.model5t.ch.ssvs$cross.corr$res.res %>%
  as.data.frame() %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "p-values")

aveg_pairwise_corr.model5t.ch.ssvs[] <- lapply(aveg_pairwise_corr.model5t.ch.ssvs, 
                                               function(x) {gsub("%", "\\\\%", x)})

aveg_pairwise_corr.model6t.ch.ssvs <- sum.model6t.ch.ssvs$cross.corr$res.res %>%
  as.data.frame() %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "p-values")

aveg_pairwise_corr.model6t.ch.ssvs[] <- lapply(aveg_pairwise_corr.model6t.ch.ssvs, 
                                               function(x) {gsub("%", "\\\\%", x)})

aveg_pairwise_corr.model56t.ch.ssvs <-
  rbind(aveg_pairwise_corr.model5t.ch.ssvs,
        aveg_pairwise_corr.model6t.ch.ssvs)

dput(aveg_pairwise_corr.model56t.ch.ssvs)

# BRAZIL

# MODELS 5 AND 6

aveg_pairwise_corr.model5t.br.ssvs <- sum.model5t.br.ssvs$cross.corr$res.res %>%
  as.data.frame() %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "p-values")

aveg_pairwise_corr.model5t.br.ssvs[] <- lapply(aveg_pairwise_corr.model5t.br.ssvs, 
                                               function(x) {gsub("%", "\\\\%", x)})

aveg_pairwise_corr.model6t.br.ssvs <- sum.model6t.br.ssvs$cross.corr$res.res %>%
  as.data.frame() %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "p-values")

aveg_pairwise_corr.model6t.br.ssvs[] <- lapply(aveg_pairwise_corr.model6t.br.ssvs, 
                                               function(x) {gsub("%", "\\\\%", x)})

aveg_pairwise_corr.model56t.br.ssvs <-
  rbind(aveg_pairwise_corr.model5t.br.ssvs,
        aveg_pairwise_corr.model6t.br.ssvs)

dput(aveg_pairwise_corr.model56t.br.ssvs)

# MODELS 7 AND 8

aveg_pairwise_corr.model7t.br.ssvs <- sum.model7t.br.ssvs$cross.corr$res.res %>%
  as.data.frame() %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "p-values")

aveg_pairwise_corr.model7t.br.ssvs[] <- lapply(aveg_pairwise_corr.model7t.br.ssvs, 
                                               function(x) {gsub("%", "\\\\%", x)})

aveg_pairwise_corr.model8t.br.ssvs <- sum.model8t.br.ssvs$cross.corr$res.res %>%
  as.data.frame() %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "p-values")

aveg_pairwise_corr.model8t.br.ssvs[] <- lapply(aveg_pairwise_corr.model8t.br.ssvs, 
                                               function(x) {gsub("%", "\\\\%", x)})

aveg_pairwise_corr.model78t.br.ssvs <-
  rbind(aveg_pairwise_corr.model7t.br.ssvs,
        aveg_pairwise_corr.model8t.br.ssvs)

dput(aveg_pairwise_corr.model78t.br.ssvs)
