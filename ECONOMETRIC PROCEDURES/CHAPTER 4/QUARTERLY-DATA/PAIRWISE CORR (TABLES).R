# Average pairwise cross-country correlation of country model residuals

# SWITZERLAND

# MODELS 1 AND 2

aveg_pairwise_corr.model1t.ch.ssvs <- sum.model1t.ch.ssvs$cross.corr$res.res %>%
  as.data.frame() %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "p-values")

aveg_pairwise_corr.model1t.ch.ssvs[] <- lapply(aveg_pairwise_corr.model1t.ch.ssvs, 
                                               function(x) {gsub("%", "\\\\%", x)})

aveg_pairwise_corr.model2t.ch.ssvs <- sum.model2t.ch.ssvs$cross.corr$res.res %>%
  as.data.frame() %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "p-values")

aveg_pairwise_corr.model2t.ch.ssvs[] <- lapply(aveg_pairwise_corr.model2t.ch.ssvs, 
                                               function(x) {gsub("%", "\\\\%", x)})

aveg_pairwise_corr.model12t.ch.ssvs <-
  rbind(aveg_pairwise_corr.model1t.ch.ssvs,
        aveg_pairwise_corr.model2t.ch.ssvs)

dput(aveg_pairwise_corr.model12t.ch.ssvs)

# MODELS 3 AND 4

aveg_pairwise_corr.model3t.ch.ssvs <- sum.model3t.ch.ssvs$cross.corr$res.res %>%
  as.data.frame() %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "p-values")

aveg_pairwise_corr.model3t.ch.ssvs[] <- lapply(aveg_pairwise_corr.model3t.ch.ssvs, 
                                               function(x) {gsub("%", "\\\\%", x)})

aveg_pairwise_corr.model4t.ch.ssvs <- sum.model4t.ch.ssvs$cross.corr$res.res %>%
  as.data.frame() %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "p-values")

aveg_pairwise_corr.model4t.ch.ssvs[] <- lapply(aveg_pairwise_corr.model4t.ch.ssvs, 
                                               function(x) {gsub("%", "\\\\%", x)})

aveg_pairwise_corr.model34t.ch.ssvs <-
  rbind(aveg_pairwise_corr.model3t.ch.ssvs,
        aveg_pairwise_corr.model4t.ch.ssvs)

dput(aveg_pairwise_corr.model34t.ch.ssvs)

# BRAZIL

aveg_pairwise_corr.model1t.br.ssvs <- sum.model1t.br.ssvs$cross.corr$res.res %>%
  as.data.frame() %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "p-values")

aveg_pairwise_corr.model1t.br.ssvs[] <- lapply(aveg_pairwise_corr.model1t.br.ssvs, 
                                               function(x) {gsub("%", "\\\\%", x)})

dput(aveg_pairwise_corr.model1t.br.ssvs)

aveg_pairwise_corr.model3t.br.ssvs <- sum.model3t.br.ssvs$cross.corr$res.res %>%
  as.data.frame() %>%
  rename_all( ~ paste0("$", .x)) %>%
  rename_all( ~ paste0(.x, "$")) %>%
  rownames_to_column(var = "p-values")

aveg_pairwise_corr.model3t.br.ssvs[] <- lapply(aveg_pairwise_corr.model3t.br.ssvs, 
                                               function(x) {gsub("%", "\\\\%", x)})

dput(aveg_pairwise_corr.model3t.br.ssvs)
