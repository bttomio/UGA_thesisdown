# TABLE WITH VARIABLES USED

# MODEL REPORT

library(BGVAR)
library(tidyverse)

# SWITZERLAND

# MODEL 5

endo <- lapply(model5t.ch.ssvs$cc.results$coeffs,colnames)
exo  <- lapply(model5t.ch.ssvs$cc.results$coeffs,rownames)
#EXCLUDED EXO (TO GET THEM, RECOPY THE FUNCTION - PRINT.BGVAR)
cN   <- names(endo)

vars <- list()
for(i in 1:length(endo)){
  vars[[i]] <- gsub(paste(cN[i],".",sep=""),"",endo[[i]])
}

varNames <- lapply(vars,function(l) paste(l,collapse=", "))
names(varNames) <- cN
varNames <- unlist(varNames) %>%
  data.frame()

library(splitstackshape)
varNamessplit <- cSplit(varNames, ".", ",") %>%
  data.frame()

rownames(varNamessplit) <- rownames(varNames)

colnames(varNamessplit) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7")

varNamessplit[] <- lapply(varNamessplit, function(x) {replace_na(x, "")})
varNamessplit[] <- lapply(varNamessplit, function(x) {paste0("$", x, "$")})
varNamessplit[] <- lapply(varNamessplit, function(x) {gsub("\\$\\$", "", x)})

modspec.model5t.ch.ssvs <- varNamessplit %>%
  mutate_all(list(~na_if(.,""))) %>%
  unite("Domestic variables", V1:V7, sep = ", ", remove = T, na.rm = T) %>%
  rownames_to_column("Unit name")

dput(modspec.model5t.ch.ssvs)

table(unlist(modspec.model5t.ch.ssvs))

modspec.model5t.ch.ssvs %>%
  na.omit %>%
  summarise_all(n_distinct)

# MODEL 6

endo <- lapply(model6t.ch.ssvs$cc.results$coeffs,colnames)
exo  <- lapply(model6t.ch.ssvs$cc.results$coeffs,rownames)
#EXCLUDED EXO (TO GET THEM, RECOPY THE FUNCTION - PRINT.BGVAR)
cN   <- names(endo)

vars <- list()
for(i in 1:length(endo)){
  vars[[i]] <- gsub(paste(cN[i],".",sep=""),"",endo[[i]])
}

varNames <- lapply(vars,function(l) paste(l,collapse=", "))
names(varNames) <- cN
varNames <- unlist(varNames) %>%
  data.frame()

library(splitstackshape)
varNamessplit <- cSplit(varNames, ".", ",") %>%
  data.frame()

rownames(varNamessplit) <- rownames(varNames)

colnames(varNamessplit) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7")

varNamessplit[] <- lapply(varNamessplit, function(x) {replace_na(x, "")})
varNamessplit[] <- lapply(varNamessplit, function(x) {paste0("$", x, "$")})
varNamessplit[] <- lapply(varNamessplit, function(x) {gsub("\\$\\$", "", x)})

modspec.model6t.ch.ssvs <- varNamessplit %>%
  mutate_all(list(~na_if(.,""))) %>%
  unite("Domestic variables", V1:V7, sep = ", ", remove = T, na.rm = T) %>%
  rownames_to_column("Unit name")

dput(modspec.model6t.ch.ssvs)

table(unlist(modspec.model6t.ch.ssvs))

modspec.model6t.ch.ssvs %>%
  na.omit %>%
  summarise_all(n_distinct)

# BRAZIL

# MODEL 5

endo <- lapply(model5t.br.ssvs$cc.results$coeffs,colnames)
exo  <- lapply(model5t.br.ssvs$cc.results$coeffs,rownames)
#EXCLUDED EXO (TO GET THEM, RECOPY THE FUNCTION - PRINT.BGVAR)
cN   <- names(endo)

vars <- list()
for(i in 1:length(endo)){
  vars[[i]] <- gsub(paste(cN[i],".",sep=""),"",endo[[i]])
}

varNames <- lapply(vars,function(l) paste(l,collapse=", "))
names(varNames) <- cN
varNames <- unlist(varNames) %>%
  data.frame()

library(splitstackshape)
varNamessplit <- cSplit(varNames, ".", ",") %>%
  data.frame()

rownames(varNamessplit) <- rownames(varNames)

colnames(varNamessplit) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7")

varNamessplit[] <- lapply(varNamessplit, function(x) {replace_na(x, "")})
varNamessplit[] <- lapply(varNamessplit, function(x) {paste0("$", x, "$")})
varNamessplit[] <- lapply(varNamessplit, function(x) {gsub("\\$\\$", "", x)})

modspec.model5t.br.ssvs <- varNamessplit %>%
  mutate_all(list(~na_if(.,""))) %>%
  unite("Domestic variables", V1:V7, sep = ", ", remove = T, na.rm = T) %>%
  rownames_to_column("Unit name")

dput(modspec.model5t.br.ssvs)

table(unlist(modspec.model5t.br.ssvs))

modspec.model5t.br.ssvs %>%
  na.omit %>%
  summarise_all(n_distinct)

# MODEL 6

endo <- lapply(model6t.br.ssvs$cc.results$coeffs,colnames)
exo  <- lapply(model6t.br.ssvs$cc.results$coeffs,rownames)
#EXCLUDED EXO (TO GET THEM, RECOPY THE FUNCTION - PRINT.BGVAR)
cN   <- names(endo)

vars <- list()
for(i in 1:length(endo)){
  vars[[i]] <- gsub(paste(cN[i],".",sep=""),"",endo[[i]])
}

varNames <- lapply(vars,function(l) paste(l,collapse=", "))
names(varNames) <- cN
varNames <- unlist(varNames) %>%
  data.frame()

library(splitstackshape)
varNamessplit <- cSplit(varNames, ".", ",") %>%
  data.frame()

rownames(varNamessplit) <- rownames(varNames)

colnames(varNamessplit) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7")

varNamessplit[] <- lapply(varNamessplit, function(x) {replace_na(x, "")})
varNamessplit[] <- lapply(varNamessplit, function(x) {paste0("$", x, "$")})
varNamessplit[] <- lapply(varNamessplit, function(x) {gsub("\\$\\$", "", x)})

modspec.model6t.br.ssvs <- varNamessplit %>%
  mutate_all(list(~na_if(.,""))) %>%
  unite("Domestic variables", V1:V7, sep = ", ", remove = T, na.rm = T) %>%
  rownames_to_column("Unit name")

dput(modspec.model6t.br.ssvs)

table(unlist(modspec.model6t.br.ssvs))

modspec.model6t.br.ssvs %>%
  na.omit %>%
  summarise_all(n_distinct)

# MODEL 7

endo <- lapply(model7t.br.ssvs$cc.results$coeffs,colnames)
exo  <- lapply(model7t.br.ssvs$cc.results$coeffs,rownames)
#EXCLUDED EXO (TO GET THEM, RECOPY THE FUNCTION - PRINT.BGVAR)
cN   <- names(endo)

vars <- list()
for(i in 1:length(endo)){
  vars[[i]] <- gsub(paste(cN[i],".",sep=""),"",endo[[i]])
}

varNames <- lapply(vars,function(l) paste(l,collapse=", "))
names(varNames) <- cN
varNames <- unlist(varNames) %>%
  data.frame()

library(splitstackshape)
varNamessplit <- cSplit(varNames, ".", ",") %>%
  data.frame()

rownames(varNamessplit) <- rownames(varNames)

colnames(varNamessplit) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

varNamessplit[] <- lapply(varNamessplit, function(x) {replace_na(x, "")})
varNamessplit[] <- lapply(varNamessplit, function(x) {paste0("$", x, "$")})
varNamessplit[] <- lapply(varNamessplit, function(x) {gsub("\\$\\$", "", x)})

modspec.model7t.br.ssvs <- varNamessplit %>%
  mutate_all(list(~na_if(.,""))) %>%
  unite("Domestic variables", V1:V8, sep = ", ", remove = T, na.rm = T) %>%
  rownames_to_column("Unit name")

dput(modspec.model7t.br.ssvs)

table(unlist(modspec.model7t.br.ssvs))

modspec.model7t.br.ssvs %>%
  na.omit %>%
  summarise_all(n_distinct)

# MODEL 8

endo <- lapply(model8t.br.ssvs$cc.results$coeffs,colnames)
exo  <- lapply(model8t.br.ssvs$cc.results$coeffs,rownames)
#EXCLUDED EXO (TO GET THEM, RECOPY THE FUNCTION - PRINT.BGVAR)
cN   <- names(endo)

vars <- list()
for(i in 1:length(endo)){
  vars[[i]] <- gsub(paste(cN[i],".",sep=""),"",endo[[i]])
}

varNames <- lapply(vars,function(l) paste(l,collapse=", "))
names(varNames) <- cN
varNames <- unlist(varNames) %>%
  data.frame()

library(splitstackshape)
varNamessplit <- cSplit(varNames, ".", ",") %>%
  data.frame()

rownames(varNamessplit) <- rownames(varNames)

colnames(varNamessplit) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

varNamessplit[] <- lapply(varNamessplit, function(x) {replace_na(x, "")})
varNamessplit[] <- lapply(varNamessplit, function(x) {paste0("$", x, "$")})
varNamessplit[] <- lapply(varNamessplit, function(x) {gsub("\\$\\$", "", x)})

modspec.model8t.br.ssvs <- varNamessplit %>%
  mutate_all(list(~na_if(.,""))) %>%
  unite("Domestic variables", V1:V8, sep = ", ", remove = T, na.rm = T) %>%
  rownames_to_column("Unit name")

dput(modspec.model8t.br.ssvs)

table(unlist(modspec.model8t.br.ssvs))

modspec.model8t.br.ssvs %>%
  na.omit %>%
  summarise_all(n_distinct)
