# TABLE WITH VARIABLES USED

# MODEL REPORT

library(BGVAR)
library(tidyverse)

# SWITZERLAND

# MODEL 1

endo <- lapply(model1t.ch.ssvs$cc.results$coeffs,colnames)
exo  <- lapply(model1t.ch.ssvs$cc.results$coeffs,rownames)
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

colnames(varNamessplit) <- c("V1", "V2", "V3", "V4", "V5", "V6")

varNamessplit[] <- lapply(varNamessplit, function(x) {replace_na(x, "")})
varNamessplit[] <- lapply(varNamessplit, function(x) {paste0("$", x, "$")})
varNamessplit[] <- lapply(varNamessplit, function(x) {gsub("\\$\\$", "", x)})

modspec.model1t.ch.ssvs <- varNamessplit %>%
  mutate_all(list(~na_if(.,""))) %>%
  unite("Domestic variables", V1:V6, sep = ", ", remove = T, na.rm = T) %>%
  rownames_to_column("Unit name")

dput(modspec.model1t.ch.ssvs)

table(unlist(modspec.model1t.ch.ssvs))

modspec.model1t.ch.ssvs %>%
  na.omit %>%
  summarise_all(n_distinct)

# MODEL 2

endo <- lapply(model2t.ch.ssvs$cc.results$coeffs,colnames)
exo  <- lapply(model2t.ch.ssvs$cc.results$coeffs,rownames)
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

colnames(varNamessplit) <- c("V1", "V2", "V3", "V4", "V5", "V6")

varNamessplit[] <- lapply(varNamessplit, function(x) {replace_na(x, "")})
varNamessplit[] <- lapply(varNamessplit, function(x) {paste0("$", x, "$")})
varNamessplit[] <- lapply(varNamessplit, function(x) {gsub("\\$\\$", "", x)})

modspec.model2t.ch.ssvs <- varNamessplit %>%
  mutate_all(list(~na_if(.,""))) %>%
  unite("Domestic variables", V1:V6, sep = ", ", remove = T, na.rm = T) %>%
  rownames_to_column("Unit name")

dput(modspec.model2t.ch.ssvs)

table(unlist(modspec.model2t.ch.ssvs))

modspec.model2t.ch.ssvs %>%
  na.omit %>%
  summarise_all(n_distinct)

# MODEL 3

endo <- lapply(model3t.ch.ssvs$cc.results$coeffs,colnames)
exo  <- lapply(model3t.ch.ssvs$cc.results$coeffs,rownames)
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

colnames(varNamessplit) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9")

varNamessplit[] <- lapply(varNamessplit, function(x) {replace_na(x, "")})
varNamessplit[] <- lapply(varNamessplit, function(x) {paste0("$", x, "$")})
varNamessplit[] <- lapply(varNamessplit, function(x) {gsub("\\$\\$", "", x)})

modspec.model3t.ch.ssvs <- varNamessplit %>%
  mutate_all(list(~na_if(.,""))) %>%
  unite("Domestic variables", V1:V9, sep = ", ", remove = T, na.rm = T) %>%
  rownames_to_column("Unit name")

dput(modspec.model3t.ch.ssvs)

table(unlist(modspec.model3t.ch.ssvs))

modspec.model3t.ch.ssvs %>%
  na.omit %>%
  summarise_all(n_distinct)

# MODEL 4

endo <- lapply(model4t.ch.ssvs$cc.results$coeffs,colnames)
exo  <- lapply(model4t.ch.ssvs$cc.results$coeffs,rownames)
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

colnames(varNamessplit) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9")

varNamessplit[] <- lapply(varNamessplit, function(x) {replace_na(x, "")})
varNamessplit[] <- lapply(varNamessplit, function(x) {paste0("$", x, "$")})
varNamessplit[] <- lapply(varNamessplit, function(x) {gsub("\\$\\$", "", x)})

modspec.model4t.ch.ssvs <- varNamessplit %>%
  mutate_all(list(~na_if(.,""))) %>%
  unite("Domestic variables", V1:V9, sep = ", ", remove = T, na.rm = T) %>%
  rownames_to_column("Unit name")

dput(modspec.model4t.ch.ssvs)

table(unlist(modspec.model4t.ch.ssvs))

modspec.model4t.ch.ssvs %>%
  na.omit %>%
  summarise_all(n_distinct)

# BRAZIL

# MODEL 1

endo <- lapply(model1t.br.ssvs$cc.results$coeffs,colnames)
exo  <- lapply(model1t.br.ssvs$cc.results$coeffs,rownames)
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

colnames(varNamessplit) <- c("V1", "V2", "V3", "V4", "V5", "V6")

varNamessplit[] <- lapply(varNamessplit, function(x) {replace_na(x, "")})
varNamessplit[] <- lapply(varNamessplit, function(x) {paste0("$", x, "$")})
varNamessplit[] <- lapply(varNamessplit, function(x) {gsub("\\$\\$", "", x)})

modspec.model1t.br.ssvs <- varNamessplit %>%
  mutate_all(list(~na_if(.,""))) %>%
  unite("Domestic variables", V1:V6, sep = ", ", remove = T, na.rm = T) %>%
  rownames_to_column("Unit name")

dput(modspec.model1t.br.ssvs)

table(unlist(modspec.model1t.br.ssvs))

modspec.model1t.br.ssvs %>%
  na.omit %>%
  summarise_all(n_distinct)

# MODEL 3

endo <- lapply(model3t.br.ssvs$cc.results$coeffs,colnames)
exo  <- lapply(model3t.br.ssvs$cc.results$coeffs,rownames)
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

colnames(varNamessplit) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9")

varNamessplit[] <- lapply(varNamessplit, function(x) {replace_na(x, "")})
varNamessplit[] <- lapply(varNamessplit, function(x) {paste0("$", x, "$")})
varNamessplit[] <- lapply(varNamessplit, function(x) {gsub("\\$\\$", "", x)})

modspec.model3t.br.ssvs <- varNamessplit %>%
  mutate_all(list(~na_if(.,""))) %>%
  unite("Domestic variables", V1:V9, sep = ", ", remove = T, na.rm = T) %>%
  rownames_to_column("Unit name")

dput(modspec.model3t.br.ssvs)

table(unlist(modspec.model3t.br.ssvs))

modspec.model3t.br.ssvs %>%
  na.omit %>%
  summarise_all(n_distinct)