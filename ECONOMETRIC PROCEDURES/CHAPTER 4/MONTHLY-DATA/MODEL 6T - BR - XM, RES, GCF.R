# MODEL 6 THESIS BR

library(BGVAR)
options(scipen=999)

# MODEL GCF SEPARATELY

GF.weights<-list()
GF.weights$weights<-rep(1/11, 11)
names(GF.weights$weights)<-c("AU","BR","CA","CH","GB","JP","MX","NZ","RU","U2","US")
GF.weights$variables<-c(colnames(MODEL6T_BR$GF),"NP", "EQ")
GF.weights$exo<-"GCF"
OE.weights.model6t.br <- list(GF=GF.weights)

RNGkind("L'Ecuyer-CMRG")
set.seed(123)
model6t.br.ssvs <- bgvar(
  Data = MODEL6T_BR,
  W = BRGFCTWM,
  draws = 20000,
  burnin = 35000,
  plag = 1,
  prior = "SSVS",
  hyperpara = NULL,
  SV = TRUE,
  thin = 100,
  trend = TRUE,
  expert = list(
    save.shrink.store = TRUE,
    use_R = TRUE,
    Wex.restr = c("ER"),
    OE.weights = OE.weights.model6t.br,
    cores = 7
  ),
  eigen = 1.05
)

print(model6t.br.ssvs)
sum.model6t.br.ssvs <- summary(model6t.br.ssvs, cores = 7)
sum.model6t.br.ssvs
