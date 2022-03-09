# MODEL 5 THESIS BR

library(BGVAR)
options(scipen=999)

# MODEL VIX SEPARATELY

GF.weights<-list()
GF.weights$weights<-rep(1/11, 11)
names(GF.weights$weights)<-c("AU","BR","CA","CH","GB","JP","MX","NZ","RU","U2","US")
GF.weights$variables<-c(colnames(MODEL5T_BR$GF),"NP", "EQ")
GF.weights$exo<-"VIX"
OE.weights.model5t.br <- list(GF=GF.weights)

RNGkind("L'Ecuyer-CMRG")
set.seed(123)
model5t.br.ssvs <- bgvar(
  Data = MODEL5T_BR,
  W = BRVIXTWM,
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
    OE.weights = OE.weights.model5t.br,
    cores = 7
  ),
  eigen = 1.05
)

print(model5t.br.ssvs)
sum.model5t.br.ssvs <- summary(model5t.br.ssvs, cores = 7)
sum.model5t.br.ssvs
