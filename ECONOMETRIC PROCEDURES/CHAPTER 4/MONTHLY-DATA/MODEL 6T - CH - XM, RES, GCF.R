# MODEL 6 THESIS CH

library(BGVAR)
options(scipen=999)

# MODEL GCF SEPARATELY

GF.weights<-list()
GF.weights$weights<-rep(1/9, 9)
names(GF.weights$weights)<-c("AU","CA","CH","GB","JP","MX","NZ","U2","US")
GF.weights$variables<-c(colnames(MODEL6T_CH$GF),"NP", "EQ")
GF.weights$exo<-"GCF"
OE.weights.model6t.ch <- list(GF=GF.weights)

RNGkind("L'Ecuyer-CMRG")
set.seed(123)
model6t.ch.ssvs <- bgvar(
  Data = MODEL6T_CH,
  W = CHONLYGCFTWM2,
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
    OE.weights = OE.weights.model6t.ch,
    cores = 7
  ),
  eigen = 1.05
)

print(model6t.ch.ssvs)
sum.model6t.ch.ssvs <- summary(model6t.ch.ssvs, cores = 7)
sum.model6t.ch.ssvs
