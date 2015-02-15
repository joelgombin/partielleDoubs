library(Hmisc)
library(dplyr)
library(ggplot2)
library(eiPack)
# evite pb avec summarize de Hmisc
unloadNamespace('Hmisc')

read.csv("~/ipn/data/resulttsBVT12.csv", sep = ";", stringsAsFactors = FALSE) %>%
  mutate(
    Abstention.T1 = Inscrits.T1 - Votants.T1,
    Abstention.T2 = Inscrits.T2 - Votants.T2,
    BN.T1 = Blancs + Nuls,
    BN.T2 = Blancs.T2 + Nuls.T2,
    Votants.T2 - Votants.T1,
    deltaExprimés = Exprimés.T2 - Exprimés,
    Abstention.T1.ins = Abstention.T1 / Inscrits.T1 * 100,
    BN.T1.ins = BN.T1 / Inscrits.T1 * 100,
    Barbier.ins = Barbier / Inscrits.T1 * 100,
    Montel.ins = Montel / Inscrits.T1 * 100,
    Demouge.ins = Demouge / Inscrits.T1 * 100,
    Autres.ins = (Bonnot + Vinci + O + Boudjekada + Adami + Treppo + Lachambre + Hervé + Rousseaux + Sanchez) / Inscrits.T1 * 100,
    Autres = Bonnot + Vinci + O + Boudjekada + Adami + Treppo + Lachambre + Hervé + Rousseaux + Sanchez,
    Abstention.T2.ins = Abstention.T2 / Inscrits.T2 * 100,
    BN.T2.ins = BN.T2 / Inscrits.T2 * 100,
    Montel.T2.ins = Montel.T2 / Inscrits.T2 * 100,
    Barbier.T2.ins = Barbier.T2 / Inscrits.T2 * 100
  ) -> data

reg1 <- lm(Montel.T2.ins ~ 0 + Abstention.T1.ins + BN.T1.ins + Montel.ins + Barbier.ins + Demouge.ins + Autres.ins, data = data, weights = data$Inscrits)
reg2 <- lm(Barbier.T2.ins ~ 0 + Abstention.T1.ins + BN.T1.ins + Montel.ins + Barbier.ins + Demouge.ins + Autres.ins, data = data, weights = data$Inscrits)
reg3 <- lm(BN.T2.ins ~ 0 + Abstention.T1.ins + BN.T1.ins + Montel.ins + Barbier.ins + Demouge.ins + Autres.ins, data = data, weights = data$Inscrits)
reg4 <- lm(Abstention.T2.ins ~ 0 + Abstention.T1.ins + BN.T1.ins + Montel.ins + Barbier.ins + Demouge.ins + Autres.ins, data = data, weights = data$Inscrits)

data %>% 
  ggplot(aes(x = Demouge.ins, y = Montel.T2.ins - Montel.ins)) +
  geom_point()

data %>% 
  ggplot(aes(x = Abstention.T2.ins - Abstention.T1.ins, y = Montel.T2.ins - Montel.ins)) +
  geom_point()

data %>% 
  ggplot(aes(x = Abstention.T2.ins - Abstention.T1.ins, y = Barbier.T2.ins - (Barbier.ins + Autres.ins))) +
  geom_point()

data %>% 
  ggplot(aes(x = Demouge.ins, y = Abstention.T2.ins - Abstention.T1.ins)) +
  geom_point()

formula <- cbind(Montel.T2, Barbier.T2, BN.T2, Abstention.T2) ~ cbind(Abstention.T1, BN.T1, Demouge, Montel, Barbier, Autres)
dbuf <- ei.MD.bayes(formula, data=data, sample=100000, burnin=10000, total = "Inscrits.T1")

tour1 <- c("Abstention.T1", "BN.T1", "Demouge", "Montel", "Barbier", "Autres")
tour2 <- c("Montel.T2", "Barbier.T2", "BN.T2", "Abstention.T2")
bureau <- 1:96

df1 <- expand.grid(tour1=tour1, tour2=tour2, bureau=bureau)
df1$taux <- colMeans(dbuf$draws$Beta[,paste("beta.", df1$tour1, ".", df1$tour2, ".", df1$bureau, sep="")])
df1$tot <- data$Inscrits.T1[df1$bureau]

df1 %>%
  group_by(tour1, tour2) %>%
  summarize(
    moy = Hmisc::wtd.mean(taux, weights=tot),
    var = Hmisc::wtd.var(taux, weights=tot)) -> df2
