library("dplyr")
data <- read.csv("resulttsBVT12.csv", sep = ";", stringsAsFactors = FALSE)
data <- data %>% mutate(Abstention.T1 = Inscrits.T1 - Votants.T1,
                        Abstention.T2 = Inscrits.T2 - Votants.T2,
                        BN.T1 = Blancs + Nuls,
                        BN.T2 = Blancs.T2 + Nuls.T2)
data <- data %>% mutate(deltaVotants = Votants.T2 - Votants.T1,
                        deltaExprimés = `Exprimés.T2` - `Exprimés`)
data <- data %>% mutate(Abstention.T1.ins = Abstention.T1 / Inscrits.T1 * 100,
                        BN.T1.ins = BN.T1 / Inscrits.T1 * 100,
                        Barbier.ins = Barbier / Inscrits.T1 * 100,
                        Montel.ins = Montel / Inscrits.T1 * 100,
                        Demouge.ins = Demouge / Inscrits.T1 * 100,
                        Autres.ins = (Bonnot + Vinci + O + Boudjekada + Adami + Treppo + Lachambre + Hervé + Rousseaux + Sanchez) / Inscrits.T1 * 100,
                        Autres = Bonnot + Vinci + O + Boudjekada + Adami + Treppo + Lachambre + Hervé + Rousseaux + Sanchez,
                        Abstention.T2.ins = Abstention.T2 / Inscrits.T2 * 100,
                        BN.T2.ins = BN.T2 / Inscrits.T2 * 100,
                        Montel.T2.ins = Montel.T2 / Inscrits.T2 * 100,
                        Barbier.T2.ins = Barbier.T2 / Inscrits.T2 * 100)
reg1 <- lm(Montel.T2.ins ~ 0 + Abstention.T1.ins + BN.T1.ins + Montel.ins + Barbier.ins + Demouge.ins + Autres.ins, data = data, weights = data$Inscrits)
reg2 <- lm(Barbier.T2.ins ~ 0 + Abstention.T1.ins + BN.T1.ins + Montel.ins + Barbier.ins + Demouge.ins + Autres.ins, data = data, weights = data$Inscrits)
reg3 <- lm(BN.T2.ins ~ 0 + Abstention.T1.ins + BN.T1.ins + Montel.ins + Barbier.ins + Demouge.ins + Autres.ins, data = data, weights = data$Inscrits)
reg4 <- lm(Abstention.T2.ins ~ 0 + Abstention.T1.ins + BN.T1.ins + Montel.ins + Barbier.ins + Demouge.ins + Autres.ins, data = data, weights = data$Inscrits)

library(texreg)
screenreg(list(reg1, reg2, reg3, reg4))

library("ggplot2")
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

library(ei)
formula <- cbind(Montel.T2, Barbier.T2, BN.T2, Abstention.T2) ~ cbind(Abstention.T1, BN.T1, Demouge, Montel, Barbier, Autres)
dbuf <- ei(formula, data=data, sample=50000, burnin=5000, total = "Inscrits.T1")
# on crée une matrice pour stocker les estimations
#tour1 <- c("Abstention", "Blancs et nuls", "Demouge (UMP)", "Montel (FN)", "Barbier (PS)", "Autres")
tour1 <- c("Abstention.T1", "BN.T1", "Demouge", "Montel", "Barbier", "Autres")
#tour2 <- c("Montel (FN)", "Barbier (PS)", "Blancs et nuls", "Abstention")
tour2 <- c("Montel.T2", "Barbier.T2", "BN.T2", "Abstention.T2")
df <- array(dim=c(length(tour1), length(tour2)))
dimnames(df)[[1]] <- tour1
dimnames(df)[[2]] <- tour2

# calcul des estimations moyennes 
for (i in tour1) {
  for (j in tour2) {
    df[i,j] <- weighted.mean(colMeans(dbuf$draws$Beta[,paste("beta.", i, ".", j,".", 1:96, sep="")]), data$Inscrits.T1)
  }
}

# calcul des écarts-types
df2 <- array(dim=c(length(tour1), length(tour2)))
dimnames(df2)[[1]] <- tour1
dimnames(df2)[[2]] <- tour2

for (i in tour1) {
  for (j in tour2) {
    df2[i,j] <- sqrt(Hmisc::wtd.var(summary(dbuf$draws$Beta[,paste("beta.",i,".",j,".",1:96, sep="")])$statistics[,1], data$Inscrits.T1))
  }
}

