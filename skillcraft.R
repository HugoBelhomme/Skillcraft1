setwd("/home/boss/MAIN/2A/MRR/Projet")
tab = read.table("SkillCraft1_Dataset.csv", header = TRUE, sep = ",", dec = ".", na.strings = "?")

# Être capable de deviner le rang d'un joueur à partir de statistiques d'une de ses parties

head(tab)
dim(tab)

mean(tab$UniqueHotkeys)
summary(tab)

tabCleaned = tab
# On retire les lignes comportant des valeurs manquantes et on change le type de 3 colonnes
{
  tabCleaned = tabCleaned[-which(is.na(tabCleaned$Age)),]
  tabCleaned = tabCleaned[-which(is.na(tabCleaned$HoursPerWeek)),]
  tabCleaned = tabCleaned[-which(is.na(tabCleaned$TotalHours)),]
  
  tabCleaned$Age = as.numeric(as.character(tabCleaned$Age))
  tabCleaned$HoursPerWeek = as.numeric(as.character(tabCleaned$HoursPerWeek))
  tabCleaned$TotalHours = as.numeric(as.character(tabCleaned$TotalHours))
}

tabCleaned = tabCleaned[-c(1793,770,1977,2215,2323),] # On retire les menteurs sur le nombre d'heures de jeu

for(i in (3:20)) {
  tabCleaned[,i] = (tabCleaned[,i] - mean(tabCleaned[,i])) / sd(tabCleaned[,i])
}

head(sort(tabCleaned$TotalHours, decreasing = T)) # la moyenne est de 16

library('corrplot')
par(mfrow = c(1,1))
corrplot(cor(tabCleaned), method = "number")
corrplot.mixed(cor(tabCleaned), lower = "ellipse", upper = "number")
tab8 = tab
which(tab[,] == "?")
tab[which(tab$LeagueIndex == 8)]


library('ggplot2')

{
  indices = sample(3333, 3333/10)
  reg = lm(formula = LeagueIndex ~ ., data = tabCleaned[-indices,-1])
  res = predict.glm(reg, tabCleaned[indices,], "link") - tabCleaned[indices,2]
}

qplot((1:length(res)),as.vector(res), xlab = "Index", ylab = "Résidus", 
      main = "Affichage des résidus sur le jeu d'entrainement", geom = c("point", "smooth"))
## VS
plot((1:length(res)),as.vector(res));abline(a=0, b=0, col = "red")


summary(tabCleaned$TotalHours)
head(sort(tabCleaned$TotalHours, decreasing = T), n = 300)


# regression simple, repérer les grosses erreurs
# plot (hour, hour)
qplot(tabCleaned$TotalHours, tabCleaned$HoursPerWeek)
