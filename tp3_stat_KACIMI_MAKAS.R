#install.packages("corrplot")
library(corrplot)


# Charger la base de données
fitness = read.csv("fitness.csv")

# Structure de la base de données
str(fitness)

# Résumé des variables
summary(fitness)

# Ensemble des interactions des variables
plot(fitness)

# Influence de chaques variable
plot(oxy ~ age, data = fitness)
plot(oxy ~ weight, data = fitness)
plot(oxy ~ runtime, data = fitness)
plot(oxy ~ rstpulse, data = fitness)
plot(oxy ~ runpulse, data = fitness)
plot(oxy ~ maxpulse, data = fitness)

# Calcul la matrice de corrélation
correlation_matrix = cor(fitness)

# Representation graphique de la matrice de corrélation
corrplot(correlation_matrix)

# on calcul la matrice de covariance pour voir la correlation entre les vecteurs colonne (CL) car un modèle linéaire est bien quand il y a peu de corrélations




### Régression linéaire multiple


## 1)


# Variable "oxy"
Y = fitness$oxy

is.vector(Y)
# Y est bien un vecteur

# Supprime la variable "oxy" de la base de données
donne_sans_oxy = subset(fitness, select = -oxy)

# Créer un vecteur colonne avec uniquement des 1
intercept = rep(1, nrow(donne_sans_oxy))

# Ajouter le vecteur de 1 à la matrice de données
Xinter = cbind(intercept, donne_sans_oxy)

# Affiche les premières lignes de la nouvelle matrice de données
head(Xinter)

is.matrix(Xinter)
# Ce n'est pas une matrice, on va la transformer

# Transforme en matrice
X = as.matrix(Xinter)

# Calcul de l'estimateur beta des MCO
beta = solve( t(X)%*%X ) %*% (t(X)%*%Y)

print(beta)

# La valeur la plus extreme de beta est celui de runtime, c'est cohérent avec le graphique
# de la matrice de corrélation car le point rouge est le plus extreme en runtime


## 2)

# Valeur ajustée
Y_chap = X%*%beta

# Résidus de la régression 
residus = Y - Y_chap



## 3)

length(residus)

# Rang de X
p = qr(X)$rank

n = length(residus)

# Variance résiduelle
sigma2_chap = sum(residus^2) / (n - p)

# Matrice de covariance de l'estimateur du vecteur beta
cov_beta_chap = sigma2_chap * solve(t(X) %*% X)

# Affichage de l'estimation de sigma^2
print(sigma2_chap)

# Affichage de la matrice de covariance de l'estimateur du vecteur beta
print(cov_beta_chap)

# Estimations de la variance des estimateurs de chaque βj (éléments diagonaux de la matrice de covariance de beta)
variance_estimateurs = diag(cov_beta_chap)

print(variance_estimateurs)


## 4)
print(length(beta))

# Niveau de confiance 
alpha = 0.05

# Initialiser un vecteur pour stocker les intervalles de confiance
intervalles_confiance = matrix(NA, nrow = p, ncol = 2)

# Boucle pour calculer les intervalles de confiance
for (j in 1:length(beta)) {
  # Calculer l'intervalle de confiance pour le coefficient j
  borne_inf = beta[j] - qt(1 - alpha/2, n-p) * sqrt( cov_beta_chap[j, j] )
  borne_sup = beta[j] + qt(1 - alpha/2, n-p) * sqrt( cov_beta_chap[j, j] )
  
  # Stocker les bornes de l'intervalle de confiance dans le vecteur
  intervalles_confiance[j,] = c(borne_inf, borne_sup)
}

# Affichage des intervalles de confiance
print(intervalles_confiance)



## 5)
p = 6

stat = (n-p-1)*norm(  Y_chap - rep(mean(Y),n) ,type='2')^2 / ( p*norm(Y-Y_chap,type='2')^2 )

# 22.43

# Calcul des quantiles pour la loi de Fisher
qF_1 = qf(1 - alpha/2, df1 = p, df2 = n - p - 1)
qF_2 = qf(alpha/2, df1 = p, df2 = n - p - 1)

# Affichage des quantiles
cat("Quantile d'ordre 1-alpha/2 :", qF_1)
cat("Quantile d'ordre alpha/2 :", qF_2)

print(stat)

# Test à droite
p_value =  1 - pf(stat, df1 = p, df2 = n - p - 1)
print(p_value)

# p_value très petite en dessous de alpha (ordre 10e-09) on rejette H0

## 6)

stat2 = beta/sqrt(diag(cov_beta_chap))
print(stat2)

# Créer un vecteur de taille stat2 contenant la valeur 0 répétée
p_value2 = rep(0, length(stat2))



for (j in 1:length(stat2)) {
  p_value2[j] =  1 - pt(stat2[j], df = n-p)
  if (p_value2[j] < alpha) {
    cat("p valeur pour j =", j, ':', p_value2[j], 'on rejette H0 \n')
  } else {
    cat("p valeur pour j =", j, ':', p_value2[j], 'on ne peut rien dire \n')
  }
}


## 7)

# Ajustement du modèle linéaire
reg = lm(Y ~ ., data = fitness)


plot(reg, which = 1) #Graphique des résidus par rapport aux valeurs ajustées. Pour vérifier l’hypothèse d’homoscédasticité (variance constante des erreurs).
plot(reg, which = 3) # graphique de l’effet des leviers. Pour identifier les observations qui ont une grande influence sur l’estimation des coefficients de régression.

# Si les résidus se dispersent uniformément sans former de motifs ou de tendances, cela suggère que l’homoscédasticité est 
# respectée, ce qui est une hypothèse importante pour la validité du modèle de régression linéaire.


## 8)

# Installation du package lmtest

#if (!requireNamespace("lmtest", quietly = TRUE)) {
#  install.packages("lmtest")
#}

library(lmtest)

# Appliquer le test de Breusch-Pagan
breusch_pagan_test = bptest(reg)

# Afficher les résultats du test
print(breusch_pagan_test)

# p-valeur (0.1009) supérieure à alpha = 0.05, on ne rejette pas l'hypothèse nulle d'homoscédasticité


## 9)

# Outil graphique : QQ-plot
qqnorm(resid(reg)) # Trace le QQ-plot des résidus
qqline(resid(reg)) # Trace la ligne de référence

# Test statistique : Test de normalité (Shapiro-Wilk)
shapiro.test(resid(reg)) # Applique le test de Shapiro-Wilk aux résidus

# p-valeur (0.0001055) inférieur à alpha = 0.05, on rejette l'hypothèse nulle selon laquelle les résidus suivent une normale



### 2 Selection de variable

## 2.1

## 1)

# On pourrait retirer la variable maxpulse qui est fortement corrélé d'après le tableau de
# corrélation avec la variable runpulse.


## 2)

# Ajuster les deux modèles
modele_complet = lm(oxy ~ ., data = fitness)
modele_reduit = lm(oxy ~ . - maxpulse, data = fitness)

# Effectuer le test de rapport de vraisemblance
test_rapport_vraisemblance = anova(modele_reduit, modele_complet, test = "Chisq")

# Afficher les résultats
print(test_rapport_vraisemblance)

# p-value est inférieure au seuil alpha 0.05, cela suggère qu'il y a une différence significative entre les deux modèles.


## 2.2

## 1)

# Ajustez le modèle initial
reg = lm(oxy ~ ., data = fitness)

# Retirez la variable "maxpulse" du modèle
reg_sans_maxpulse = update(reg, . ~ . - maxpulse)

# Affichez le résumé du nouveau modèle
summary(reg_sans_maxpulse)

#L'âge (age) a un effet significatif sur le niveau d'oxygène consommé (oxy) avec un p-value de 0.0108.
#Le temps d'exercice (runtime) a également un effet significatif avec un p-value très faible de 7.13e-07.
#Le pouls au repos (rstpulse) et le pouls après l'exercice (runpulse) ont des p-values de 0.7059 et 0.0234 respectivement. Cela suggère que leur effet sur le niveau d'oxygène consommé pourrait être moins significatif.
#Le poids (weight) n'est pas significatif avec un p-value de 0.3777.


## 2)

reg_initial <- lm(oxy ~ ., data = fitness)
AIC_initial <- AIC(reg_initial)

variables_explicatives <- names(fitness)[-1]  # Exclure la variable dépendante 'oxy'
AIC_values <- numeric(length(variables_explicatives))

for (i in seq_along(variables_explicatives)) {
  formule <- formula(paste("oxy ~ . -", variables_explicatives[i]))
  reg_temp <- lm(formule, data = fitness)
  AIC_values[i] <- AIC(reg_temp)
}

index_min_AIC <- which.min(AIC_values)
variable_a_retirer <- variables_explicatives[index_min_AIC]

while (AIC_values[index_min_AIC] < AIC_initial) {
  formule <- formula(paste("oxy ~ . -", variable_a_retirer))
  reg_initial <- lm(formule, data = fitness)
  
  # Réactualiser l'AIC initial
  AIC_initial <- AIC(reg_initial)
  
  # Mettre à jour la liste des variables explicatives
  variables_explicatives <- names(fitness)[-1]
  
  # Recalculer les valeurs AIC
  AIC_values <- numeric(length(variables_explicatives))
  
  for (i in seq_along(variables_explicatives)) {
    formule_temp <- formula(paste("oxy ~ . -", variables_explicatives[i]))
    reg_temp <- lm(formule_temp, data = fitness)
    AIC_values[i] <- AIC(reg_temp)
  }
  
  # Identifier la prochaine variable à retirer
  index_min_AIC <- which.min(AIC_values)
  variable_a_retirer <- variables_explicatives[index_min_AIC]
}


## 3)

# Ajuster le modèle initial avec toutes les variables explicatives
reg_initial <- lm(oxy ~ ., data = fitness)

# Réduire le modèle par critère AIC en utilisant la méthode "backward"
reg_reduit <- step(reg_initial, direction = "backward", trace = FALSE)

# Afficher le résumé du modèle réduit
summary(reg_reduit)

# Analyser les résidus du modèle réduit
plot(reg_reduit, which = 1)  # Graphique des résidus standardisés par rapport aux valeurs ajustées
plot(reg_reduit, which = 3)  # Graphique des résidus par rapport aux valeurs ajustées

# Le modèle réduit semble bien ajusté aux données, avec plusieurs variables significatives pour expliquer 
# la consommation d'oxygène, notamment l'âge, la durée de course, le pouls de course et le pouls maximal.
