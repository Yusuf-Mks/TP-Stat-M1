# KACIMI Walid / MAKAS Yusuf

tips = read.csv('tips.csv')

tips$SEX = as.factor(tips$SEX)
tips$TIME = as.factor(tips$TIME)
tips$DAY = as.factor(tips$DAY)
tips$SMOKER = as.factor(tips$SMOKER)

str(tips)

##

# Il n'est pas pertinent d'évaluer la générosité des clients
# sans introduire le montant total des clients


tipsratio = tips$TIP/tips$TOTBILL
tipsJour = tipsratio[tips$TIME == 0 ]
tipsSoir = tipsratio[tips$TIME == 1 ]

hist(tipsJour, prob=1)
hist(tipsSoir, prob=1)




### 1 Test de Fischer ###


## 1)


# Calculer la variance empirique corrigée pour tipsJour
S_tipsJour = var(tipsJour, na.rm = TRUE)

# Calculer la variance empirique corrigée pour tipsSoir
S_tipsSoir = var(tipsSoir, na.rm = TRUE)

# Affichage de la statistique de test de Fischer
cat("Statistique de test Fischer :", S_tipsJour/S_tipsSoir)



## Intervalle de confiance

alpha = 0.05

# Nombre d'éléments dans tipsJour
n = length(tipsJour)

# Nombre d'éléments dans tipsSoir
m = length(tipsSoir)

# Tracage de la densité de probabilité de la distribution Fisher
curve(df(x, n-1, m-1), from = 0, to = 2, xlab = "Valeur", ylab = "Densité de probabilité", main = "Distribution Fisher")

# D'après la courbe tracé, on doit avoir pour alpha = 5% des quantiles autour de 0.5 et 1.5

# Calcul des quantiles pour la loi de Fisher
qF_1 = qf(1 - alpha/2, df1 = n - 1, df2 = m - 1)
qF_2 = qf(alpha/2, df1 = n - 1, df2 = m - 1)

# Affichage des quantiles
cat("Quantile d'ordre 1-alpha/2 :", qF_1)
cat("Quantile d'ordre alpha/2 :", qF_2)

# On obtient bien ce que nous attendions ( càd autour de 0.5 et 1.5 )

# Calcul des bornes de l'intervalle de confiance
borne_infF = S_tipsJour / (S_tipsSoir * qF_1)
borne_supF = S_tipsJour / (S_tipsSoir * qF_2)

# Affichage de l'intervalle de confiance
cat("Intervalle de conf Fischer alpha =",alpha,": [", borne_infF, ", ", borne_supF, "]")

# On trouve un intervalle de confiance qui ne contient pas 1, les variances ont de grandes chances
# de ne pas être égales. On s'attend donc plus tard à rejeter l'hypothèse H0

# Affichage de la zone de rejet en supposant H0 vraie
cat("Zone de rejet Fisher alpha =",alpha,": [0, ", qF_2, "[",'U ]',qF_1,', +inf[')


# Definissons une fonction pour la p_value avec un echantillon T en paramètre

p_valeur_F = function(T) {
  # On calcul la p-valeur avec la fonction de reparition pf (Fisher)
  
  p_value = 2 * min(pf(T, df1 = n - 1, df2 = m - 1), 1 - pf(T, df1 = n - 1, df2 = m - 1))
  
  return(p_value)
}


# On defini notre test pour nos données
T_valeurF = S_tipsJour/S_tipsSoir

print(T_valeurF)

# On s'attend à avoir pour T = 0.355 une p_value < alpha car T dans la zone de rejet

# Calcul et affichage p-valeurs
pvalF = p_valeur_F(T_valeurF)
cat("p-valeur pour T =", T_valeurF, ":", pvalF )

# p-valeur très faible en 10e-6, cela suggère une forte preuve contre l'hypothèse H0
# Donc il existe une différence dans la générosité des clients entre jour/soir.
# De plus, l'utilisation de ce test est pertinent pour comparer les variances de deux groupes indépendants


## 2)

# Utilisation de la fonction var.test pour comparer les variances
var_test = var.test(tipsJour, tipsSoir)

print(var_test)

# On retrouve bien nos resultats calculé précedement , avec par ex une p_value 3.852e-06
# On peut donc conclure qu'il y a une différénce significative entre les variances des
# pourboires entre la journée et le soir.






### 2 Test de Student ###


## 1)

# Calcul des variables :

# Calcul de la moyenne pour tipsJour
moy_tipsJour = mean(tipsJour)

# Calcul de la moyenne pour tipsSoir
moy_tipsSoir = mean(tipsSoir)

S_JourSoir = (n-1)/(n+m-2)*S_tipsJour + (m-1)/(n+m-2)*S_tipsSoir


# Tracé de la densité de probabilité de la distribution de Student
curve(dt(x, df = n+m-2), from = -3, to = 3, xlab = "Valeur", ylab = "Densité de probabilité", main = "Distribution de Student")

# Distribution symétrique, un calcul de quantile suffit (on prend celui positif)
# Graphiquement on s'attend à avoir des quantiles autour de +-2

# Calcul du quantile d'ordre 1-alpha/2
qS = qt(1-alpha/2, df = n+m-2)

# Affichage du résultat
cat("Quantile d'ordre 1-alpha/2 :", qS)


# Calcul des bornes de l'intervalle de confiance
borne_infS = moy_tipsJour-moy_tipsSoir - qS*sqrt(S_JourSoir*(1/n+1/m))
borne_supS = moy_tipsJour-moy_tipsSoir + qS*sqrt(S_JourSoir*(1/n+1/m))

# Affichage de l'intervalle de confiance
cat("Intervalle de conf Student alpha =",alpha,": [", borne_infS, ", ", borne_supS, "]")

# Affichage de la zone de rejet en supposant H0 vraie
cat("Zone de rejet Student alpha =",alpha,": [-inf, ", -qS, "[",'U ]',qS,', +inf[')


# Definissons une fonction pour la p_value avec un echantillon T en paramètre

p_valeur_S = function(T) {
  
  # On calcul la p-valeur avec la fonction de reparition pt (Student)
  
  p_value = 2 * min( pt(T, df = n+m-2), 1 - pt(T, df = n+m-2) )
  
  return(p_value)
}


# On defini notre test pour nos données
T_valeurS = (moy_tipsJour-moy_tipsSoir) / sqrt(S_JourSoir*(1/n+1/m)) 

print(T_valeurS)
# T proche de zero donc on s'attend à accepté car elle n'appartient pas à la zone de rejet

# Calcul et affichage p-valeurs
pvalS = p_valeur_S(T_valeurS)
cat("p-valeur pour T =", T_valeurS, ":", pvalS )

# p-valeur = 0.5278868 supérieur à notre niveau de signification alpha 0.05. Donc,
# nous ne pouvons pas rejeter l'hypothèse H0. Ainsi, d'un point de vue statistique,
# il n'y a pas de différence significative entre les moyennes des pourboires
# pour la journée et le soir.


## 2)

# Utilisation de la fonction t.test pour comparer les moyennes
t_test = t.test(tipsJour, tipsSoir)

print(t_test)

# On retrouve bien nos résultats calculés précédemment, tels que les intervalles de confiance 
# qui sont quasiment égaux.
# la p-value peut varié avec t.test car il utilise des algorithmes plus sophistiqués 

# On a bien une p-valeur > alpha, Donc sur la base de nos données, il n'y a pas de preuves 
# statistiques significatives pour affirmer qu'il y a une différence dans la générosité des 
# clients entre la journée et le soir.






### 3 Test de Kolmogorov ###


## 1)


# On veut tester la normalité des échantilons. Nous allons donc utiliser la fonction de 
# repartition d'une loi normal de moyenne et ecart type de nos echantillons avec la
# fonction pnorm de R.


# Création de la fonction de répartition empirique pour tipsJour et tipsSoir
ecdf_jour = ecdf(tipsJour)
ecdf_soir = ecdf(tipsSoir)


# Calcul de l'écart type pour les données 
# Calcul sans biais pour ajuster la distribution normale

ecart_type_tipsJour = sd(tipsJour)
ecart_type_tipsSoir = sd(tipsSoir)


# Création de la fonction de répartition normale avec pnorm pour comparer
f_jour = function(x) pnorm(x, mean = moy_tipsJour, sd = ecart_type_tipsJour)
f_soir = function(x) pnorm(x, mean = moy_tipsSoir, sd = ecart_type_tipsSoir)



# On trace en bleu la fonction empirique et en rouge la fonction de répartition normal 

# Pour tipsJour :

plot(ecdf_jour, main = "tipsJour", 
     xlab = "Valeur", ylab = "Probabilité", col = "blue") ; curve(f_jour, add = TRUE, col = "red", 
      lty = 2, lwd = 2, from = min(tipsJour), to = max(tipsJour)) ; legend("bottomright", 
                              legend = c("Empirique", "Normale"), col = c("blue", "red"), lty = 1:2, cex = 0.8)


# Pour tipsSoir :

plot(ecdf_soir, main = "tipsSoir", 
     xlab = "Valeur", ylab = "Probabilité", col = "blue") ; curve(f_soir, add = TRUE, col = "red", 
      lty = 2, lwd = 2, from = min(tipsSoir), to = max(tipsSoir)) ; legend("bottomright", 
                              legend = c("Empirique", "Normale"), col = c("blue", "red"), lty = 1:2, cex = 0.8)


# Les courbes se superposent relativement bien, montrant un bon ajustement des données à la distribution normale.
# Pour tipsJour, on remarque deux petites zones de divergence, aux alentours de 0.10 et 0.17.
# Pour tipsSoir, la distribution semble plus uniforme, bien qu'elle présente une légère contraction par rapport à la distribution normale.



## 2)

# Nous prenons toujours un risque de premiere espèce à 5%
alpha = 0.05

cat("Taille de l'échantillon tipsJour :", n)
cat("Taille de l'échantillon tipsSoir :", m)

# Dans les deux cas, nous sortons de la Table 1, nous allons alors approximer la valeur critique par :

critique_jour = sqrt( -1/(2*n)*log(alpha/2) )
critique_soir = sqrt( -1/(2*m)*log(alpha/2) )

cat("Valeur critique jour =",critique_jour)
cat("Valeur critique soir =",critique_soir)

# On va maintenant calculer le max de la difference entre empirique et normal (delta dans le TP)

# Pour tipsJour (j) et tipsSoir (s) :

# Initialisation du vecteur delta_moins et delta_plus
delta_moins_j = numeric(n+1)  # n+1 car i=0,...,n
delta_plus_j = numeric(n+1)

delta_moins_s = numeric(m+1)
delta_plus_s = numeric(m+1)

# Initialisation de la première valeur delta_moins ( car X(0) vaut -inf )
delta_moins_j[1] =  0

# Initialisation de la dernière valeur delta_plus ( car X(m+1) vaut +inf )
delta_moins_s[m+1] =  0

# Tri des données dans l'ordre croissant pour pouvoir les comparer avec la fonction de repartition
tipsJour_Croissant = sort(tipsJour)
tipsSoir_Croissant = sort(tipsSoir)


# Calcul des valeurs et stockage dans delta_moins pour jour
for (i in 1:n) {
  delta_moins_j[i+1] = abs(i/n - f_jour(tipsJour_Croissant[i]))
}


# Calcul des valeurs et stockage dans delta_plus pour jour
for (i in 1:(n-1)) {
  delta_plus_j[i] = abs(i/n - f_jour(tipsJour_Croissant[i+1]))
}

# Calcul des valeurs et stockage dans delta_moins pour soir
for (i in 2:m) {
  delta_moins_s[i+1] = abs(i/m - f_soir(tipsSoir_Croissant[i]))
}

# Calcul des valeurs et stockage dans delta_plus pour soir
for (i in 1:(m-1)) {
  delta_plus_s[i] = abs(i/m - f_soir(tipsSoir_Croissant[i+1]))
}

# Calcul du vecteur contenant le maximum de chaque composante
delta_jour = pmax(delta_moins_j, delta_plus_j)
delta_soir = pmax(delta_moins_s, delta_plus_s)

# Calcul final de T_K :
TK_jour = max(delta_jour)
TK_soir = max(delta_soir)

cat("TK_jour =",TK_jour,"comparé a la valeur critique :",critique_jour)
cat("TK_soir =",TK_soir,"comparé a la valeur critique :",critique_soir)

# On a donc TK_jour < Val_critique, (0.110975 < 0.164694) ainsi on ne rejette pas H0
# et TK_soir > val_critique (0.1306725 > 0.1023708) ainsi on rejete H0

# En d'autres termes, on rejete la normalité de la distribution de tipsJour, mais nous
# ne pouvons pas rejeter l'hypothèse nulle de normalité pour la distribution de tipsSoir.



## 3)


# Test de normalité pour TipsJour
ks_test_jour = ks.test(tipsJour, "pnorm", mean = mean(tipsJour), sd = sd(tipsJour))
print(ks_test_jour)

# D = 0.11098 : nous trouve pareil

# Test de normalité pour TipsSoir
ks_test_soir = ks.test(tipsSoir, "pnorm", mean = mean(tipsSoir), sd = sd(tipsSoir))
print(ks_test_soir)

# D = 0.13067 : nous trouve pareil

# Donc le resultat est le même en terme de rejet de H0 



## 4)


# Test de normalité pour TipsJour
shapiro_test_jour = shapiro.test(tipsJour)
print(shapiro_test_jour)

# Test de normalité pour TipsSoir
shapiro_test_soir = shapiro.test(tipsSoir)
print(shapiro_test_soir)

# Pour tipsJour, la valeur p est de 0.4852, ce qui est bien au-dessus du seuil de 0.05. Donc  
# nous ne rejetons pas l'hypothèse nulle selon laquelle l'échantillon suit une distribution normale.

# Pour TipsSoir, la valeur p est très faible (3.76e-15), bien en dessous du seuil de significativité de 0.05.
# Ainsi, nous rejetons l'hypothèse nulle et concluons que l'échantillon ne suit pas une distribution normale.

# Finalement nous retrouvons le même resultat de choix de rejet.








### 4. Distribution de la p-valeur ###


## 1)

# Dans les tests d'hypothèse, p valeur est la probabilité d'obtenir des résultats de test au moins aussi extrêmes que 
# le résultat réellement observé, en supposant que l'hypothèse nulle est correcte. Cette valeur p est calculée sur la 
# base des données observées, qui sont des échantillons aléatoires de la population. Puisque les données sont aléatoires, 
# la valeur p calculée, qui est fonction de ces données, est également aléatoire.


## 2)

# Supposons un test d'Hypotèse (par exemple unilatéral Gauche) : 
#
# La p-valeur vaut : pval=F.T(tobs) (P = F.T(T)),    avec : - tobs : une observation de notre stat de test T
#                                                           - F.T : la fonction de répartition de loi de la stat T sous H0
#  
# Calculons la fct de répartition de notre p-val:
# F.pval(x) = P(pval <= x) = P(F.T(T) <= p)
#                          = P(T<=F.T^-1(p))
#                          = F.T(F.T^-1(p))
#                          = p
#
# On a la même fonction de répartition qu'une loi uniforme sur [0,1] donc les 2 loi sont égales.


## 3)

# set.seed permet de reproduire les mêmes séquences de nombres aléatoires à chaque exécution du script
set.seed(123)


taille_echantillon = 30 # Taille de l'échantillon
moyenne = 0 # Moyenne de la distribution gaussienne
variance = 1 # Variance de la distribution gaussienne
nombre_echantillons = 1000 # Nombre d'échantillons à générer

# Initialisation du vecteur pour stocker les p-valeurs
p_values = numeric(nombre_echantillons)


# Génération des échantillons et calcul des p-valeurs
for (i in 1:nombre_echantillons) {
  echantillon = rnorm(taille_echantillon, mean = moyenne, sd = sqrt(variance))
  test = t.test(echantillon) # Test de student avec la fonciton t.test
  p_values[i] = test$p.value # Extrait la p-valeur de test
}

# Tracé de l'histogramme des p-valeurs
hist(p_values, breaks = 20, main = "Distribution des p-valeurs",
     xlab = "p-valeur", ylab = "Fréquence", col = "blue", freq = FALSE)

#  ligne représentant la densité de la distribution uniforme entre 0 et 1
curve(dunif(x, 0, 1), add = TRUE, col = "red")



## 4)

# On effectue une simulation générant des echantillons à partir d'une distrib exponentielle(1) :

# Définition des paramètres de la simulation
tailles_echantillon = c(10, 20, 30, 50, 100)  # Tailles d'échantillon à tester
nombre_simulations = 1000  # Nombre de simulations pour chaque taille d'échantillon
niveau_signification = 0.05  # Niveau de signification du test

# Fonction pour générer des échantillons à partir d'une distrib exponentielle
gener_exponentielle = function(taille) {
  return(rexp(taille, rate = 1))  # rexp : vecteur de taille taille de distribution exponentielle(1)
}

# Simulation et test de Student pour chaque taille d'échantillon
results = list()  # On stockera chaque vecteur de p-value pour les différentes tailles d'échantillon

for (taille in tailles_echantillon) {
  
  p_values = numeric(nombre_simulations)
  
  for (i in 1:nombre_simulations) {
    
    echantillon = gener_exponentielle(taille)  # Génération de l'échantillon exponentiel
    test_result = t.test(echantillon)  # Test de Student
    p_values[i] = test_result$p.value  # Stockage de la p-value
    
  }
  
  results[[as.character(taille)]] = p_values
}

# Tracé des histogrammes des valeurs p pour chaque taille d'échantillon
par(mfrow = c(3, 2))  # 3 lignes, 2 colonnes pour afficher les histogrammes
for (i in 1:length(tailles_echantillon)) {
  hist(results[[as.character(tailles_echantillon[i])]], breaks = 20,
       main = paste("Taille de l'échantillon:", tailles_echantillon[i]),
       xlab = "p-valeur", ylab = "Fréquence", col = "blue", freq = FALSE)
  curve(dunif(x, 0, 1), add = TRUE, col = "red")  # Courbe de densité uniforme
}



