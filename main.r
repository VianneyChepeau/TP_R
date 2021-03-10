# Title     : TP_R
# Created by: CHEPEAU Vianney, Rémy LAROCHE, Cassandra RATRIMOARISON
# Created on: 10/03/2021

# Récupération du fichier de données
data <- read.csv("ventes.csv")
#str(data)

# Partie 1
data_part1 <- data
head(data_part1)

# Suppression colonne "Canal"
data_part1 <- subset(data_part1, select = -c(1))
head(data_part1) 

#Ajouter a droite la colonne "Total" = « ProduitsFrais »+ « Lait »+ « Epicerie »+ « Surgele »+ « Detergents »+« Traiteur »
data_part1["Total"] <- data_part1$ProduitsFrais + data_part1$Lait + data_part1$Epicerie + data_part1$Surgele + data_part1$Detergents + data_part1$Traiteur
head(data_part1)
 
#Ajouter a droite la colonne : « % ProduitsFrais » = « ProduitsFrais » par rapport a « Total »
data_part1["% ProduitsFrais"] <- round((data_part1$ProduitsFrais/data_part1$Total)*100, 0)

#De même pour toutes les autres colonnes
data_part1["% Lait"] <- round((data_part1$Lait/data_part1$Total)*100, 0)
data_part1["% Epicerie"] <- round((data_part1$Epicerie/data_part1$Total)*100, 0)
data_part1["% Surgele"] <- round((data_part1$Surgele/data_part1$Total)*100, 0)
data_part1["% Detergents"] <- round((data_part1$Detergents/data_part1$Total)*100, 0)
data_part1["% Traiteur"] <- round((data_part1$Traiteur/data_part1$Total)*100, 0)
head(data_part1)

#Exporter le tableau
write.csv2(data_part1, file = "TPR.csv")
test <- read.csv2("TPR.csv")
head(test)

#Afficher pour chacunes des régions le CA Total moyen

#Partie 2 

#Déterminer la paire de variables qui a le meilleur coef de corrél linéaire (« ProduitsFrais », « Lait », « Epicerie », « Surgele », « Detergents » et « Traiteur »)

#Regression linéaire des deux paire de variable (Y par rapport à X et X par rapport à Y)

# Calculer une prévision, avec chacune de ces deux régressions,

#Afficher ces deux prévisions ainsi que le nuage de point et la droite de regression
