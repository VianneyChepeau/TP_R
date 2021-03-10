# Titre  : TP_R
# Groupe : CHEPEAU Vianney, LAROCHE Remy , RATRIMOARISON Cassandra
# Le Cré    : 10/03/2021

# Récupération du fichier de données
data <- read.csv("ventes.csv")
#str(data)

# Partie 1
print("----------------- Partie 1 -----------------", quote = FALSE)
#########
data_part1 <- data
str(data_part1)

# Suppression colonne "Canal"
data_part1 <- subset(data_part1, select = -c(1))
str(data_part1)

#Ajouter a droite la colonne "Total" = « ProduitsFrais »+ « Lait »+ « Epicerie »+ « Surgele »+ « Detergents »+« Traiteur »
data_part1["Total"] <- data_part1$ProduitsFrais + data_part1$Lait + data_part1$Epicerie + data_part1$Surgele + data_part1$Detergents + data_part1$Traiteur
str(data_part1)
 
#Ajouter a droite la colonne : « % ProduitsFrais » = « ProduitsFrais » par rapport a « Total »
data_part1["% ProduitsFrais"] <- round((data_part1$ProduitsFrais/data_part1$Total)*100, 0)

#De même pour toutes les autres colonnes
data_part1["% Lait"] <- round((data_part1$Lait/data_part1$Total)*100, 0)
data_part1["% Epicerie"] <- round((data_part1$Epicerie/data_part1$Total)*100, 0)
data_part1["% Surgele"] <- round((data_part1$Surgele/data_part1$Total)*100, 0)
data_part1["% Detergents"] <- round((data_part1$Detergents/data_part1$Total)*100, 0)
data_part1["% Traiteur"] <- round((data_part1$Traiteur/data_part1$Total)*100, 0)
str(data_part1)

#Exporter le tableau
write.csv2(data_part1, file = "TPR.csv")
tprPart1 <- read.csv2("TPR.csv")
str(tprPart1)

#Afficher pour chacunes des régions le CA Total moyen

for (i in 1:3) {
    region <- data_part1[data$Region == i,]
    print(sprintf("CA Total moyen Region %d : %s euros" , i,round(mean(region$Total),2)), quote = FALSE)
}

#Partie 2
print("----------------- Partie 2 -----------------", quote = FALSE)
#########
data_part2 <- data
str(data_part2)
print("-----", quote = FALSE)


# Recherche du meilleur coefficient de corrélation linéaire
max <- 0 
for (i in 3:8) { 
    for (j in i:8) {        # ici, on fait une double boucle pour pouvoir faire une corréliation entre toutes les colonnes
        if(j!=i){           # on évite de faire la corrélation sur la même colonne
            currentCor = cor(data_part2[,i], data_part2[,j], use = "complete.obs") # on récupère la corrélation entre deux colonnes
            if(currentCor > max){ # si la corrélation récupéré est supérieur à la meilleur trouvé auparavant, alors nouvelle meilleur corrélation trouvé 
                # affichage des informations de la nouvelle corrélation
                x <- i
                y <- j
                max = currentCor 
            }
        }
    }
}
#Affiche la meilleure corrélation
print(sprintf("La corrélation maximum est entre %s et %s : %s" ,names(data_part2)[x], names(data_part2)[y], max), quote = FALSE)
print("-----", quote = FALSE)


#Regression linéaire des deux paire de variable (Y par rapport à X et X par rapport à Y)
dataX <- data_part2[,x]
dataY <- data_part2[,y]

regXY <- lm(dataX ~ dataY, data = data_part2)
regYX <- lm(dataY ~ dataX, data = data_part2)

# Calculer une prévision, avec chacune de ces deux régressions,
predXY <- predict(regXY, data.frame(dataY = 2*mean(dataY)))
predYX <- predict(regYX, data.frame(dataX = 2*mean(dataX)))
print(sprintf("La prédiction de X pour Y=%s est %s",round(2*mean(dataY),2),predXY[[1]]), quote = FALSE)
print(sprintf("La prédiction de Y pour X=%s est %s",round(2*mean(dataX),2),predYX[[1]]), quote = FALSE)

#Afficher ces deux prévisions ainsi que le nuage de point et la droite de regression
plot(dataX, dataY)
abline(regYX)

