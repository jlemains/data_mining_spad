#setwd("")

require(dplyr)
require(readxl)
require(assertthat)
require(XLConnect)
require(xlsx)

data <- read_excel("data.xls")

#Parametres
param <- data.frame(matrix(ncol=1,nrow=0))
colnames(param) <- "Taille"

# Nombres d'observations
param["Observations",]<- nrow(data)

# Variables Nominales et Continues
dat <- data.frame(sapply(data,class))
colnames(dat)<-"class"
param["Variables quantitatives",]<-nrow(subset(dat,dat$class=="numeric"))
param["Variables qualitatives",]<-nrow(subset(dat,dat$class=="character"))

print(param)

#------------Stat simple

stat <- data.frame(matrix(ncol = 0,nrow=2))
row.names(stat)<- c("Score 1","Score 2")

stat$Effectif <- c(length(data$`Score 1`),length(data$`Score 2`))
stat$Moyenne <- c(round(mean(data$`Score 1`),1),round(mean(data$`Score 2`),1))
stat$Ecart_type <- c(round(sd(data$`Score 1`),1),round(sd(data$`Score 2`),1))
stat$MIN <- c(min(data$`Score 1`),min(data$`Score 2`))
stat$MAX <- c(max(data$`Score 1`),max(data$`Score 2`))
stat$MIN1 <- c(sort(unique(data$`Score 1`))[2],sort(unique(data$`Score 2`))[2])
stat$MAX2 <- c(sort(unique(data$`Score 1`),decreasing = TRUE)[2],sort(unique(data$`Score 2`),decreasing = TRUE)[2])
stat$VarCoeff <- c(round(sd(data$`Score 1`)/mean(data$`Score 1`),1),round(sd(data$`Score 2`)/mean(data$`Score 2`),1))
stat$Median <- c(median(data$`Score 1`),median(data$`Score 2`))

print(stat)

#-------------Tris a plat



#Choix des variables nominales
cat_col <- row.names(subset(dat,dat$class=="character"))
cat_col <- cat_col[cat_col != "Identifiant Client"]

for (i in cat_col){
  cat("\n","---------------",i,"------------","\n")
  tri_a_plat<-data.frame(matrix(ncol = 0,nrow=length(unique(data[[i]]))+1))
  row.names(tri_a_plat) <-c(row.names(as.matrix(table(data[[i]]))),"Ensemble") 
  tri_a_plat$Effectifs<-c(as.matrix(table(data[[i]])),0)
  tri_a_plat$Pourcentage<-c(as.matrix(prop.table(table(data[[i]])))*100,0)
  tri_a_plat$`Pourcentage cumule`<-c(as.matrix(prop.table(table(data[[i]],useNA = "no")))*100,0)
  tri_a_plat["Ensemble",]<-colSums(tri_a_plat)
  tri_a_plat <- tri_a_plat[c("Effectifs","Pourcentage","Pourcentage cumule")]
  tri_a_plat$Pourcentage<-round(tri_a_plat$Pourcentage,1)
  tri_a_plat$`Pourcentage cumul?`<-round(tri_a_plat$`Pourcentage cumule`,1)
  print(tri_a_plat)
}


