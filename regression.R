#setwd("")

require(dplyr)
require(readxl)
require(assertthat)
require(XLConnect)
require(xlsx)
require(readxl)
require(MASS)

wb<-createWorkbook(type="xlsx")

# Define some cell styles
#++++++++++++++++++++
# Title and sub title styles
TITLE_STYLE <- CellStyle(wb)+ Font(wb,heightInPoints=16,color="blue", isBold=TRUE, underline=1)
SUB_TITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=14, isItalic=TRUE, isBold=FALSE)
TITLE_TABLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=14, isItalic=FALSE, isBold=TRUE,color="chocolate1")
# Styles for the data table row/column names
TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
  Alignment(wrapText=TRUE)

xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  xlsx::setCellStyle(sheetTitle[[1,1]], titleStyle)
}

#------- Modalités de référence

data<- read.xlsx("data_recode.xlsx",sheetIndex=1,encoding = "UTF-8",stringsAsFactors=TRUE)

data$Age.du.client <- relevel(data$Age.du.client, ref = "moins de 23 ans")
data$Situation.familiale.regroupe <- relevel(data$Situation.familiale.regroupe, ref = "célibataire")
data$Ancienneté <- relevel(data$Ancienneté, ref = "anc. 1 an  ou moins")
data$Domiciliation.du.salaire <- relevel(data$Domiciliation.du.salaire, ref = "Non domicilié")
data$Profession <- relevel(data$Profession, ref = "autre")
data$Moyenne.encours <- relevel(data$Moyenne.encours, ref = "moins de 2K encours")
data$Moyenne.des.mouvements <- relevel(data$Moyenne.des.mouvements, ref = "moins 10 K  mouvt")
data$Cumul.des.débits <- relevel(data$Cumul.des.débits, ref = "moins de 40  débits")
data$Domiciliation.de.l.épargne.regroupe <- relevel(data$Domiciliation.de.l.épargne.regroupe, ref = "pas d'épargne")


# Tirage aléaoire et sans remise des 70% des individus de l'échantillon On
# initialise le tirage aléatoire afin de retomber sur nos pieds à chaque fois
ind = sample(2,nrow(data), replace=T, prob=c(0.7,0.3))
# Echantillon d'apprentissage

train <- data[ind==1, ]
# Echantillon de test
test <- data[ind==2, ]


#------ REG_LOGIS

sheet1 <- xlsx::createSheet(wb, sheetName = "REG_LOGIS")
# Add title
xlsx.addTitle(sheet1, rowIndex=1, title="Présentation du modèle",titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet1, rowIndex=2,title="Distribution de la variable à expliquer par échantillon"
              ,titleStyle = SUB_TITLE_STYLE)

# Information générales
param_reg <- data.frame(matrix(ncol=0,nrow=2))
param_reg$Ordre <- c(1,2)
param_reg$Modalité <- unique(data$Type.de.client)
param_reg$Apprentissage <- table(train$Type.de.client)
param_reg$Test <- table(test$Type.de.client)
param_reg$Ensemble <- param_reg$Apprentissage+param_reg$Test

addDataFrame(param_reg, sheet1, startRow=4, startColumn=1,row.names=FALSE, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
xlsx::setColumnWidth(sheet1, colIndex=c(1:ncol(param_reg)), colWidth=15)






#---------- STAT
sheet2 <- xlsx::createSheet(wb, sheetName = "STAT")
# Add title
xlsx.addTitle(sheet2, rowIndex=1, title="Statistiques descriptives sur les variables explicatives",
              titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet2, rowIndex=2,title="Tri à plat des variables nominales (échantillons d'apprentissage)",
              titleStyle = SUB_TITLE_STYLE)

temp <- data.frame(matrix(ncol=3,nrow=1))
colnames(temp)<- c("Mod","Bon client","Mauvais client")

names <- names(train)[!names(train) %in% c("Score.1","Score.2","NA.","Identifiant.Client",
                                           "Type.de.client","Autorisation.de.découvert",
                                           "Interdiction.de.chéquier")]

dataStat <- data.frame(matrix(ncol=1,nrow=0))

count=0
for (i in names){
  
  xlsx.addTitle(sheet2, rowIndex=count+4,title=gsub("[.]"," ",i),
                titleStyle = TITLE_TABLE_STYLE)
  table<-data.frame(t(table(train$Type.de.client, train[[i]])))
  dataB<-within(subset(table,table$Var2=="Bon client"),rm(Var2))
  dataM<-within(subset(table,table$Var2=="Mauvais client"),rm(Var2))
  dataMerge <- merge(dataB,dataM,by="Var1")
  dataMerge$Ensemble <- rowSums(dataMerge[names(dataMerge) %in% c("Freq.x","Freq.y")])
  dataStat <- dataMerge
  colnames(dataStat) <- c("Modalités","Bon client","Mauvais client","Ensemble")
  addDataFrame(dataStat, sheet2, startRow=count+5, startColumn=1,row.names=FALSE, 
               colnamesStyle = TABLE_COLNAMES_STYLE,
               rownamesStyle = TABLE_ROWNAMES_STYLE)
  count=count+nrow(dataStat)+3
}

xlsx::setColumnWidth(sheet2, colIndex=c(1:ncol(dataStat)), colWidth=25)

#------------REG_MODEL_COEF

sheet3 <- xlsx::createSheet(wb, sheetName = "REG_MODEL")
# Add title
xlsx.addTitle(sheet3, rowIndex=1, title="Coefficients du modèle par variable",
              titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet3, rowIndex=2,title="Coefficients de régression estimés par maximum de vraisemblance",
              titleStyle = SUB_TITLE_STYLE)

dataModel <- data.frame(matrix(nrow=23,ncol=0))

myreg<-glm(Type.de.client~Age.du.client+Ancienneté+Domiciliation.du.salaire+
             Profession+Moyenne.encours+Moyenne.des.mouvements+Cumul.des.débits+
             Situation.familiale.regroupe+
             Domiciliation.de.l.épargne.regroupe
           ,data=train,family=binomial(link=logit))


dataModel$Modalité <- names(coef(myreg))
dataModel$Coefficient <- coef(myreg)
dataModel$STD.Error <- summary(myreg)$coefficients[,2]
dataModel$P_valeur <- summary(myreg)$coefficients[,4]

# Création de la feuille "REG_MODEL_COEF"
addDataFrame(dataModel, sheet3, startRow=4, startColumn=1,row.names=FALSE, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
xlsx::setColumnWidth(sheet3, colIndex=c(1:ncol(dataModel)), colWidth=30)


#----------REG_MODEL_OR

xlsx.addTitle(sheet3, rowIndex=30, title="Estimation des 'Odds Ratio'",
              titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet3, rowIndex=31,title="(Intervalle de confiance à 95%)",
              titleStyle = SUB_TITLE_STYLE)

od_ratio <- data.frame(exp(cbind(OR = coef(myreg), confint(myreg))))
od_ratio$Variables <- row.names(od_ratio)
row.names(od_ratio)<-NULL
od_ratio <- od_ratio[,c(4,1,2,3)]
names(od_ratio)<-c("Variables","OR","Borne Inférieure","Borne supérieure")

# Création de la feuille "REG_MODEL_OR"
addDataFrame(od_ratio, sheet3, startRow=32, startColumn=1,row.names=FALSE, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)


#------------MAT_CONF


sheet4 <- xlsx::createSheet(wb, sheetName = "MAT_CONF")
# Add title
xlsx.addTitle(sheet4, rowIndex=1, title="Matrice de confusion",
              titleStyle = TITLE_STYLE)

add_to_sheet<- function(sheet,data,title_,start){
  xlsx.addTitle(sheet, rowIndex=start,title=title_,
                titleStyle = TITLE_TABLE_STYLE)
  addDataFrame(data, sheet, startRow=start+1, startColumn=1,row.names=TRUE, 
              colnamesStyle = TABLE_COLNAMES_STYLE,
              rownamesStyle = TABLE_ROWNAMES_STYLE)
}

#Effectifs
predict <- predict(myreg)
table<-data.frame(table(train$Type.de.client, predict > 0.5))
eff_appren <- data.frame(matrix(table$Freq,ncol=2,nrow=2))
eff_appren$Total <- rowSums(eff_appren)
eff_appren["Total",]<- colSums(eff_appren)
row.names(eff_appren)<- c("Bon Client","Mauvais Client","Total")
names(eff_appren)<- c("Classé Bon Client","Classé Mauvais Client","Total")
add_to_sheet(sheet4,eff_appren,"Effectifs (apprentissage)",3)

predict <-predict(myreg, test)
table<-data.frame(table(test$Type.de.client, predict > 0.5))
eff_test <- data.frame(matrix(table$Freq,ncol=2,nrow=2))
eff_test$Total <- rowSums(eff_test)
eff_test["Total",]<- colSums(eff_test)
row.names(eff_test)<- c("Bon Client","Mauvais Client","Total")
names(eff_test)<- c("Classé Bon Client","Classé Mauvais Client","Total")
add_to_sheet(sheet4,eff_test,"Effectifs (test)",9)

#Matrices de classement
class_appren <- eff_appren
class_appren["Bon Client",]<-round((eff_appren["Bon Client",][,c("Classé Bon Client","Classé Mauvais Client")]/eff_appren["Bon Client",]$Total)*100,2)
class_appren["Mauvais Client",]<-round((eff_appren["Mauvais Client",][,c("Classé Bon Client","Classé Mauvais Client")][,c(2,1)]/eff_appren["Mauvais Client",]$Total)*100,2)
class_appren$Total<-NULL
class_appren["Total",]<-colMeans(class_appren[c("Bon Client","Mauvais Client"),])
names(class_appren)<-c("Bien classé","Mal Classé")
add_to_sheet(sheet4,class_appren,"Matrice de classement en % (apprentissage)",15)

class_test <- eff_test
class_test["Bon Client",]<-round((eff_test["Bon Client",][,c("Classé Bon Client","Classé Mauvais Client")]/eff_test["Bon Client",]$Total)*100,2)
class_test["Mauvais Client",]<-round((eff_test["Mauvais Client",][,c("Classé Bon Client","Classé Mauvais Client")][,c(2,1)]/eff_test["Mauvais Client",]$Total)*100,2)
class_test$Total<-NULL
class_test["Total",]<-colMeans(class_test[c("Bon Client","Mauvais Client"),])
add_to_sheet(sheet4,class_test,"Matrice de classement en % (test)",21)

#Pourcentage en ligne
pour_appren <- eff_appren
pour_appren$`Classé Bon Client`<-round((eff_appren$`Classé Bon Client`/eff_appren$Total)*100,2)
pour_appren$`Classé Mauvais Client`<-round((eff_appren$`Classé Mauvais Client`/eff_appren$Total)*100,2)
pour_appren$Total<-round((eff_appren$Total/eff_appren$Total)*100,2)
recallA<-pour_appren['Bon Client','Classé Bon Client']
falloutA<-pour_appren['Mauvais Client','Classé Bon Client']
specA<-pour_appren['Mauvais Client','Classé Mauvais Client']
add_to_sheet(sheet4,pour_appren,"Pourcentages en ligne (apprentissage)",27)

pour_test <- eff_test
pour_test$`Classé Bon Client`<-round((eff_test$`Classé Bon Client`/eff_test$Total)*100,2)
pour_test$`Classé Mauvais Client`<-round((eff_test$`Classé Mauvais Client`/eff_test$Total)*100,2)
pour_test$Total<-round((eff_test$Total/eff_test$Total)*100,2)
recallT<-pour_test['Bon Client','Classé Bon Client']
falloutT<-pour_test['Mauvais Client','Classé Bon Client']
specT<-pour_test['Mauvais Client','Classé Mauvais Client']
add_to_sheet(sheet4,pour_test,"Pourcentages en ligne (test)",33)

#Pourcentage en colonnes
pour_appren_ <- eff_appren
pour_appren_["Bon Client",]<-round((eff_appren["Bon Client",]/eff_appren["Total",])*100,2)
pour_appren_["Mauvais Client",]<-round((eff_appren["Mauvais Client",]/eff_appren["Total",])*100,2)
pour_appren_["Total",]<-round((eff_appren["Total",]/eff_appren["Total",])*100,2)
precisionA<-pour_appren_['Bon Client','Classé Bon Client']
fdrA<-pour_appren_['Mauvais Client','Classé Bon Client']
add_to_sheet(sheet4,pour_appren_,"Pourcentages en colonne (apprentissage)",39)

pour_test_ <- eff_test
pour_test_["Bon Client",]<-round((eff_test["Bon Client",]/eff_test["Total",])*100,2)
pour_test_["Mauvais Client",]<-round((eff_test["Mauvais Client",]/eff_test["Total",])*100,2)
pour_test_["Total",]<-round((eff_test["Total",]/eff_test["Total",])*100,2)
precisionT<-pour_test_['Bon Client','Classé Bon Client']
fdrT<-pour_test_['Mauvais Client','Classé Bon Client']
add_to_sheet(sheet4,pour_test_,"Pourcentages en colonne (test)",45)

#Indicateur : 
indicateur <- data.frame(matrix(ncol=0,nrow=2))
indicateur$Recall <- round(c(recallA,recallT),1)
indicateur$Fallout <- round(c(falloutA,falloutT),1)
indicateur$Precision <- round(c(precisionA,precisionT),1)
indicateur$Specificity <- round(c(specA,specT),1)
indicateur$`False discovery rate` <- round(c(fdrA,fdrT),1)
indicateur$FScore <- round(2*(indicateur$Precision*indicateur$Recall/(indicateur$Precision+indicateur$Recall)),1)

indicateur <- data.frame(t(indicateur))
names(indicateur)<-c("Apprentissage","Test")

add_to_sheet(sheet4,indicateur,"Indicateur",51)

xlsx::setColumnWidth(sheet4, colIndex=c(1:(ncol(eff_appren)+1)), colWidth=20)


xlsx::saveWorkbook(wb, "Resultat final regression avec R.xlsx")