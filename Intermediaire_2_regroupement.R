#setwd("")

require(dplyr)
require(readxl)
require(assertthat)
require(XLConnect)
require(xlsx)
require(readxl)

my_data <- read_excel("data.xls")


#-------Regroupement
nb_individu <- nrow(my_data)
nb_colonne <- ncol(my_data)

my_data['Situation familiale regroupe']<-NA

for(i in 1:nb_individu){
  if(my_data$`Situation familiale`[i] == "veuf" || my_data$`Situation familiale`[i] == "divorcé"){
    my_data$`Situation familiale regroupe`[i] <- "Divorcé ou Veuf"
  }else{
    my_data$`Situation familiale regroupe`[i] <- my_data$`Situation familiale`[i]
  }
}

my_data["Domiciliation de l'épargne regroupe"]<-NA

for(i in 1:nb_individu){
  if(my_data$`Domiciliation de l'épargne`[i] == "de 10 Ã 100K épargne" || my_data$`Domiciliation de l'épargne`[i] == "plus de 100K épargne"){
    my_data$`Domiciliation de l'épargne regroupe`[i] <- "plus de 10K épargne"
  }else{
    my_data$`Domiciliation de l'épargne regroupe`[i] <- my_data$`Domiciliation de l'épargne`[i]
  }
}

data <- my_data

wb<-createWorkbook(type="xlsx")

# Define some cell styles
#++++++++++++++++++++
# Title and sub title styles
TITLE_STYLE <- CellStyle(wb)+ Font(wb,heightInPoints=16,color="blue", isBold=TRUE, underline=1)
SUB_TITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=14, isItalic=TRUE, isBold=FALSE)
# Styles for the data table row/column names
TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
  Alignment(wrapText=TRUE)
TITLE_TABLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=14, isItalic=FALSE, isBold=TRUE,color="chocolate1")


xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  xlsx::setCellStyle(sheetTitle[[1,1]], titleStyle)
}

#--------- Paramètres 

sheet1 <- xlsx::createSheet(wb, sheetName = "Paramètres")
# Add title
xlsx.addTitle(sheet1, rowIndex=1, title="Paramètres",titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet1, rowIndex=2,title="Quantités importantes",titleStyle = SUB_TITLE_STYLE)

param <- data.frame(matrix(ncol=1,nrow=0))
colnames(param) <- "Taille"

# Nombre d'observations
param["NB d'observations",]<- nrow(data)

# Variables Nominales et Continues
dat <- data.frame(sapply(data,class))
colnames(dat)<-"class"
param["NB de continues",]<-nrow(subset(dat,dat$class=="numeric"))
param["NB de nominales",]<-nrow(subset(dat,dat$class=="character"))

# Création de le feuille Paramètres
addDataFrame(param, sheet1, startRow=4, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
xlsx::setColumnWidth(sheet1, colIndex=c(1:ncol(param)), colWidth=20)


#--------------Tris à plat 

sheet3 <- xlsx::createSheet(wb, sheetName = "Tris à plat")
# Add title
xlsx.addTitle(sheet3, rowIndex=1, title="Tris à plat",titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet3, rowIndex=2,title="Regroupement des modalités par variables",titleStyle = SUB_TITLE_STYLE)

# Choix des variables 
tri_a_plat = data.frame(matrix(ncol=2,nrow=0))
cat_col = c("Situation familiale","Domiciliation de l'épargne",
            "Domiciliation de l'épargne regroupe",
            "Situation familiale regroupe")


count=4
for (i in cat_col){
  xlsx.addTitle(sheet3, rowIndex=count,title=i,
                titleStyle = TITLE_TABLE_STYLE)
  tri_a_plat=data.frame(matrix(ncol = 0,nrow=length(unique(data[[i]]))+1))
  row.names(tri_a_plat) <-c(row.names(as.matrix(table(data[[i]]))),"Ensemble") 
  tri_a_plat$Effectifs<-c(as.matrix(table(data[[i]])),0)
  tri_a_plat$Pourcentage<-c(as.matrix(prop.table(table(data[[i]])))*100,0)
  tri_a_plat$`Pourcentage cumulé`<-c(as.matrix(prop.table(table(data[[i]],useNA = "no")))*100,0)
  tri_a_plat["Ensemble",]<-colSums(tri_a_plat)
  tri_a_plat <- tri_a_plat[c("Effectifs","Pourcentage","Pourcentage cumulé")]
  tri_a_plat$Pourcentage<-round(tri_a_plat$Pourcentage,1)
  tri_a_plat$`Pourcentage cumulé`<-round(tri_a_plat$`Pourcentage cumulé`,1)
  addDataFrame(as.data.frame(tri_a_plat), sheet3, startRow=count+1, startColumn=1,row.names=TRUE, 
               colnamesStyle = TABLE_COLNAMES_STYLE,
               rownamesStyle = TABLE_ROWNAMES_STYLE)
  count=count+length(unique(data[[i]]))+4
}


xlsx::setColumnWidth(sheet3, colIndex=c(1:(ncol(tri_a_plat)+1)), colWidth=30)

# Save in one file
xlsx::saveWorkbook(wb, "Resultat Intermediaire 2 avec regroupement avec R.xlsx")

data$`Domiciliation de l'épargne`<-NULL
data$`Situation familiale`<-NULL

write.xlsx(data,file="data_recode.xlsx",row.names=TRUE)
