

require(dplyr)
require(readxl)
require(xlsx)
require(stats)
require(formattable)


stat_simple <- function(data){
  
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
  cat('\n','----------------------','Parametres','----------------------','\n')
  print(param)
  
  #------------Stat simple
  
  stat <- data.frame(matrix(ncol = 0,nrow=2))
  row.names(stat)<- c("Score 1","Score 2")
  
  stat$Effectif <- c(length(data$`Score.1`),length(data$`Score.2`))
  stat$Moyenne <- c(round(mean(data$`Score.1`),1),round(mean(data$`Score.2`),1))
  stat$Ecart_type <- c(round(sd(data$`Score.1`),1),round(sd(data$`Score.2`),1))
  stat$MIN <- c(min(data$`Score.1`),min(data$`Score.2`))
  stat$MAX <- c(max(data$`Score.1`),max(data$`Score.2`))
  stat$MIN1 <- c(sort(unique(data$`Score.1`))[2],sort(unique(data$`Score.2`))[2])
  stat$MAX2 <- c(sort(unique(data$`Score.1`),decreasing = TRUE)[2],sort(unique(data$`Score.2`),decreasing = TRUE)[2])
  stat$VarCoeff <- c(round(sd(data$`Score.1`)/mean(data$`Score.1`),1),round(sd(data$`Score.2`)/mean(data$`Score.2`),1))
  stat$Median <- c(median(data$`Score.1`),median(data$`Score.2`))
  cat('\n','----------------------','Stat simple','----------------------','\n')
  print(stat)
}

tri_a_plat_fct <- function(data){
  #-------------Tris a plat
  
  dat <- data.frame(sapply(data,class))
  colnames(dat)<-"class"
  
  #Choix des variables nominales
  cat_col <- row.names(subset(dat,dat$class=="character"))
  cat_col <- cat_col[cat_col != "Identifiant.Client"]
  
  for (i in cat_col){
    tri_a_plat<-data.frame(matrix(ncol = 0,nrow=length(unique(data[[i]]))+1))
    row.names(tri_a_plat) <-c(row.names(as.matrix(table(data[[i]]))),"Ensemble") 
    tri_a_plat$Effectifs<-c(as.matrix(table(data[[i]])),0)
    tri_a_plat$Pourcentage<-c(as.matrix(prop.table(table(data[[i]])))*100,0)
    tri_a_plat$`Pourcentage cumule`<-c(as.matrix(prop.table(table(data[[i]],useNA = "no")))*100,0)
    tri_a_plat["Ensemble",]<-colSums(tri_a_plat)
    tri_a_plat <- tri_a_plat[c("Effectifs","Pourcentage","Pourcentage cumule")]
    tri_a_plat$Pourcentage<-round(tri_a_plat$Pourcentage,1)
    tri_a_plat$`Pourcentage cumul�`<-round(tri_a_plat$`Pourcentage cumule`,1)
    cat('\n','----------------------',i,'----------------------','\n')
    print(tri_a_plat)
  }
}

regroupement <- function(data){
  #-------Regroupement
  nb_individu <- nrow(data)
  nb_colonne <- ncol(data)
  
  data['Situation familiale regroupe']<-NA
  
  for(i in 1:nb_individu){
    if(data$Situation.familiale[i] == "veuf" || data$Situation.familiale[i] == "divorc�"){
      data$`Situation familiale regroupe`[i] <- "Divorc� ou Veuf"
    }else{
      data$`Situation familiale regroupe`[i] <- data$Situation.familiale[i]
    }
  }
  
  data["Domiciliation de l'�pargne regroupe"]<-NA
  
  for(i in 1:nb_individu){
    if(data$Domiciliation.de.l.�pargne[i] == "de 10 � 100K �pargne" 
       || data$Domiciliation.de.l.�pargne[i] == "plus de 100K �pargne"){
      data$`Domiciliation de l'�pargne regroupe`[i] <- "plus de 10K �pargne"
    }else{
      data$`Domiciliation de l'�pargne regroupe`[i] <- data$Domiciliation.de.l.�pargne[i]
    }
    
  }
  
  return(data)
}

reg_logistic <- function(data){
  #------- Modalites de reference
  
  data$Age.du.client <- relevel(factor(data$Age.du.client), ref = "moins de 23 ans")
  data$`Situation familiale regroupe` <- relevel(factor(data$`Situation familiale regroupe`), ref = "c�libataire")
  data$Anciennet� <- relevel(factor(data$Anciennet�), ref = "anc. 1 an  ou moins")
  data$Domiciliation.du.salaire <- relevel(factor(data$Domiciliation.du.salaire), ref = "Non domicili�")
  data$Profession <- relevel(factor(data$Profession), ref = "autre")
  data$Moyenne.encours <- relevel(factor(data$Moyenne.encours), ref = "moins de 2K encours")
  data$Moyenne.des.mouvements <- relevel(factor(data$Moyenne.des.mouvements), ref = "moins 10 K  mouvt")
  data$Cumul.des.d�bits <- relevel(factor(data$Cumul.des.d�bits), ref = "moins de 40  d�bits")
  data$`Domiciliation de l'�pargne regroupe` <- relevel(factor(data$`Domiciliation de l'�pargne regroupe`), ref = "pas d'�pargne")
  
  # Tirage aleaoire et sans remise des 70% des individus de l'echantillon On
  # initialise le tirage aleatoire afin de retomber sur nos pieds a chaque fois
  ind = sample(2,nrow(data), replace=T, prob=c(0.7,0.3))
  
  # Echantillon d'apprentissage
  train <- data[ind==1, ]
  # Echantillon de test
  test <- data[ind==2, ]
  
  
  #------ REG_LOGIS
  
  # Information generales
  param_reg <- data.frame(matrix(ncol=0,nrow=2))
  param_reg$Ordre <- c(1,2)
  param_reg$Modalite <- unique(data$Type.de.client)
  param_reg$Apprentissage <- table(train$Type.de.client)
  param_reg$Test <- table(test$Type.de.client)
  param_reg$Ensemble <- param_reg$Apprentissage+param_reg$Test
  
  print(param_reg)
  
  names <- names(train)[!names(train) %in% c("Score.1","Score.2","NA.","Identifiant.Client",
                                             "Type.de.client","Autorisation.de.d�couvert",
                                             "Interdiction.de.ch�quier")]
  
  dataStat <- data.frame(matrix(ncol=1,nrow=0))
  
  for (i in names){
    cat("\n","---------------",i,"------------","\n")
    table<-data.frame(t(table(train$Type.de.client, train[[i]])))
    dataB<-within(subset(table,table$Var2=="Bon client"),rm(Var2))
    dataM<-within(subset(table,table$Var2=="Mauvais client"),rm(Var2))
    dataMerge <- merge(dataB,dataM,by="Var1")
    dataMerge$Ensemble <- rowSums(dataMerge[names(dataMerge) %in% c("Freq.x","Freq.y")])
    dataStat <- dataMerge
    colnames(dataStat) <- c("Modalit�s","Bon client","Mauvais client","Ensemble")
    print(dataStat)
  }
  
  
  #------------REG_MODEL_COEF
  
  dataModel <- data.frame(matrix(nrow=22,ncol=0))
  
  myreg<-glm(factor(Type.de.client)~Age.du.client+Anciennet�+Domiciliation.du.salaire+
               Profession+Moyenne.encours+Moyenne.des.mouvements+Cumul.des.d�bits+
               `Situation familiale regroupe`+
               `Domiciliation de l'�pargne regroupe`
             ,data=train,family=binomial(link=logit))
  
  
  dataModel$Modalit� <- names(coef(myreg))
  dataModel$Coefficient <- coef(myreg)
  dataModel$STD.Error <- summary(myreg)$coefficients[,2]
  dataModel$P_valeur <- summary(myreg)$coefficients[,4]
  
  print(dataModel)
  
  
  #----------REG_MODEL_OR
  
  od_ratio <- data.frame(exp(cbind(OR = coef(myreg), confint(myreg))))
  od_ratio$Variables <- row.names(od_ratio)
  row.names(od_ratio)<-NULL
  od_ratio <- od_ratio[,c(4,1,2,3)]
  names(od_ratio)<-c("Variables","OR","Borne Inf�rieure","Borne sup�rieure")
  
  print(od_ratio)
  
  #------------MAT_CONF
  
  
  #Effectifs
  predict <- predict(myreg)
  table<-data.frame(table(train$Type.de.client, predict > 0.5))
  eff_appren <- data.frame(matrix(table$Freq,ncol=2,nrow=2))
  eff_appren$Total <- rowSums(eff_appren)
  eff_appren["Total",]<- colSums(eff_appren)
  row.names(eff_appren)<- c("Bon Client","Mauvais Client","Total")
  names(eff_appren)<- c("Class� Bon Client","Class� Mauvais Client","Total")
  print("Effectifs (apprentissage)")
  print(eff_appren)
  
  predict <-predict(myreg, test)
  table<-data.frame(table(test$Type.de.client, predict > 0.5))
  eff_test <- data.frame(matrix(table$Freq,ncol=2,nrow=2))
  eff_test$Total <- rowSums(eff_test)
  eff_test["Total",]<- colSums(eff_test)
  row.names(eff_test)<- c("Bon Client","Mauvais Client","Total")
  names(eff_test)<- c("Class� Bon Client","Class� Mauvais Client","Total")
  print("Effectifs (test)")
  print(eff_test)
  
  
  
  #Matrices de classement
  class_appren <- eff_appren
  class_appren["Bon Client",]<-round((eff_appren["Bon Client",][,c("Class� Bon Client","Class� Mauvais Client")]/eff_appren["Bon Client",]$Total)*100,2)
  class_appren["Mauvais Client",]<-round((eff_appren["Mauvais Client",][,c("Class� Bon Client","Class� Mauvais Client")][,c(2,1)]/eff_appren["Mauvais Client",]$Total)*100,2)
  class_appren$Total<-NULL
  class_appren["Total",]<-colMeans(class_appren[c("Bon Client","Mauvais Client"),])
  names(class_appren)<-c("Bien class�","Mal Class�")
  print("Matrice de classement en % (apprentissage)")
  print(class_appren)
  
  class_test <- eff_test
  class_test["Bon Client",]<-round((eff_test["Bon Client",][,c("Class� Bon Client","Class� Mauvais Client")]/eff_test["Bon Client",]$Total)*100,2)
  class_test["Mauvais Client",]<-round((eff_test["Mauvais Client",][,c("Class� Bon Client","Class� Mauvais Client")][,c(2,1)]/eff_test["Mauvais Client",]$Total)*100,2)
  class_test$Total<-NULL
  class_test["Total",]<-colMeans(class_test[c("Bon Client","Mauvais Client"),])
  print("Matrice de classement en % (test)")
  print(class_test)
  
  
  #Pourcentage en ligne
  pour_appren <- eff_appren
  pour_appren$`Class� Bon Client`<-round((eff_appren$`Class� Bon Client`/eff_appren$Total)*100,2)
  pour_appren$`Class� Mauvais Client`<-round((eff_appren$`Class� Mauvais Client`/eff_appren$Total)*100,2)
  pour_appren$Total<-round((eff_appren$Total/eff_appren$Total)*100,2)
  recallA<-pour_appren['Bon Client','Class� Bon Client']
  falloutA<-pour_appren['Mauvais Client','Class� Bon Client']
  specA<-pour_appren['Mauvais Client','Class� Mauvais Client']
  print("Pourcentages en ligne (apprentissage)")
  print(pour_appren)
  
  pour_test <- eff_test
  pour_test$`Class� Bon Client`<-round((eff_test$`Class� Bon Client`/eff_test$Total)*100,2)
  pour_test$`Class� Mauvais Client`<-round((eff_test$`Class� Mauvais Client`/eff_test$Total)*100,2)
  pour_test$Total<-round((eff_test$Total/eff_test$Total)*100,2)
  recallT<-pour_test['Bon Client','Class� Bon Client']
  falloutT<-pour_test['Mauvais Client','Class� Bon Client']
  specT<-pour_test['Mauvais Client','Class� Mauvais Client']
  print("Pourcentages en ligne (test)")
  print(pour_test)
  
  
  #Pourcentage en colonnes
  pour_appren_ <- eff_appren
  pour_appren_["Bon Client",]<-round((eff_appren["Bon Client",]/eff_appren["Total",])*100,2)
  pour_appren_["Mauvais Client",]<-round((eff_appren["Mauvais Client",]/eff_appren["Total",])*100,2)
  pour_appren_["Total",]<-round((eff_appren["Total",]/eff_appren["Total",])*100,2)
  precisionA<-pour_appren_['Bon Client','Class� Bon Client']
  fdrA<-pour_appren_['Mauvais Client','Class� Bon Client']
  print("Pourcentages en colonne (apprentissage)")
  print(pour_appren_)
  
  pour_test_ <- eff_test
  pour_test_["Bon Client",]<-round((eff_test["Bon Client",]/eff_test["Total",])*100,2)
  pour_test_["Mauvais Client",]<-round((eff_test["Mauvais Client",]/eff_test["Total",])*100,2)
  pour_test_["Total",]<-round((eff_test["Total",]/eff_test["Total",])*100,2)
  precisionT<-pour_test_['Bon Client','Class� Bon Client']
  fdrT<-pour_test_['Mauvais Client','Class� Bon Client']
  print("Pourcentages en colonne (test)")
  print(pour_test_)
  
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
  
  print(indicateur)
}