library(foreign)
library(dplyr)
library(univOutl)
library(gbm)

setwd("D:/ANA_MARIA/Task13_SURSE_NOU/SURSE/Ind_calitate")
sursa2015 <- read.dbf("SURSA_2015.DBF")
sursa2016 <- read.dbf("SURSA_2016.DBF")
sursa2017 <- read.dbf("sursa_2017.DBF")
sursa2018 <- read.dbf("sursa_2018.DBF")


################################################ Helpers ##############################################################
#######################################################################################################################

# Campuri cu litere mici
clean <- function (df) {
  column_names <- names(df)
  pos_new_col <- grep("(^x$|^x.)", tolower(column_names))
  last_col <- as.integer(seq(from=ncol(df)-7, to = ncol(df), by = 1))
  if (identical(pos_new_col, last_col)) df <- df[,-pos_new_col] # elimina coloane adaugate in plus
  names(df) <- tolower(names(df))
  return (df)
}

# Elimina pfa, ong, public
remove_unit <- function (sursa) {
  sursa <- clean(sursa)
  if (is.na(sursa$tip_un[1])) {
    sursa <- subset(sursa, tip < 41)
  } else {
    sursa <- subset(sursa, tip_un < 41)
  }
  return (sursa)
}

add_cls_marime <- function (sursa) {
  pos_nrmediu_sal <- grep("(r14)", names(sursa))
  nrmediu <- sursa[,pos_nrmediu_sal]
  sursa$nrmediu_sal <- rowMeans(nrmediu, na.rm = TRUE)
  sursa$cls_marime <- ifelse(sursa$nrmediu_sal >= 0 & sursa$nrmediu_sal <= 9, 1, 
                             ifelse(sursa$nrmediu_sal > 9 & sursa$nrmediu_sal <= 49, 2, 
                                    ifelse(sursa$nrmediu_sal > 49 & sursa$nrmediu_sal <= 249, 3, 4)))
  return(sursa)
}

add_div_caen <- function(sursa) {
  sursa$div <- ifelse(sursa[,c(2)]==0, ifelse(nchar(sursa$caen01) == 4, round(sursa$caen01/100), round(sursa$caen01/10)),
                              ifelse(nchar(sursa[,c(2)]) == 4, round(sursa[,c(2)]/100), round(sursa[,c(2)]/10)))
  return(sursa)
}


################################################ # 1. Indicator missing ###############################################
#######################################################################################################################

indicator_missing = function(sursa){ 
  sursa <- clean(sursa)
  sursa <- remove_unit(sursa)
  sursa <- add_cls_marime(sursa)
  indicator = list()
  for (i in 1:4) { 
    val_lipsa = subset(sursa, is.na(t_r1) & is.na(t_r2) & is.na(t_r3) &
                         is.na(t_r4) & is.na(t_r5) & is.na(t_r6) &
                         is.na(t_r7) & is.na(t_r8) & is.na(t_r9) & 
                         is.na(t_r10) & is.na(t_r11) & is.na(t_r12) & 
                         cls_marime == i) 
    val_total = subset(sursa, cls_marime == i)
    indicator[i] = nrow(val_lipsa)/nrow(sursa)*100 
  } 
  indicator_missing = data.frame(cls1 = c(indicator[[1]]), cls2 = c(indicator[[2]]), cls3 = c(indicator[[3]]), cls4 = c(indicator[[4]])) 
  return(indicator_missing) 
}

indicator_missing(sursa2015)
indicator_missing(sursa2016)
indicator_missing(sursa2017)
indicator_missing(sursa2018)

miss <- function (sursa) {
  sursa <- clean(sursa)
  sursa <- remove_unit(sursa)
  val_lipsa = subset(sursa, is.na(t_r1) & is.na(t_r2) & is.na(t_r3) &
                       is.na(t_r4) & is.na(t_r5) & is.na(t_r6) &
                       is.na(t_r7) & is.na(t_r8) & is.na(t_r9) & 
                       is.na(t_r10) & is.na(t_r11) & is.na(t_r12))
  indicator = nrow(val_lipsa)/nrow(sursa)*100 
  return (indicator)
}


miss(sursa2015)
miss(sursa2016)
miss(sursa2017)
miss(sursa2018)


################################################ # 2. Indicator periodicity ###############################################
#######################################################################################################################


periodicitate = function(sursa) { 
  periodicitate <- sursa %>% group_by(t_r1, t_r2, t_r3, t_r4,
                                      t_r5, t_r6, t_r7, t_r8,
                                      t_r9, t_r10, t_r11, t_r12) %>% dplyr :: summarise(n=n()) 
  periodicitate <- as.data.frame(arrange(periodicitate, desc(n))) 
  return(periodicitate) 
}


indicator_period <- function (sursa) {
  sursa <- clean(sursa)
  sursa <- remove_unit(sursa)
  val_lipsa <- subset(sursa, is.na(t_r1) & is.na(t_r2) & is.na(t_r3) &
                       is.na(t_r4) & is.na(t_r5) & is.na(t_r6) &
                       is.na(t_r7) & is.na(t_r8) & is.na(t_r9) & 
                       is.na(t_r10) & is.na(t_r11) & is.na(t_r12))
  pos <- !(sursa$cod %in% val_lipsa$cod)
  sursa <- sursa[pos,]
  period <- periodicitate(sursa)
  missing_values <- 0
  monthly <- 0 
  quarterly <- 0
  others <- 0
  val_missing <- 0
  val_month <- 0
  val_quart <- 0
  
  for (i in 1:6) {
    # cond1 <- is.na(period$t_r1[i]) & is.na(period$t_r2[i]) & is.na(period$t_r3[i]) & is.na(period$t_r4[i]) &
    #   is.na(period$t_r5[i]) & is.na(period$t_r6[i]) & is.na(period$t_r7[i]) & is.na(period$t_r8[i]) &
    #   is.na(period$t_r9[i]) & is.na(period$t_r10[i]) & is.na(period$t_r11[i]) & is.na(period$t_r12[i])
    cond2 <- !is.na(period$t_r1[i]) & !is.na(period$t_r2[i]) & !is.na(period$t_r3[i]) & !is.na(period$t_r4[i]) &
      !is.na(period$t_r5[i]) & !is.na(period$t_r6[i]) & !is.na(period$t_r7[i]) & !is.na(period$t_r8[i]) &
      !is.na(period$t_r9[i]) & !is.na(period$t_r10[i]) & !is.na(period$t_r11[i]) & !is.na(period$t_r12[i])
    cond3 <- is.na(period$t_r1[i]) & is.na(period$t_r2[i]) & !is.na(period$t_r3[i]) & 
      is.na(period$t_r4[i]) & is.na(period$t_r5[i]) & !is.na(period$t_r6[i]) & 
      is.na(period$t_r7[i]) & is.na(period$t_r8[i]) & !is.na(period$t_r9[i]) & 
      is.na(period$t_r10[i]) & is.na(period$t_r11[i]) & !is.na(period$t_r12[i]) 
    
    
    # if (cond1) {
    #   val_missing <- period$n[i]
    #   missing_values <- val_missing/nrow(sursa)*100
    # }
    
    if (cond2) {
      val_month <- period$n[i]
      monthly <- val_month/nrow(sursa)*100
    }
    
    if (cond3) {
      val_quart <- period$n[i]
      quarterly <- val_quart/nrow(sursa)*100
    }
    
  }
  
  others <- (nrow(sursa) - (val_missing + val_month + val_quart))/nrow(sursa)*100
  
  ind <- data.frame(missing_values = missing_values, monthly= monthly, quarterly = quarterly, others = others)
  return (ind)
}


indicator_period(sursa2015)
indicator_period(sursa2016)
indicator_period(sursa2017)
indicator_period(sursa2018)



################################################ # 3. Data patterns ###################################################
#######################################################################################################################


calculeazaPatternValoriAberante = function(sursa){ 
  sursa <- clean(sursa)
  sursa <- remove_unit(sursa)
  quarterly <- subset(sursa,  is.na(t_r1) & is.na(t_r2) & t_r3 == 1 & is.na(t_r4) & 
                        is.na(t_r5) & t_r6 == 1 & is.na(t_r7) & is.na(t_r8) &
                        t_r9 == 1 & is.na(t_r10) & is.na(t_r11) & t_r12 == 1)
  p1 = subset(sursa,  ca_03 == 0 & ca_06 == 0 & ca_09 == 0 & ca_12 > 0 &
                is.na(t_r1) & is.na(t_r2) & t_r3 == 1 & is.na(t_r4) & 
                is.na(t_r5) & t_r6 == 1 & is.na(t_r7) & is.na(t_r8) & 
                t_r9 == 1 & is.na(t_r10) & is.na(t_r11) & t_r12 == 1)
  procent1 = paste0(round(nrow(p1)/nrow(quarterly) * 100, 2), "% ")
  p2 = subset(sursa, ca_03 == 0 & ca_06 > 0 & ca_09 > 0 & ca_12 > 0 & 
                is.na(t_r1) & is.na(t_r2) & t_r3 == 1 & is.na(t_r4) &
                is.na(t_r5) & t_r6 == 1 & is.na(t_r7) & is.na(t_r8) &
                t_r9 == 1 & is.na(t_r10) & is.na(t_r11) & t_r12 == 1) 
  procent2 = paste0(round(nrow(p2)/nrow(quarterly) * 100, 2), "% ")
  p3 = subset(sursa, ca_03 == ca_06 & ca_06 == ca_09 & ca_09 == ca_12 & 
                is.na(t_r1) & is.na(t_r2) & t_r3 == 1 & is.na(t_r4) &
                is.na(t_r5) & t_r6 == 1 & is.na(t_r7) & is.na(t_r8) & 
                t_r9 == 1 & is.na(t_r10) & is.na(t_r11) & t_r12 == 1) 
  procent3 = paste0(round(nrow(p3)/nrow(quarterly) * 100, 2), "% ")
  p4 = subset(sursa, ca_03 == ca_06 & ca_06 == ca_09 & ca_12 > 0 & ca_12 != ca_09 &
                is.na(t_r1) & is.na(t_r2) & t_r3 == 1 & is.na(t_r4) &
                is.na(t_r5) & t_r6 == 1 & is.na(t_r7) & is.na(t_r8) & 
                t_r9 == 1 & is.na(t_r10) & is.na(t_r11) & t_r12 == 1)
  procent4 = paste0(round(nrow(p4)/nrow(quarterly) * 100, 2), "% ")
  p5 = subset(sursa, (ca_03 < 0 | ca_06 < 0 | ca_09 < 0 | ca_12 < 0) &
                (is.na(t_r1) & is.na(t_r2) & t_r3 == 1 & is.na(t_r4) &
                is.na(t_r5) & t_r6 == 1 & is.na(t_r7) & is.na(t_r8) & 
                t_r9 == 1 & is.na(t_r10) & is.na(t_r11) & t_r12 == 1))
  procent5 = paste0(round(nrow(p5)/nrow(quarterly) * 100, 2), "% ")
  pattern = cat( "Procentul unitatilor care au depus cifra de afaceri 0 in primele trei trimestre 
                 si cifra de afaceri pozitiva in trimestrul patru este:",procent1, "\n", 
                 "Procentul unitatilor care au depus cifra de afaceri 0 in primul trimestru si
                 valori pozitive ale cifrei de afaceri in celelalte trei trimestre este:",
                 procent2, "\n", "Procentul unitatilor care au depus aceeasi cifra de afaceri in
                 toate cele patru trimestre este:",procent3,"\n",
                 "Procentul unitatilor au depus aceeasi cifra de afaceri pe trei trimestre si o alta
                 valoare pozitiva in trimestrul patru este:", procent4, "\n", "Procentul unitatilor 
                 care au depus valori negative pentru cifra de afaceri:", procent5)
}


calculeazaPatternValoriAberante(sursa2015)
calculeazaPatternValoriAberante(sursa2016)
calculeazaPatternValoriAberante(sursa2017)
calculeazaPatternValoriAberante(sursa2018)



################################################ # 4. Overcoverage / Undercoverage ####################################
#######################################################################################################################

fb2015 <- read.dbf("fb15.dbf")
finan15 <- read.dbf("finan15.dbf")
nrow(fb2015)

indicator_overcoverage = function(sursa, REGIS){ 
  indicator_overcoverage = nrow(subset(sursa, !(sursa$COD %in% as.list(REGIS$codi))))/ nrow(REGIS) * 100     
  return(indicator_overcoverage) 
}

indicator_undercoverage = function(sursa, REGIS){ 
  indicator_undercoverage = nrow(subset(REGIS, !(REGIS$codi %in% as.list(sursa$COD))))/ nrow(REGIS) * 100       
  return(indicator_undercoverage) 
}

indicator_overcoverage(sursa2015, xy)
indicator_undercoverage(sursa2015, xy)

x = data.frame(codi = finan15$SIRUES, deni = finan15$SIRUES)
y = data.frame(codi = fb2015$SIRUES, deni = fb2015$SIRUES)
xy = rbind(x,y)



# ERORI
###################################################### # Unit Errors ##############################################
##################################################################################################################

calculeazaEroriUnitateDeMasura = function(sursa_an_curent, sursa_an_precedent){
  sursa_an_curent <- clean(sursa_an_curent)
  sursa_an_curent <- remove_unit(sursa_an_curent)
  sursa_an_curent <- add_cls_marime(sursa_an_curent)
  
  sursa_an_curent <- subset(sursa_an_curent, !is.na(sursa_an_curent$t_r1) & !is.na(sursa_an_curent$t_r2) & !is.na(sursa_an_curent$t_r3) & 
                              !is.na(sursa_an_curent$t_r4) & !is.na(sursa_an_curent$t_r5) & !is.na(sursa_an_curent$t_r6) & 
                              !is.na(sursa_an_curent$t_r7) & !is.na(sursa_an_curent$t_r8) & !is.na(sursa_an_curent$t_r9) & 
                              !is.na(sursa_an_curent$t_r10) & !is.na(sursa_an_curent$t_r11) & !is.na(sursa_an_curent$t_r12))
  
  sursa_an_precedent <- clean(sursa_an_precedent)
  sursa_an_precedent <- remove_unit(sursa_an_precedent)
  sursa_an_precedent <- add_cls_marime(sursa_an_precedent)
  
  sursa_an_precedent <- subset(sursa_an_precedent, !is.na(sursa_an_precedent$t_r1) & !is.na(sursa_an_precedent$t_r2) & !is.na(sursa_an_precedent$t_r3) & 
                                 !is.na(sursa_an_precedent$t_r4) & !is.na(sursa_an_precedent$t_r5) & !is.na(sursa_an_precedent$t_r6) & 
                                 !is.na(sursa_an_precedent$t_r7) & !is.na(sursa_an_precedent$t_r8) & !is.na(sursa_an_precedent$t_r9) & 
                                 !is.na(sursa_an_precedent$t_r10) & !is.na(sursa_an_precedent$t_r11) & !is.na(sursa_an_precedent$t_r12))
  
  #Fixarea parametrilor
  n = 12 #numar de luni 
  A = 0.00065
  B = 0.00135
  
  colnames(sursa_an_precedent)[which(names(sursa_an_precedent) == "ca_12")] = "ca_12_rr"
  # Join intre sursa_an_curent si sursa_an_precedent: 
  # Avem nevoie de luna 12 din sursa_an_precedent
  sursa_an_curent = left_join(sursa_an_curent, sursa_an_precedent[,c("cod","ca_12_rr")], by = c("cod"))
  for(i in 2:n){ 
    col1 = paste0("Metoda0_",i)
    col2 = paste0("t_r",i)
    col3 = paste0(ifelse(i <= 9, "ca_0", "ca_"), i)
    col4 = paste0(ifelse(i-1 <= 9, "ca_0", "ca_"), i-1)
    sursa_an_curent[,"Metoda0_1"] = ifelse(sursa_an_curent[,"t_r1"] == 1 &
                                             sursa_an_curent[,"ca_12_rr"]!= 0 & 
                                             sursa_an_curent[,"ca_01"]/sursa_an_curent[,"ca_12_rr"] > A &
                                             sursa_an_curent[,"ca_01"]/sursa_an_curent[,"ca_12_rr"] < B, 1, 0)
    sursa_an_curent$Metoda0_1[is.na(sursa_an_curent$Metoda0_1)] = 0
    sursa_an_curent[,col1] = ifelse(sursa_an_curent[,col2] == 1 & 
                                      sursa_an_curent[,col4] != 0 & 
                                      sursa_an_curent[,col3]/sursa_an_curent[,col4] > A &
                                      sursa_an_curent[,col3]/sursa_an_curent[,col4] < B , 1, 0)
  }
  # Numarul de outliers pe fiecare luna
  p = colSums(sursa_an_curent %>% select(Metoda0_1:Metoda0_12), na.rm = TRUE)
  # Procent de outliers pe fiecare luna
  procent = vector("numeric", 12L)
  for (i in 1:n){
    col = paste0("t_r", i)
    procent[i] = paste0(round(p[[i]]*100/sum(sursa_an_curent[,col]),2), "% ")
  }
  luna = c("ianuarie", "februarie", "martie", "aprilie",
           "mai", "iunie", "iulie", "august", "septembrie",
           "octombrie", "noiembrie", "decembrie")
  procent = data.frame(luna, procent)
  return(sursa_an_curent)
  # Scrierea fisierului cu marcarea erorilor in coloanele Metoda0_1:Metoda0_12 
  # 1 - cifra de afaceri este aberanta 
  # 0 - cifra de afaceri nu este aberanta
  ## write.csv(sursa_an_curent, "path/sursa_metoda0.csv")
} 

s2018 <- calculeazaEroriUnitateDeMasura(sursa2015, sursa2016)
s2018 <- s2018[,-c(178)]
write.dbf(s2018, "sursa_2015_met0.dbf")

############################# # Comparison with reporting history for the business ####################################
#######################################################################################################################

calculeazaMetodaComparatie = function(sursa_an_curent, sursa_an_precedent){
  
  sursa_an_curent <- clean(sursa_an_curent)
  sursa_an_curent <- remove_unit(sursa_an_curent)
  sursa_an_curent <- add_cls_marime(sursa_an_curent)
  
  sursa_an_curent <- subset(sursa_an_curent, !is.na(sursa_an_curent$t_r1) & !is.na(sursa_an_curent$t_r2) & !is.na(sursa_an_curent$t_r3) & 
                          !is.na(sursa_an_curent$t_r4) & !is.na(sursa_an_curent$t_r5) & !is.na(sursa_an_curent$t_r6) & 
                          !is.na(sursa_an_curent$t_r7) & !is.na(sursa_an_curent$t_r8) & !is.na(sursa_an_curent$t_r9) & 
                          !is.na(sursa_an_curent$t_r10) & !is.na(sursa_an_curent$t_r11) & !is.na(sursa_an_curent$t_r12))
  
  sursa_an_precedent <- clean(sursa_an_precedent)
  sursa_an_precedent <- remove_unit(sursa_an_precedent)
  sursa_an_precedent <- add_cls_marime(sursa_an_precedent)
  
  sursa_an_precedent <- subset(sursa_an_precedent, !is.na(sursa_an_precedent$t_r1) & !is.na(sursa_an_precedent$t_r2) & !is.na(sursa_an_precedent$t_r3) & 
                              !is.na(sursa_an_precedent$t_r4) & !is.na(sursa_an_precedent$t_r5) & !is.na(sursa_an_precedent$t_r6) & 
                              !is.na(sursa_an_precedent$t_r7) & !is.na(sursa_an_precedent$t_r8) & !is.na(sursa_an_precedent$t_r9) & 
                              !is.na(sursa_an_precedent$t_r10) & !is.na(sursa_an_precedent$t_r11) & !is.na(sursa_an_precedent$t_r12))
  
  #Fixarea parametrilor
  n = 12 #numar de luni
  c = 10000
  
  # Modificare denumire coloana ca_12_r din sursa_an_precedent 
  # Pentru a nu se face confuzie cu ca_12_r din sursa_2016
  colnames(sursa_an_precedent)[which(names(sursa_an_precedent) == "ca_12")] = "ca_12_rr"
  # Media cifrelor de afaceri pe ultimele 12 luni - sursa_an_precedent
  sursa_an_precedent[,"media"] = 10 * rowMeans(sursa_an_precedent[, c("ca_01","ca_02",
                                                                      "ca_03","ca_04","ca_05", "ca_06","ca_07",
                                                                      "ca_08", "ca_09", "ca_10","ca_11", "ca_12_rr")])
  
  sursa_an_curent = left_join(sursa_an_curent, sursa_an_precedent[,c("cod","media")], by = c("cod"))
  
  for(i in 1:n){
    col1 = paste0("Metoda3_",i)
    col2 = paste0("t_r",i)
    col3 = paste0(ifelse(i <= 9, "ca_0", "ca_"), i)
    
    sursa_an_curent[,col1] = ifelse(sursa_an_curent[,col2] == 1 & 
                                      sursa_an_curent[,col3] > sursa_an_curent[,"media"] & 
                                      sursa_an_curent[,col3] > c , 1, 0)
  }
  
  p = colSums(sursa_an_curent %>% select(Metoda3_1:Metoda3_12), na.rm = TRUE)

  # Calcularea procentului 
  procent = vector("numeric", 12L)
  for (i in 1:n){
    col = paste0("t_r", i)
    print(sum(sursa_an_curent[,col]))
    procent[i] = paste0( round(p[[i]]*100/sum(sursa_an_curent[,col], na.rm = TRUE),2), " % ")
  }
  
  luna = c("ianuarie", "februarie", "martie", "aprilie", "mai", "iunie", "iulie",
           "august", "septembrie", "octombrie", "noiembrie", "decembrie")
  procent = data.frame(luna, procent)
  return(sursa_an_curent)
  
  # Scrierea fisierului cu marcarea erorilor in coloanele Metoda3_1:Metoda3_12
  # 1 - cifra de afaceri este aberanta
  # 0 - cifra de afaceri nu este aberanta
  ##write.csv(sursa_an_curent, "path/sursa_metoda3.csv")
}

s2018 <- calculeazaMetodaComparatie(sursa2018, sursa2017)

write.dbf(s2018, "sursa_2018_met1.dbf")

calculeazaMetodaComparatie(sursa2018, sursa2017)

############################# # Quartile distances #####################################################################
#######################################################################################################################


out <- function (sursa) {
  sursa <- clean(sursa)
  sursa <- remove_unit(sursa)
  sursa <- add_cls_marime(sursa)
  sursa <- subset(sursa, !is.na(sursa$t_r1) & !is.na(sursa$t_r2) & !is.na(sursa$t_r3) & 
                         !is.na(sursa$t_r4) & !is.na(sursa$t_r5) & !is.na(sursa$t_r6) & 
                         !is.na(sursa$t_r7) & !is.na(sursa$t_r8) & !is.na(sursa$t_r9) & 
                         !is.na(sursa$t_r10) & !is.na(sursa$t_r11) & !is.na(sursa$t_r12))
  sursa <- add_div_caen(sursa)
  sursa$div <- as.factor(sursa$div)
  lv <- levels(sursa$div)
  for (i in 1:12) {
    col1 = paste0("Metoda2_",i)
    col2 = paste0("t_r",i)
    col3 = paste0(ifelse(i <= 9, "ca_0", "ca_"), i)
    out <- 0
    for (j in 1:4) {
      for (t in lv) {
        l <- length(sursa[which(sursa$cls_marime == j & sursa$div == t), col3])
        if (l == 0) next
        outliersDetection = boxB(sursa[which(sursa$cls_marime == j & sursa$div == t), col3], k = 20, method = "asymmetric")
        out <- c(out, outliersDetection$outliers)
      }
    }
    sursa[,col1] <- 0
    out <- out[-c(1)]
    sursa[out, col1] <- rep(1,length(out))
  }
  
  p = colSums(sursa %>% select(Metoda2_1:Metoda2_12), na.rm = TRUE)
  
  # Calcularea procentului 
  procent = vector("numeric", 12L)
  for (i in 1:12){
    col = paste0("t_r", i)
    procent[i] = paste0(round(p[[i]]*100/sum(sursa[,col], na.rm = TRUE),2), " % ")
  }
  
  luna = c("ianuarie", "februarie", "martie", "aprilie", "mai", "iunie", "iulie",
           "august", "septembrie", "octombrie", "noiembrie", "decembrie")
  procent = data.frame(luna, procent)
  return(sursa)
}


s2018 <- out(sursa2018)

procent <- out(sursa2015)

write.dbf(s2018, "sursa_2015_met2.dbf")




############################################### # IMPUTARE ############################################################
#######################################################################################################################

# Boosting
vatData <- s2018
train = sample(1:nrow(vatData), round(0.1 * nrow(vatData))) #train set

test = vatData[-train,] #test set

# prezicem cifra de afaceri (turnover) in functie de 
# venit si numarul mediu de angajati
# toate 3 coloane: turnover, grossIncome, avgEmployeesNo se gasesc in vatData
# aici antrenam modelul

boosting = gbm(ca_01 ~ r7_t01 + r14_t01, data = vatData[train, ], 
               n.trees=5000, 
               interaction.depth=4)
# aici facem predictia 
predictieBoosting = predict(boosting, newdata = vatData[-train, ],
                            n.trees =5000) 

#root mean square error: eroarea facand comparatie cu datele reale
mseBoosting = sqrt(mean((predictieBoosting - test[,"ca_01"])^2))

df = data.frame(org=test[,"ca_01"], pred=predictieBoosting)
df$dif <- abs(df$pred-df$org)
sum(df$dif)/sum(df$org)*100


mean_size_div <- function (sursa,met = "mean") {
  sursa <- clean(sursa)
  sursa <- remove_unit(sursa)
  sursa <- add_cls_marime(sursa)
  sursa <- subset(sursa, !is.na(sursa$t_r1) & !is.na(sursa$t_r2) & !is.na(sursa$t_r3) & 
                    !is.na(sursa$t_r4) & !is.na(sursa$t_r5) & !is.na(sursa$t_r6) & 
                    !is.na(sursa$t_r7) & !is.na(sursa$t_r8) & !is.na(sursa$t_r9) & 
                    !is.na(sursa$t_r10) & !is.na(sursa$t_r11) & !is.na(sursa$t_r12))
  sursa <- add_div_caen(sursa)
  sursa <- subset(sursa, !is.na(sursa$div))
  sursa <- subset(sursa, !is.na(sursa$cls_marime))
  sursa$div <- as.factor(sursa$div)
  lv <- levels(sursa$div)
  cls <- 0
  div <- 0
  media <- 0
  ratio <- 0
  for (i in 1:4) {
    for (j in lv) {
      l <- length(sursa[which(sursa$cls_marime == i & sursa$div == j), "ca_03"])
      if (l == 0) next
      if (met == "ratio") {
        cur <- sum(sursa[which(sursa$cls_marime == i & sursa$div == j), "ca_03"], na.rm = TRUE)
        prev <- sum(sursa[which(sursa$cls_marime == i & sursa$div == j), "ca_02"], na.rm = TRUE)
        ratio <- c(ratio, cur/prev)
      }
      if (met == "ratio1") {
        cur <- sum(sursa[which(sursa$cls_marime == i & sursa$div == j), "ca_03"], na.rm = TRUE)
        prev <- sum(year[which(year$cls_marime == i & year$div == j), "ca_03"], na.rm = TRUE)
        ratio <- c(ratio, cur/prev)
      }
      med <- median(sursa[which(sursa$cls_marime == i & sursa$div == j), "ca_03"], na.rm = TRUE)
      media  <- c(media, med)
      div <- c(div, j)
      cls <- c(cls, i)
    }
  }
  if (met == "ratio" | met == "ratio1") {
    df <- data.frame(cls, div, ratio)
    df <- df[-c(1),]
    return(df)
  }
  df <- data.frame(cls, div, media)
  df <- df[-c(1),]
  return(df)
}


clean <- function (sursa) {
  sursa <- clean(sursa)
  sursa <- remove_unit(sursa)
  sursa <- add_cls_marime(sursa)
  sursa <- subset(sursa, !is.na(sursa$t_r1) & !is.na(sursa$t_r2) & !is.na(sursa$t_r3) & 
                    !is.na(sursa$t_r4) & !is.na(sursa$t_r5) & !is.na(sursa$t_r6) & 
                    !is.na(sursa$t_r7) & !is.na(sursa$t_r8) & !is.na(sursa$t_r9) & 
                    !is.na(sursa$t_r10) & !is.na(sursa$t_r11) & !is.na(sursa$t_r12))
  sursa <- add_div_caen(sursa)
  sursa <- subset(sursa, !is.na(sursa$div))
  sursa <- subset(sursa, !is.na(sursa$cls_marime))
  sursa$div <- as.factor(sursa$div)
  return (sursa)
}




# 1. Remove Errors
error <- out(sursa2018)
error <- subset(error, error$Metoda2_3!=1)
error <- error[,c(1:179)]
error <- calculeazaEroriUnitateDeMasura(error,sursa2017)
error <- subset(error, error$Metoda0_3!=1)
error <- error[,c(1:179)]
error <- calculeazaMetodaComparatie(error,sursa2017)
error <- subset(error, error$Metoda3_3!=1)
error <- error[,c(1:179)]

t = Sys.time()
v <- 0
for (i in 1:200) {
  # 2. Train
  pos_train = sample(1:nrow(error), round(nrow(error)*0.1)) 
  train = error[-pos_train,]
  test = error[pos_train,]
  
  # 3. Media
  med <- mean_size_div(train)
  
  # 4.Pred
  test1 <- left_join(test, med, by = c("div"="div", "cls_marime" = "cls"))
  
  # Eval
  rie <- sum(abs(test1$media-test1$ca_03), na.rm = TRUE)/sum(test1$ca_03, na.rm = TRUE)
  v <- c(v,rie)
}

Sys.time() - t



# Ratio
t = Sys.time()
vr <- 0
for (i in 1:200) {
  # 2. Train
  pos_train = sample(1:nrow(error), round(nrow(error)*0.1)) 
  train = error[-pos_train,]
  test = error[pos_train,]
  rat <- mean_size_div(train, met = "ratio")
  test1 <- left_join(test, rat, by = c("div"="div", "cls_marime" = "cls"))
  test1$ratio <- test1$ca_02*test1$ratio
  rie <- sum(abs(test1$ratio-test1$ca_03), na.rm = TRUE)/sum(test1$ca_03, na.rm = TRUE)
  vr <- c(vr, rie)
}
Sys.time() - t




pos_train = sample(1:nrow(error), round(nrow(error)*0.1)) 
train = error[-pos_train,]
test = error[pos_train,]
rat <- mean_size_div(train, met = "ratio")
test1 <- left_join(test, rat, by = c("div"="div", "cls_marime" = "cls"))
test1$ratio <- test1$ca_02*test1$ratio
rie <- sum(abs(test1$ratio-test1$ca_03), na.rm = TRUE)/sum(test1$ca_03, na.rm = TRUE)








# Boosting
t = Sys.time()
mse <- 0
r <- 0
for (i in 1:2) {
  pos_train = sample(1:nrow(error), round(nrow(error)*0.1)) 
  train = error[-pos_train,]
  test = error[pos_train,]
  boosting = gbm(ca_03 ~ r7_t03 + r14_t03, data = train, 
                 n.trees=5000, 
                 interaction.depth=4)
  predictieBoosting = predict(boosting, newdata = test,
                              n.trees =5000) 
  mseBoosting = sqrt(mean((predictieBoosting - test[,"ca_03"])^2))
  mse <- c(mse, mseBoosting)
  rie <- sum(abs(predictieBoosting-test$ca_03), na.rm = TRUE)/sum(test$ca_03, na.rm = TRUE)
  r <- c(r, rie)
}
Sys.time() - t
