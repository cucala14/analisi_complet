setwd("~/uni/tfg")
library(readxl)
library(dplyr)
library(tidyverse)
library(webshot)
library(suncalc)
library(lutz)
library(survival)
library(car)
library(lmtest)


Shark_Incident_Database <- read_excel("Australian Shark-Incident Database Public Version.xlsx")
colSums(is.na(Shark_Incident_Database))
head(Shark_Incident_Database)
data<-Shark_Incident_Database

data$Victim.activityN <- ifelse(data$Victim.activity == "unmotorised boating" | data$Victim.activity == "motorised boating", "boating", 
                                ifelse(data$Victim.activity == "spearfishing", "fishing", 
                                       ifelse(data$Victim.activity == "surfing", "boarding", 
                                              ifelse(data$Victim.activity == "scuba diving", "diving", 
                                                     ifelse(data$Victim.activity == "standing", "other: standing in water", 
                                                            ifelse(data$Victim.activity == "swimming" | data$Victim.activity == "snorkelling" | data$Victim.activity == "other: floating" | data$Victim.activity == "wading", "swimming",
                                                                   data$Victim.activity))))))
data$Victim.activityN[data$Victim.activityN=="other: standing in water"]<-"other"
data$Victim.activityN[data$Victim.activityN=="other: hull scraping"]<-"other"
data$Victim.activityN[data$Victim.activityN=="other: jetskiing; swimming"]<-"other"

data$Recovery.status2 <- ifelse(data$Victim.injury == "Injured", "injured", data$Victim.injury)
data$Recovery.status2[data$Recovery.status2=="injury"]<-"injured"

data$Shark.common.name <- ifelse(data$Shark.common.name == "tiger shark ", "tiger shark", data$Shark.common.name)
data$Shark.common.name <- ifelse(data$Shark.common.name == "white shark ", "white shark", data$Shark.common.name)

data$Site.category[data$Site.category=="Coastal"]<-"coastal"
data$Site.category[data$Site.category=="Ocean/pelagic"]<-"ocean"
data$Site.category[data$Site.category=="other: fish farm"]<-"river"
data$Site.category[data$Site.category=="island open ocean"]<-"ocean"


data$fatal<-ifelse(data$Recovery.status2=="fatal",1,0)
data$Estacion_año <- ifelse(data$Incident.month >= 12 | data$Incident.month < 3, "Verano",
                       ifelse(data$Incident.month >= 3 & data$Incident.month < 6, "Otoño",
                              ifelse(data$Incident.month >= 6 & data$Incident.month < 9, "Invierno", 
                                     ifelse(data$Incident.month >= 9 & data$Incident.month < 12, "Primavera", NA))))

data<-as_tibble(data)
data <- dplyr::select(data, UIN, Incident.month, Incident.year, Recovery.status2, 
               State, Site.category, Shark.common.name, `Provoked/unprovoked`, 
               Victim.activityN, Injury.location, Victim.gender, Data.source, Reference,
               fatal, Estacion_año)
data <- data %>%
  mutate(UIN = row_number())
data$Incident.month <- trimws(data$Incident.month)
id<-seq(1,length(dat2$Incident.day),1)

#tenim incident.day fins 22/03/2022 que és fila 1196
dat2 <- read.delim2(file="timedb2.txt", header=T, sep="\t")
data<-data[1:1196,]
dat2<-dat2[1:1196,]
data<-cbind(data, dat2$Incident.day)
data<- dplyr::mutate(data, Incident.day=data$`dat2$Incident.day`, Provoked.unprovoked=`Provoked/unprovoked`)
data <- dplyr::select(data, UIN, Incident.day, Incident.month, Incident.year, Recovery.status2, 
                      State, Site.category, Shark.common.name, `Provoked/unprovoked`, 
                      Victim.activityN, Victim.gender,
                      fatal, Estacion_año)
data <- data %>%
  mutate(Incident.date = as.Date(paste(Incident.year, Incident.month, Incident.day, sep = "-"))) %>%
  arrange(Incident.date) 


data$injured<-ifelse(data$Recovery.status2=="injured",1,0)
data$costa<-ifelse(data$Site.category=="coastal",1,0)
data$swim_yes<-ifelse(data$Victim.activityN!="swimming",0,1)

data<-data %>% mutate(data, Incident.month=as.numeric(Incident.month), 
                      Recovery.status2=as.factor(Recovery.status2), State=as.factor(State), 
                      Site.category=as.factor(Site.category), Shark.common.name= as.factor(Shark.common.name),
                      `Provoked/unprovoked`=as.factor(`Provoked/unprovoked`), Victim.activityN=as.factor(Victim.activityN),
                      Victim.gender=as.factor(Victim.gender),
                      fatal=as.factor(fatal), Estacion_año=as.factor(Estacion_año), injured=as.factor(injured),
                      costa=as.factor(costa), swim_yes=as.factor(swim_yes))
data$shark.common.name <- ifelse(data$Shark.common.name=="wobbegoong", data$Shark.common.name, "other")
data$shark.common.name <- ifelse(data$Shark.common.name=="bull shark", data$Shark.common.name, "other")
data$shark.common.name <- ifelse(data$Shark.common.name=="tiger shark", data$Shark.common.name, "other")
data$shark.common.name <- ifelse(data$Shark.common.name=="white shark", data$Shark.common.name, "other")

data$Victim.activityN<-ifelse(is.na(data$Victim.activityN), "other", data$Victim.activityN)

data <- data %>%
  mutate(
    State = as.factor(State),
    Site.category = as.factor(Site.category),
    Shark.common.name = as.factor(Shark.common.name),
    `Provoked/unprovoked` = as.factor(`Provoked/unprovoked`),
    Victim.activityN = as.factor(Victim.activityN),
    Victim.gender = as.factor(Victim.gender),
    Estacion_año = as.factor(Estacion_año),
    shark.common.name = as.factor(shark.common.name)
  ) 

data$`Provoked/unprovoked` <-ifelse(data$`Provoked/unprovoked`=="provoked", 1, 0)
data$Victim.gender <-ifelse(data$Victim.gender=="male", 0, 1)

ggplot(data, aes(x = Incident.year)) +
  geom_bar(fill = "red", color = "black", alpha = 0.5) +
  labs(title = "Distribució dels Incidents per Any", x = "Any", y = "Freqüència") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(data,Recovery.status2=="fatal"), aes(x = Incident.year)) +
  geom_bar(fill = "red", color = "black", alpha = 1) +
  labs(title = "Distribució dels Incidents fatals per Any", x = "Any", y = "Freqüència") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(data,State=="NSW"), aes(x = Incident.year)) +
  geom_bar(fill = "red", color = "black", alpha = 1) +
  labs(title = "Distribució dels Incidents per Any", x = "Any", y = "Freqüència") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(data,State=="QLD"), aes(x = Incident.year)) +
  geom_bar(fill = "red", color = "black", alpha = 1) +
  labs(title = "Distribució dels Incidents per Any", x = "Any", y = "Freqüència") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(data,Site.category=="coastal"), aes(x = Incident.year)) +
  geom_bar(fill = "red", color = "black", alpha = 1) +
  labs(title = "Distribució dels Incidents per Any", x = "Any", y = "Freqüència") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(data,Victim.activityN==6), aes(x = Incident.year)) +
  geom_bar(fill = "red", color = "black", alpha = 1) +
  labs(title = "Distribució dels Incidents per Any", x = "Any", y = "Freqüència") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

ggplot(data, aes(x = State)) +
  geom_bar(fill = c("darkblue","white","red","white","red","white","darkblue"), color = "black", alpha = 0.5) +
  labs(title = "Distribució dels Incidents per Estat",
       x = "Estat",
       y = "Freqüència") +
  theme_minimal() +   theme(plot.title = element_text(hjust = 0.5))

barplot(table(data$Site.category))
barplot(table(data$Recovery.status2))
barplot(table(data$State))
barplot(table(data$Victim.activityN))



###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################



model <- glm(fatal ~ injured+Incident.year+Victim.activityN+Site.category+State+Victim.gender 
             + Estacion_año+Incident.date, data = data, family = "binomial")

drop1(model, test = "Chisq")

step(model,method="both")

model1<-glm(formula = fatal~injured + Incident.year + Victim.activityN + State + 
                  Victim.gender+State , family = "binomial", 
                data = data)

drop1(model1, test = "Chisq")
summary(model1) 
exp(coef(model1))

residus_st1 <- rstudent(model1) 
plot(residus_st1, ylab = "Residus estandaritzats", xlab = "Observacions", main = "Gràfic dels Residus Estandarizats")
shapiro.test(residuals)# < 0.05

plot(model1,1)
plot(model1,2)
plot(model1,3)
plot(model1,4)

bptest(model1,studentize = FALSE)# < 2.2e-16
Box.test(residus_st1, lag = 20, type = "Ljung-Box")# = 0.8767

plot(predict(model1, type = "link"), model1$residuals, xlab = "Valors ajustats (log-odds)", ylab = "Residuals", main = "Gràfic de linealitat")

dwtest(model1)# = 0.7165

car::vif(model1)

model2<-glm(formula = fatal ~ injured + State +Victim.activityN +Incident.year, family = "binomial",
            data = data)

step(model2,method="both")
summary(model2)
drop1(model2, test = "Chisq")

plot(model2,1)
plot(model2,2)
plot(model2,3)
plot(model2,4)

residus_std2 <- rstandard(model2)
plot(residus_std2, ylab = "Residus estandaritzats", xlab = "Observacions", main = "Gràfic de Residus Estandaritzats")

dwtest(model2)# = 0.7191
Box.test(residus_std2, lag = 20, type = "Ljung-Box")  = 0.8525

plot(predict(model2, type = "link"), model2$residuals, xlab = "Valors ajustats (log-odds)", ylab = "Residuals", main = "Gràfic de linealitat")

round(car::vif(model2),4)

outlierTest(model2)
bptest(model2)
shapiro.test(residus_std2)
ks.test(residus_std2, "pnorm")  

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

modelo_weibull2.1 <- flexsurvreg(surv_obj2 ~ injured, data = victima_ctivity2.data, dist = "weibull")

residuos_2.1 <- residuals(modelo_weibull2.1)
shapiro.test(residuos_2.1)  
ks.test(residuos_2.1, "pnorm")  

modelo_weibull2.2 <- flexsurvreg(surv_obj2 ~ Incident.date, data = victima_ctivity2.data, dist = "weibull")
residuos_2.2 <- residuals(modelo_weibull2.2)
shapiro.test(residuos_2.2)  
ks.test(residuos_2.2, "pnorm")  

modelo_weibull2.3 <- flexsurvreg(surv_obj2 ~ Victim.gender, data = victima_ctivity2.data, dist = "weibull")
residuos_2.3 <- residuals(modelo_weibull2.3)
shapiro.test(residuos_2.3)  
ks.test(residuos_2.3, "pnorm")  

modelo_weibull3 <- flexsurvreg(surv_obj3 ~ Victim.gender, data = victima_ctivity3.data, dist = "weibull")
residuos_3 <- residuals(modelo_weibull3)
shapiro.test(residuos_3)  
ks.test(residuos_3, "pnorm")  

modelo_weibull3.2 <- flexsurvreg(surv_obj3 ~ Incident.date, data = victima_ctivity3.data, dist = "weibull")
residuos_3.2 <- residuals(modelo_weibull3.2)
shapiro.test(residuos_3.2)  
ks.test(residuos_3.2, "pnorm")  

modelo_weibull4 <- flexsurvreg(surv_obj4 ~ injured, data = victima_ctivity4.data, dist = "weibull")
residuos_4 <- residuals(modelo_weibull4)
shapiro.test(residuos_4)  
ks.test(residuos_4, "pnorm")  

modelo_weibull4.1 <- flexsurvreg(surv_obj4 ~ Incident.date, data = victima_ctivity4.data, dist = "weibull")
residuos_4.1 <- residuals(modelo_weibull4.1)
shapiro.test(residuos_4.1)  
ks.test(residuos_4.1, "pnorm")  

modelo_weibull4.2 <- flexsurvreg(surv_obj4 ~ Incident.date, data = victima_ctivity4.data, dist = "weibull")
residuos_4.2 <- residuals(modelo_weibull4.2)
shapiro.test(residuos_4.2)  
ks.test(residuos_4.2, "pnorm")  

#####quitamos otra vez victim gender porque solo hay un hombre

######hay pocas de una classe de injured entonces no buscamos
modelo_weibull5.1 <- flexsurvreg(surv_obj5 ~ Incident.date, data = victima_ctivity5.data, dist = "weibull")
residuos_5.1 <- residuals(modelo_weibull5.1)
shapiro.test(residuos_5.1)  
ks.test(residuos_5.1, "pnorm")  

modelo_weibull_noninjured <- flexsurvreg(surv_obj_noninjured ~ Victim.gender + Victim.activityN, data = noninjured.data,dist="weibull")
residuos_11 <- residuals(modelo_weibull_noninjured)
shapiro.test(residuos_11)  
ks.test(residuos_11, "pnorm")  

modelo_weibull_noninjured.1 <- flexsurvreg(surv_obj_noninjured ~ Victim.gender, data = noninjured.data,dist="weibull")
residuos_11.1 <- residuals(modelo_weibull_noninjured.1)
shapiro.test(residuos_11.1)  
ks.test(residuos_11.1, "pnorm")  

modelo_weibull_noninjured.2 <- flexsurvreg(surv_obj_noninjured ~ Victim.activityN, data = noninjured.data,dist="weibull")
residuos_11.2 <- residuals(modelo_weibull_noninjured.2)
shapiro.test(residuos_11.2)  
ks.test(residuos_11.2, "pnorm")  

modelo_weibull_ultim20 <- flexsurvreg(surv_obj_ultim20 ~  Victim.gender + injured+Victim.activityN, data = ultim20.data,dist="weibull")
residuos_13 <- residuals(modelo_weibull_ultim20)
shapiro.test(residuos_13)  
ks.test(residuos_13, "pnorm")  

modelo_weibull_ultim20.1 <- flexsurvreg(surv_obj_ultim20 ~  injured, data = ultim20.data,dist="weibull")
residuos_13.1 <- residuals(modelo_weibull_ultim20.1)
shapiro.test(residuos_13.1)  
ks.test(residuos_13, "pnorm")  

modelo_weibull_ultim20.2 <- flexsurvreg(surv_obj_ultim20 ~  Victim.gender, data = ultim20.data,dist="weibull")
residuos_13.2 <- residuals(modelo_weibull_ultim20.2)
shapiro.test(residuos_13.2)  
ks.test(residuos_13.2, "pnorm")  

supervivencia_1 <- Surv(time = as.numeric(tiempo_transcurrido1), event = as.numeric(victima_ctivity1.data$fatal))
grupos_gender1 <- victima_ctivity1.data$Victim.gender
test_logrank_gender1 <- survdiff(supervivencia_1 ~ grupos_gender1)
grupos_inj1 <- victima_ctivity1.data$injured

test_logrank_inj1 <- survdiff(supervivencia_1 ~ grupos_inj1, rho = 0)

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################


residuos_gen2 <- residuals(modelo_weibull2, type = "response")
supervivencia_2 <- Surv(time = as.numeric(tiempo_transcurrido2), event = as.numeric(victima_ctivity2.data$fatal))
grupos_gender2<- victima_ctivity2.data$Victim.gender
test_logrank_gender2 <- survdiff(supervivencia_2 ~ grupos_gender2, rho = 0)
grupos_inj2 <- victima_ctivity2.data$injured
test_logrank_inj2 <- survdiff(supervivencia_2 ~ grupos_inj2, rho = 0)

residuos_gen3 <- residuals(modelo_weibull3, type = "response")
supervivencia_3 <- Surv(time = as.numeric(tiempo_transcurrido3), event = as.numeric(victima_ctivity3.data$fatal))
grupos_gender3 <- victima_ctivity3.data$Victim.gender
test_logrank_gender3 <- survdiff(supervivencia_3 ~ grupos_gender3, rho = 0)
grupos_inj3 <- victima_ctivity3.data$injured


########################################
test_logrank_inj3 <- survdiff(supervivencia_3 ~ grupos_inj3, rho = 0)
########################################


residuos_gen4 <- residuals(modelo_weibull4, type = "response")
supervivencia_4 <- Surv(time = as.numeric(tiempo_transcurrido4), event = as.numeric(victima_ctivity4.data$fatal))
grupos_gender4 <- victima_ctivity4.data$Victim.gender #todo 1
test_logrank_gender4 <- survdiff(supervivencia_4 ~ grupos_gender4, rho = 0)
grupos_inj4 <- victima_ctivity4.data$injured

#######################
test_logrank_inj4 <- survdiff(supervivencia_4 ~ grupos_inj4, rho = 0)
########################

supervivencia_5 <- Surv(time = as.numeric(tiempo_transcurrido5), event = as.numeric(victima_ctivity5.data$fatal))
grupos_gender5 <- victima_ctivity5.data$Victim.gender
test_logrank_gender5 <- survdiff(supervivencia_5 ~ grupos_gender5, rho = 0)
grupos_inj5 <- victima_ctivity5.data$injured
test_logrank_inj5 <- survdiff(supervivencia_5 ~ grupos_inj5, rho = 0)

supervivencia_6 <- Surv(time = as.numeric(tiempo_transcurrido6), event = as.numeric(victima_ctivity6.data$fatal))
grupos_gender6 <- victima_ctivity6.data$Victim.gender

#@#####################
test_logrank_gender6 <- survdiff(supervivencia_6 ~ grupos_gender6, rho = 0)
########################

grupos_inj6 <- victima_ctivity6.data$injured

#######################
test_logrank_inj6 <- survdiff(supervivencia_6 ~ grupos_inj6, rho = 0)
#######################

supervivencia_7 <- Surv(time = as.numeric(tiempo_transcurrido7), event = as.numeric(victima_ctivityother.data$fatal))
grupos_inj7 <- victima_ctivityother.data$injured
test_logrank_inj7 <- survdiff(supervivencia_7 ~ grupos_inj7, rho = 0)

supervivencia_11 <- Surv(time = as.numeric(tiempo_transcurrido11), event = as.numeric(injured.data$fatal))
grupos_gender11 <- injured.data$Victim.gender

######################
test_logrank_gender11 <- survdiff(supervivencia_11 ~ grupos_gender11, rho = 0)
####################

grupos_activity11<-injured.data$Victim.activityN
######################
test_logrank_gender11 <- survdiff(supervivencia_11 ~ grupos_activity11, rho = 0)
####################
lol(supervivencia_11,grupos_gender11,grupos_activity11)

residuos_gen12 <- residuals(modelo_weibull_noninjured, type = "response")
supervivencia_12 <- Surv(time = as.numeric(tiempo_transcurrido12), event = as.numeric(noninjured.data$fatal))
grupos_gender12 <- noninjured.data$Victim.gender
test_logrank_gender12 <- survdiff(supervivencia_12 ~ grupos_gender12, rho = 0)
grupos_activity12<-noninjured.data$Victim.activityN
######################
test_logrank_gender12 <- survdiff(supervivencia_12 ~ grupos_activity12, rho = 0)
####################3
lol(supervivencia_12,grupos_gender12,grupos_activity12)

supervivencia_13 <- Surv(time = as.numeric(tiempo_transcurrido13), event = as.numeric(dateverano_.data$fatal))
grupos_gender13 <- dateverano_.data$Victim.gender

#########################3
test_logrank_gender13 <- survdiff(supervivencia_13 ~ grupos_gender13, rho = 0)
#########################

grupos_inj13 <- dateverano_.data$injured
####################################33
test_logrank_inj13 <- survdiff(supervivencia_13 ~ grupos_inj13, rho = 0)
######################################

supervivencia_14 <- Surv(time = as.numeric(tiempo_transcurrido14), event = as.numeric(ultim20.data$fatal))
grupos_gender14 <- ultim20.data$Victim.gender

test_logrank_gender14 <- survdiff(supervivencia_14 ~ grupos_gender14, rho = 0)

grupos_inj14 <- ultim20.data$injured
####################################33
test_logrank_inj14 <- survdiff(supervivencia_14 ~ grupos_inj14, rho = 0)
######################################

supervivencia_15 <- Surv(time = as.numeric(tiempo_transcurrido15), event = as.numeric(aus_depresion.data$fatal))
grupos_gender15 <- aus_depresion.data$Victim.gender

####################################
test_logrank_gender15 <- survdiff(supervivencia_15 ~ grupos_gender15, rho = 0)
###############################33

grupos_inj15 <- aus_depresion.data$injured

####################################33
test_logrank_inj15 <- survdiff(supervivencia_15~ grupos_inj15, rho = 0)
######################################

supervivencia_16 <- Surv(time = as.numeric(tiempo_transcurrido16), event = as.numeric(victima_ctivity6_inj1.data$fatal))
grupos_gender16<- victima_ctivity6_inj1.data$Victim.gender
test_logrank_gender16 <- survdiff(supervivencia_16 ~ grupos_gender16, rho = 0)
plots_logrank<-function(supervivencia,grupos_inj,grupos_gender, nombre){
  test_logrank_inj1.1 <- survdiff(supervivencia ~ grupos_inj+grupos_gender, rho = 0)
  print(test_logrank_inj1.1)
  plot(survfit(supervivencia ~ as.factor(grupos_inj)+as.factor(grupos_gender)), col = c("blue","red","green","darkgreen") ,main = nombre, xlab = "Time", ylab = "Survival Probability")
  legend("bottomleft", legend = c("Inj=0,G=0", "Inj=0, G=1", "Inj=1, G=0", "Inj=1, G=1"), col = c("blue", "red", "green","darkgreen"), lwd = 1, cex = 0.8)
  }
par(mfrow=c(2,4))
plots_logrank(supervivencia_1,grupos_inj1,grupos_gender1, "Boating")
plots_logrank(supervivencia_3,grupos_inj3,grupos_gender3, "Diving")
plots_logrank(supervivencia_4,grupos_inj4,grupos_gender4, "Fishing")
plots_logrank(supervivencia_5,grupos_inj5,grupos_gender5, "other")
plots_logrank(supervivencia_6,grupos_inj6,grupos_gender6, "swimming")
plots_logrank(supervivencia_7,grupos_inj7,grupos_gender7, "unknown")
plots_logrank(supervivencia_14,grupos_inj14,grupos_gender14, "ultims20")
plots_logrank(supervivencia_15,grupos_inj15,grupos_gender15, "aus_depression")

par(mfrow=c(2,3))
plot(survfit(supervivencia_1 ~ as.factor(grupos_inj1)), col = c("blue","red","green") ,main = "Test Log-rank - Boarding and Injured", xlab = "Time", ylab = "Survival Probability")
legend("bottomleft", legend = levels(as.factor(grupos_inj1)), col = c("blue","red","green"), lwd = 1, cex = 0.8)

plot(survfit(supervivencia_3 ~ as.factor(grupos_inj3)), col = c("blue","red","green") ,main = "Test Log-rank - Diving and Injured", xlab = "Time", ylab = "Survival Probability")
legend("bottomleft", legend = levels(as.factor(grupos_inj3)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_inj3))

plot(survfit(supervivencia_4 ~ as.factor(grupos_inj4)), col = c("blue","red","green") ,main = "Test Log-rank - Fishing and Injured", xlab = "Time", ylab = "Survival Probability")
legend("bottomleft", legend = levels(as.factor(grupos_inj4)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_inj4))

plot(survfit(supervivencia_11 ~ as.factor(grupos_gender11)), col = c("blue","red","green") ,main = "Test Log-rank - Injured and Gender", xlab = "Time", ylab = "Survival Probability")
legend("bottomleft", legend = levels(as.factor(grupos_gender11)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender11))

plot(survfit(supervivencia_14 ~ as.factor(grupos_inj14)), col = c("blue","red","green") ,main = "Test Log-rank - Ultim20 and Injured", xlab = "Time", ylab = "Survival Probability")
legend("bottomleft", legend = levels(as.factor(grupos_inj14)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_inj14))

plot(survfit(supervivencia_15 ~ as.factor(grupos_inj15)), col = c("blue","red","green") ,main = "Test Log-rank - Aus_depress and Injured", xlab = "Time", ylab = "Survival Probability")
legend("bottomleft", legend = levels(as.factor(grupos_inj15)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_inj15))

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

plot(survfit(supervivencia_1 ~ as.factor(grupos_gender1)), col = c("blue","red","green") ,main = "Test Log-rank - Gender 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomleft", legend = levels(as.factor(grupos_gender1)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender1))

plot(survfit(supervivencia_2 ~ as.factor(grupos_gender2)), col = c("blue","red","green") ,main = "Test Log-rank - Gender 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomleft", legend = levels(as.factor(grupos_gender2)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender2))

plot(survfit(supervivencia_3 ~ as.factor(grupos_gender3)), col = c("blue","red","green") ,main = "Test Log-rank - Gender 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_gender3)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender3))

plot(survfit(supervivencia_4 ~ as.factor(grupos_gender4)), col = c("blue","red","green") ,main = "Test Log-rank - Gender 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_gender4)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender4))

plot(survfit(supervivencia_5 ~ as.factor(grupos_gender5)), col = c("blue","red","green") ,main = "Test Log-rank - Gender 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_gender5)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender4))


plot(survfit(supervivencia_6 ~ as.factor(grupos_gender6)), col = c("blue","red","green") ,main = "Test Log-rank - Gender 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_gender6)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender6))

plot(survfit(supervivencia_7 ~ as.factor(grupos_gender7)), col = c("blue","red","green") ,main = "Test Log-rank - Gender 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_gender7)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender7))

plot(survfit(supervivencia_8 ~ as.factor(grupos_gender8)), col = c("blue","red","green") ,main = "Test Log-rank - Gender 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_gender8)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender8))

plot(survfit(supervivencia_9 ~ as.factor(grupos_gender9)), col = c("blue","red","green") ,main = "Test Log-rank - Gender 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_gender9)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender9))

plot(survfit(supervivencia_10 ~ as.factor(grupos_gender10)), col = c("blue","red","green") ,main = "Test Log-rank - Gender 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_gender10)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender10))



plot(survfit(supervivencia_12 ~ as.factor(grupos_gender12)), col = c("blue","red","green") ,main = "Test Log-rank - Gender 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_gender12)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender12))

plot(survfit(supervivencia_13 ~ as.factor(grupos_gender13)), col = c("blue","red","green") ,main = "Test Log-rank - Gender 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_gender13)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender13))

plot(survfit(supervivencia_14 ~ as.factor(grupos_gender14)), col = c("blue","red","green") ,main = "Test Log-rank - Gender 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_gender14)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender14))

plot(survfit(supervivencia_15 ~ as.factor(grupos_gender15)), col = c("blue","red","green") ,main = "Test Log-rank - Gender 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_gender15)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender15))

plot(survfit(supervivencia_16 ~ as.factor(grupos_gender16)), col = c("blue","red","green") ,main = "Test Log-rank - Gender 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomleft", legend = levels(as.factor(grupos_gender6)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_gender1))

#####injured


plot(survfit(supervivencia_2 ~ as.factor(grupos_inj2)), col = c("blue","red","green") ,main = "Test Log-rank - inj 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomleft", legend = levels(as.factor(grupos_inj2)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_inj2))

plot(survfit(supervivencia_5 ~ as.factor(grupos_inj5)), col = c("blue","red","green") ,main = "Test Log-rank - inj 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_inj5)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_inj4))


plot(survfit(supervivencia_6 ~ as.factor(grupos_inj6)), col = c("blue","red","green") ,main = "Test Log-rank - inj 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_inj6)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_inj6))

plot(survfit(supervivencia_7 ~ as.factor(grupos_inj7)), col = c("blue","red","green") ,main = "Test Log-rank - inj 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_inj7)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_inj7))

plot(survfit(supervivencia_8 ~ as.factor(grupos_inj8)), col = c("blue","red","green") ,main = "Test Log-rank - inj 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_inj8)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_inj8))

plot(survfit(supervivencia_9 ~ as.factor(grupos_inj9)), col = c("blue","red","green") ,main = "Test Log-rank - inj 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_inj9)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_inj9))

plot(survfit(supervivencia_10 ~ as.factor(grupos_inj10)), col = c("blue","red","green") ,main = "Test Log-rank - inj 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_inj10)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_inj10))

plot(survfit(supervivencia_11 ~ as.factor(grupos_inj11)), col = c("blue","red","green") ,main = "Test Log-rank - inj 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_inj11)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_inj11))

plot(survfit(supervivencia_12 ~ as.factor(grupos_inj12)), col = c("blue","red","green") ,main = "Test Log-rank - inj 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_inj12)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_inj12))

plot(survfit(supervivencia_13 ~ as.factor(grupos_inj13)), col = c("blue","red","green") ,main = "Test Log-rank - inj 1", xlab = "Time", ylab = "Survival Probability")
legend("bottomright", legend = levels(as.factor(grupos_inj13)), col = c("blue","red","green"), lwd = 1, cex = 0.8)
levels(as.factor(grupos_inj13))

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################


fecha_referencia<-as.Date("1900-01-01")
bites <- subset(data, Incident.date >= start_date)

bites.yr <- as.numeric(table(bites$Incident.year))
yr.lab <- as.numeric(attr(table(bites$Incident.year), "names"))

# add 2 missing years to time series (1908 & 1970)
fbites.yr <- c(bites.yr[1:8],0,bites.yr[9:69],0,bites.yr[70:118])
library(zoo)
library(readr)

# Especificar la URL del archivo CSV en GitHub
url <- "https://raw.githubusercontent.com/cjabradshaw/sharkbite/master/auspop.csv"

# Leer el archivo CSV desde la URL
pob_aus <- read_csv(url)

plot(pob_aus$year, pob_aus$AUS, type = "l", lwd = 2, xlab = "Any", ylab = "Població",
     xlim = c(1900, 2022), main = "Índex d'habitants d'Austràlia 1900-2022",
     col = "blue", bg = "lightblue", pch = 20, cex = 1.2)


pob_aus1900 <- subset(pob_aus, year > 1899)
all.bites.dat <- data.frame(yr.lab,bites.yr)
colnames(all.bites.dat) <- c("year","bites")
all.pop <- pob_aus1900[,c(1,10)]
all.bites.cap <- merge(all.bites.dat, all.pop, by="year")
all.bites.cap$bites.cap <- all.bites.cap[,2] / all.bites.cap[,3]

par(mfrow=c(2,4),pty="m")
plot(all.bites.cap$year, all.bites.cap$bites.cap, type="c", lty=1, lwd=1, col="black",xlab="",ylab="",main="ALL")

data.use <- all.bites.cap
##########################
library(quantmod)

plot.ts(all.bites.cap$bites.cap, main="Tassa mossegades/població (1900-2022)", ylab="Tassa",xlab="Temps", col="darkgreen")
class(all.bites.cap$bites.cap)
acf(all.bites.cap$bites.cap)
pacf(all.bites.cap$bites.cap)#ar(2)?
head(all.bites.cap$bites.cap)
tail(all.bites.cap$bites.cap)
plot(all.bites.cap$bites.cap)
ared=ar(all.bites.cap$bites.cap)

library(tseries)
adf.test(all.bites.cap$bites.cap)#pvalue 0.01, lagorder1

library(TSA)
library(forecast)
l=log(all.bites.cap$bites.cap)
l<-ts(l, frequency = 12)
auto.arima(l,seasonal = TRUE)#ARIMA(0,1,1)  fem la differencia 
eacf(l)#ARIMA(1,1) 
acf(l)
pacf(l)#ar1


ma=arima(l,c(0,1,1),method="ML")
ma2=arima(l,c(1,0,1),method="ML")
ma3=arima(l,c(1,0,0),method="ML")

# Test de Ljung-Box para evaluar la autocorrelación
Box.test(l, lag = 20, type = "Ljung-Box")

pred_ma <- as.numeric(predict(ma)$pred)
pred_ma2 <- as.numeric(predict(ma2)$pred)
pred_ma3 <- as.numeric(predict(ma3)$pred)


mse_ma <- mean((l - pred_ma)^2)
mse_ma2 <- mean((l - pred_ma2)^2)
mse_ma3 <- mean((l - pred_ma3)^2)


print(mse_ma)
print(mse_ma2)
print(mse_ma3)

tsdiag(ma)  #  se ve que el modelo es razonable (los residuos son "ruido")
tsdiag(ma2)
tsdiag(ma3)

checkresiduals(ma)
checkresiduals(ma2)
checkresiduals(ma3)#correlacion residuos

library(lmtest)

acf(ma$residuals,lag.max=20)
acf(ma2$residuals,lag.max=20)

pacf(ma$residuals,lag.max=20)
pacf(ma2$residuals,lag.max=20)


#install.packages("FitAR")
require(FitAR)
qqnorm(ma$residuals)
qqline(ma$residuals)

qqnorm(ma2$residuals)
qqline(ma2$residuals)

aic1 <- AIC(ma)
aic2 <- AIC(ma2)
aic3 <- AIC(ma3)

bic1 <- BIC(ma)
bic2 <- BIC(ma2)
bic3 <- BIC(ma3)

#################3
armle<-ma
par(mfrow=c(2,2))
plot(armle$residuals,ylab="residus",main="Serie temporal del logaritme")
qqnorm(ma$residuals)
qqline(ma$residuals)
acf(armle$residuals, main="ACF")
pacf(armle$residuals,main="PACF")


sigma=sqrt(armle$sigma2)
plot(armle$residuals/sigma,type="p",ylab="residus estandarizados");abline(h=0)

plot(rstandard(armle),ylab='Standardized residuals',type='l')
lines(armle$residuals/sigma,col=2)
qqnorm(armle$residuals);qqline(armle$residuals)
shapiro.test(armle$residuals)#pvalue significativo, residus normales

prediccions <- predict(armle)$pred
residus_prediccions <- l - as.numeric(prediccions)
shapiro.test(residus_prediccions)
wilcox.test(residus_prediccions)
t_resultat <- t.test(residus_prediccions)

checkresiduals(armle$residuals)

Box.test(armle$residuals, lag = 20, type = "Ljung-Box")

plot(armle$residuals, type = "l", main = "Gráfic dels residus")
shapiro.test(armle$residuals)

residuos <- residuals(ma)

library(lmtest)
bptest(residuos ~ log(all.bites.cap$bites.cap))

##fem la difèrencia

l2<-diff(l)
auto.arima(l2)

l2_ts <- ts(l2, frequency = 12)
best_model <- auto.arima(l2, seasonal = TRUE)
eacf(l2)
acf(l2)#
pacf(l2)#

ma4<-arima(l2,c(0,0,1),method="ML")
ma5<-arima(l2,c(1,0,0),method="ML")
ma6<-arima(l2,c(1,0,1),method="ML")

ljung_box <- Box.test(l2, lag = 20, type = "Ljung-Box")


pred_ma4 <- as.numeric(predict(ma4)$pred)
pred_ma5 <- as.numeric(predict(ma5)$pred)
pred_ma6 <- as.numeric(predict(ma6)$pred)


mse_ma4<- mean((l2 - pred_ma4)^2)
mse_ma5 <- mean((l2 - pred_ma5)^2)
mse_ma6 <- mean((l2 - pred_ma6)^2)


print(mse_ma4)
print(mse_ma5)
print(mse_ma6)


################el dos tiene mse menor

tsdiag(ma4)  #  se ve que el modelo es razonable (los residuos son "ruido")
tsdiag(ma5)
tsdiag(ma6)

checkresiduals(ma4)
checkresiduals(ma5)
checkresiduals(ma6)#correlacion residuos

library(lmtest)

acf(ma4$residuals,lag.max=20)
acf(ma6$residuals,lag.max=20)

pacf(ma4$residuals,lag.max=20)
pacf(ma6$residuals,lag.max=20)


#install.packages("FitAR")
require(FitAR)
qqnorm(ma4$residuals)
qqline(ma4$residuals)

qqnorm(ma6$residuals)
qqline(ma6$residuals)

aic1 <- AIC(ma4)
aic2 <- AIC(ma5)
aic3 <- AIC(ma6)

bic1 <- BIC(ma4)
bic2 <- BIC(ma6)
bic3 <- BIC(ma5)

#################
armle<-ma4
plot(armle$residuals)
acf(armle$residuals)
hist(armle$residuals, breaks = "FD", col = "lightblue", main = "Histograma4de Residuos")

acf(armle$residuals)
pacf(armle$residuals)


sigma=sqrt(armle$sigma2)
par(mfrow=c(1,2))
plot(armle$residuals/sigma,type="p",main="Residus estandaritzats");abline(h=0)
hist(armle$residuals, breaks = "FD", col = "lightblue", main = "Histograma de Residus")


lines(armle$residuals/sigma,col=2)
qqnorm(armle$residuals);qqline(armle$residuals)
shapiro.test(ma4$residuals)#pvalue significatiu, residus no normales

Box.test(ma4$residuals, lag = 20, type = "Ljung-Box")

shapiro_test_ma4 <- shapiro.test(ma4$residuals)
shapiro_test_ma4

library(lmtest)

residuos_ma4 <- ma4$residuals

bptest(residuos_ma4 ~ l2)



prediccions <- predict(armle)$pred
residus_prediccions <- l2 - as.numeric(prediccions)
shapiro.test(residus_prediccions)###residuos normales
wilcox.test(residus_prediccions)

Box.test(residus_prediccions, lag = 20, type = "Ljung-Box")


num <- length(l2)
model_aux <- lm(residus_prediccions ~ 1+num)

bptest(modelo_auxiliar)

# Imprimir los resultados
print(bp_test)

