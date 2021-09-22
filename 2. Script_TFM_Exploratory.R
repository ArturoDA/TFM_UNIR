#Exploratory Analysis
#Oswaldo Arturo Díaz Arca
#TFM - Universidad Internacional de la Rioja

rm(list=ls())
setwd("D:/ruta_personal/TFM_UNIR-main")
require(dplyr)
library(Hmisc)
library(skimr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(GGally)

# Import dataset
climxrdto_papa_puno <- read.csv("Resultados/climxrdto_papa_puno.csv")
climxrdto_papa_puno$PROVINCIA[climxrdto_papa_puno$PROVINCIA == "SAN ANTONIO DE PUTINA"] <- "S.A. PUTINA"
climxrdto_papa_puno$PROVINCIA <- as.factor(climxrdto_papa_puno$PROVINCIA)
climxrdto_papa_puno$DISTRITO <- as.factor(climxrdto_papa_puno$DISTRITO)
climxrdto_papa_puno$Campaña <- as.factor(climxrdto_papa_puno$Campaña)

Tmin_camp <- c("Tmin_Oct","Tmin_Nov","Tmin_Dec","Tmin_Jan","Tmin_Feb","Tmin_Mar")
Tmax_camp <- c("Tmax_Oct","Tmax_Nov","Tmax_Dec","Tmax_Jan","Tmax_Feb","Tmax_Mar")
Prec_camp <- c("Prec_Oct","Prec_Nov","Prec_Dec","Prec_Jan","Prec_Feb","Prec_Mar")
GDD_camp <- c("GDD_Oct","GDD_Nov","GDD_Dec","GDD_Jan","GDD_Feb","GDD_Mar")
TR_camp <- c("TR_Oct","TR_Nov","TR_Dec","TR_Jan","TR_Feb","TR_Mar")
Rs_camp <- c("Rs_Oct","Rs_Nov","Rs_Dec","Rs_Jan","Rs_Feb","Rs_Mar")
# Filter columns
dataset <- climxrdto_papa_puno %>% 
  select(Tmin_camp,Tmax_camp,Prec_camp,GDD_camp,TR_camp,Rs_camp,Rdto_kg.ha,Campaña,PROVINCIA,DISTRITO) %>% 
  filter(Rdto_kg.ha != 0)
# Reshape
redim <- reshape(dataset, direction='long', 
          varying=list(c(1:6),c(7:12),c(13:18),c(19:24),c(25:30),c(31:36)),
          timevar='Mes',
          times=c(1,2,3,4,5,6),
          v.names=c('Tmin','Tmax','Prec','GDD','TR','Rs'))
redim <- redim[,-c(1,12)]
glimpse(redim)
# Calculate accumulated
dataset <- dataset %>%
  mutate(Tmax_prom = rowMeans(dataset[,Tmax_camp]),
         Tmin_prom = rowMeans(dataset[,Tmin_camp]),
         Prec_acum = rowSums(dataset[,Prec_camp]),
         GDD_acum = rowSums(dataset[,GDD_camp]),
         TR_acum = rowSums(dataset[,TR_camp]),
         Rs_acum = rowSums(dataset[,Rs_camp]))
ml_dataset <- dataset %>% select(-Campaña,-PROVINCIA,-DISTRITO)
#write.csv(ml_dataset,"Resultados/ml_dataset.csv",row.names = FALSE)

# Exploratory analysis
glimpse(dataset)
summary(dataset)
dim(dataset) #8665 x 46
sum(is.na(dataset))
describe(dataset)
skim(dataset)

glimpse(redim)
summary(redim)
dim(redim) #51990    10
sum(is.na(redim))
describe(redim)
skim(redim)

# MONTHLY PLOTS-----------------------------------------------
  # Boxplot by "provincia"
ggplot(redim, aes(x=PROVINCIA ,y=Tmax)) + geom_boxplot(outlier.colour = "darksalmon",colour="cyan4", lwd=0.5) + xlab("") + ylab("") + ggtitle("Tmax (°C)") + theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=13), plot.title = element_text(size=22), axis.text.y = element_text(size=13))

ggplot(redim, aes(x=PROVINCIA ,y=Tmin)) + geom_boxplot(outlier.colour = "darksalmon",colour="cyan4", lwd=0.5) + xlab("") + ylab("") + ggtitle("Tmin (°C)") + theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=13), plot.title = element_text(size=22), axis.text.y = element_text(size=13))

ggplot(redim, aes(x=PROVINCIA ,y=Prec)) + geom_boxplot(outlier.colour = "darksalmon",colour="cyan4", lwd=0.5) + xlab("") + ylab("Pp (mm/mes)") + ggtitle("Pp (mm/mes)") + theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=13), plot.title = element_text(size=22), axis.text.y = element_text(size=13))

ggplot(redim, aes(x=PROVINCIA ,y=Rs)) + geom_boxplot(outlier.colour = "darksalmon",colour="cyan4", lwd=0.5) + xlab("") + ylab("") + ggtitle("RS (MJ/m2.mes)") + theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=13), plot.title = element_text(size=22), axis.text.y = element_text(size=13))

ggplot(redim, aes(x=PROVINCIA ,y=GDD)) + geom_boxplot(outlier.colour = "darksalmon",colour="cyan4", lwd=0.5) + xlab("") + ylab("") + ggtitle("GDD (°C)") + theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=13), plot.title = element_text(size=22), axis.text.y = element_text(size=13))

ggplot(redim, aes(x=PROVINCIA ,y=TR)) + geom_boxplot(outlier.colour = "darksalmon",colour="cyan4", lwd=0.5) + xlab("") + ylab("") + ggtitle("TR (°C)") + theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust=1,size=13), plot.title = element_text(size=22), axis.text.y = element_text(size=13))

  # Temporal Plots for "Puno"
redim2 <- redim %>% separate(Campaña, c(NA, "Año")) %>% mutate(Fecha=as.Date(paste0(Año,"-",Mes,"-01"))) %>% select(-Año,-Mes) %>% filter(PROVINCIA=="PUNO") %>% group_by(PROVINCIA,Fecha) %>% summarise(Tmax=mean(Tmax),Tmin=mean(Tmin),Prec=mean(Prec),GDD=mean(GDD),TR=mean(TR),Rs=mean(Rs))

tmax_lines <- ggplot(redim2,aes(x=Fecha, y=Tmax)) +
  geom_line(colour="cornflowerblue", size=1) +
  ylab("Tmax (°C)") + xlab("") +
  geom_smooth(method = "lm", colour="darkgoldenrod1") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
tmin_lines <- ggplot(redim2,aes(x=Fecha, y=Tmin)) +
  geom_line(colour="cornflowerblue", size=1) +
  ylab("Tmin (°C)") + xlab("") +
  geom_smooth(method = "lm", colour="darkgoldenrod1") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
prec_lines <- ggplot(redim2,aes(x=Fecha, y=Prec)) +
  geom_line(colour="cornflowerblue", size=1) +
  ylab("Prec (mm/mes)") + xlab("") +
  geom_smooth(method = "lm", colour="darkgoldenrod1") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
gdd_lines <- ggplot(redim2,aes(x=Fecha, y=GDD)) +
  geom_line(colour="cornflowerblue", size=1) +
  ylab("GDD (°C)") + xlab("") +
  geom_smooth(method = "lm", colour="darkgoldenrod1") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
tr_lines <- ggplot(redim2,aes(x=Fecha, y=TR)) +
  geom_line(colour="cornflowerblue", size=1) +
  ylab("Rango Temp. (°C)") + xlab("") +
  geom_smooth(method = "lm", colour="darkgoldenrod1") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
rs_lines <- ggplot(redim2,aes(x=Fecha, y=Rs)) +
  geom_line(colour="cornflowerblue", size=1) +
  ylab("Rad. solar (MJ/m2.mes)") + xlab("") +
  geom_smooth(method = "lm", colour="darkgoldenrod1") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

grid.arrange(tmax_lines, tmin_lines, prec_lines, rs_lines, gdd_lines, tr_lines, nrow = 3, ncol = 2,top = "Provincia de Puno - Campañas 1997-2016")

  # Matrix of correlations
cor_matrix <- select(redim,Tmax,Tmin,Prec,Rs)
ggpairs(cor_matrix, aes(colour=""))

# PLOTS BY CAMPAIGN-------------------------------
  # Boxplot of class "Rendimiento"
ggplot(dataset, aes(x=PROVINCIA ,y=Rdto_kg.ha)) + geom_boxplot(outlier.colour = "darksalmon",colour="cyan4", lwd=0.5) + xlab("") + ylab("") + ggtitle("Rdto (kg/ha)") + theme(plot.title = element_text(hjust = 0.5, size=20))
  # Histogram of class "Rendimiento"
ggplot(dataset, aes(x = Rdto_kg.ha)) +
  geom_histogram(bins = 15, fill="#69b3a2", color="#e9ecef", alpha = 0.9) +
  labs(x="Rendimiento (kg/ha)", y = "Frecuencia") +
  theme_bw()
  # Correlation Matrix
acum_prom <- select(dataset,Tmax_prom,Tmin_prom,Prec_acum,GDD_acum,TR_acum,Rs_acum,Rdto_kg.ha)
ggpairs(acum_prom, aes(colour=""))
  # Temporal evolution of "Rendimiento"
dataset2 <- dataset %>% separate(Campaña, c(NA, "Año")) %>% mutate(Fecha=as.Date(paste0(Año,"-01-01"))) %>% select(-Año) %>% group_by(Fecha) %>% summarise(Rdto_kg.ha=mean(Rdto_kg.ha))

rdto_lines <- ggplot(dataset2,aes(x=Fecha, y=Rdto_kg.ha)) +
  geom_line(colour="cornflowerblue", size=1) +
  ylab("Rdto_kg.ha (kg/ha)") + xlab("") +
  geom_smooth(method = "lm", colour="darkgoldenrod1") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
rdto_lines

