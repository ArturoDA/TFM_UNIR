#Algoritmos Machine Learning
#Arturo Díaz Arca
#15-08-21
#TFM - Universidad Internacional de la Rioja

rm(list=ls())
require(ncdf4)
require(lubridate)
require(dplyr)
require(tidyr)
library(raster)
library(sp)

setwd("D:/UNIR Maestría/2DO CICLO/Trabajo de Fin de Máster")
#FUNCIONES ---------------------------------------
GDD_calculate <- function(Tmax, Tmin, Tbase){
  Tmedia <- (Tmin+Tmax)/2
  if_else(Tmedia>Tbase, Tmedia-Tbase,0)
}
matrix_to_df <- function (dims_x, vars_y, nom_var){
  var_df <- data.frame(cbind(dims_x,vars_y))
  names(var_df) <- c("Lon","Lat","Fecha",nom_var)
  var_df[ , c(1:2,4)] <- apply(var_df[ , c(1:2,4)], 2, function(x) as.numeric(x))
  var_df$Fecha <- as.Date(var_df$Fecha)
  var_df
}
convert_netCDF <- function(file){
  nc_data <- nc_open(file)
  var_name <- 'sd'
  lon_unique <- round(unique(climatic_total$Lon),4)
  lat_unique <- unique(climatic_total$Lat)
  lon_indx <- which(as.list(nc_data$dim$longitude$vals) %in% lon_unique)
  lat_inx <- which(as.list(nc_data$dim$latitude$vals) %in% lat_unique)
  lon <- nc_data$dim$longitude$val[lon_indx]
  lat <- nc_data$dim$latitude$val[lat_inx]
  time <- ncvar_get(nc_data, 'time')
  format = paste0(substr(file,41,44),"-",substr(file,46,47),"-01")
  dates = add_with_rollback(as.Date(format), days(time), roll_to_first = T)
  ntime <- length(dates); nlon <- length(lon); nlat <- length(lat)
  MyVariable <- ncvar_get(nc_data, var_name)[lon_indx, lat_inx, ]
  nc_close(nc_data)
  variable_vec_long <-as.vector(MyVariable)
  variable_matrix <- matrix(variable_vec_long,nrow=nlon*nlat*ntime,ncol=1)
  lonlat <- as.matrix(expand.grid(lon,lat,dates))
  sd_df <-  matrix_to_df(lonlat,variable_matrix,"sd")
  sd_df
}
rad_solar <- function(date,lat,hs){
  Gsc = 0.082
  J = trunc((275 * month(date)/9 - 30 + day(date)) - 2)
  J = if(leap_year(date)==TRUE & month(date)>2){J+1} else if(month(date)==1 & day(date)==1){J+1} else if(month(date)<3){J+2} else{J}
  dr = 1 + 0.33*cos(2*pi*J/365)
  lat_rad = (pi/180)*lat
  declin_rad = 0.409*sin((2*pi*J/365) - 1.39)
  angulo = acos(-tan(lat_rad)*tan(declin_rad))
  Ra <- (24*60/pi)*Gsc*dr*(angulo*sin(lat_rad)*sin(declin_rad) + 
                             cos(lat_rad)*cos(declin_rad)*sin(angulo))
  As = 0.25;  Bs = 0.5;  N = 24*angulo/pi;  n = hs
  Rs <- (As +  Bs*n/N)*Ra
  Rs
}

# IMPORTAR PISCO-----------------------------------------------
Tmax_daily_nc = 'Data/PISCO netCDF/PISCOv1.1_Tmax_daily_1981_2016.nc'
Tmin_daily_nc = 'Data/PISCO netCDF/PISCOv1.1_Tmin_daily_1981_2016.nc'
Pp_daily_nc = 'Data/PISCO netCDF/PISCOv2.1_Pp_daily_1981_2016.nc'

nc_data = nc_open(Tmax_daily_nc)
print(nc_data)
var_name <- 'tmax'
#Posiciones para Puno y Años 1996-2016
lon_indx <- which(nc_data$dim$X$vals > -71.2 & nc_data$dim$X$vals < -68.5)
lat_inx <- which(nc_data$dim$Y$vals > -17.3 & nc_data$dim$Y$vals < -12.8)
times_indx <- which(nc_data$dim$T$vals >= 13149.5 & nc_data$dim$T$vals <= 20819.5)
#Extracción
lon <- nc_data$dim$X$val[lon_indx]
lat <- nc_data$dim$Y$val[lat_inx]
time <- nc_data$dim$T$val[times_indx] %>% -0.5
dates = add_with_rollback(as.Date("1960-01-01"), days(time), roll_to_first = T)
ntime <- length(dates); nlon <- length(lon); nlat <- length(lat)
MyVariable <- ncvar_get(nc_data, var_name)[lon_indx, lat_inx, times_indx]
nc_close(nc_data)
# Conversión a Matriz:
variable_vec_long <-as.vector(MyVariable)
variable_matrix <- matrix(variable_vec_long,nrow=nlon*nlat*ntime,ncol=1)
lonlat <- as.matrix(expand.grid(lon,lat,dates))
# Matriz a dataframe
nom_var_tx <- "Tmax"; nom_var_tn <- "Tmin"; nom_var_prec <- "Prec"
Tmax_daily_df <-  matrix_to_df(lonlat,variable_matrix,nom_var_tx) #<---Temp.Máx.
Tmin_daily_df <-  matrix_to_df(lonlat,variable_matrix,nom_var_tn) #<---Temp.Min.
Prec_daily_df <-  matrix_to_df(lonlat,variable_matrix,nom_var_prec) #<---Prec
#write.csv(Tmax_daily_df,"Resultados/Tmax_daily_df.csv",row.names = FALSE)
#write.csv(Tmin_daily_df,"Resultados/Tmin_daily_df.csv",row.names = FALSE)
#write.csv(Prec_daily_df,"Resultados/Prec_daily_df.csv",row.names = FALSE)
#Tmax_daily_df <- read.csv("Resultados/Tmax_daily_df.csv", row.names = 1)
#Tmin_daily_df <- read.csv("Resultados/Tmin_daily_df.csv", row.names = 1)
#Prec_daily_df <- read.csv("Resultados/Prec_daily_df.csv", row.names = 1)

# UNIÓN DE VARIABLES
temps_daily <- merge(Tmax_daily_df,Tmin_daily_df, by=c("Lon","Lat","Fecha"))
climatics_daily = merge(temps_daily, Prec_daily_df, by = c("Lon","Lat","Fecha"))
#Extracción de no coincidentes
dif <- setdiff(Prec_daily_df[,1:3], climatics_daily[,1:3])
pp_faltantes <- merge(dif, Prec_daily_df, by=c("Lon","Lat","Fecha"))
#Rendondear Lon y Lat para que coincidan
pp_faltantes = pp_faltantes %>% mutate(Lon = round(Lon,4), Lat = round(Lat,6))
temps_daily = temps_daily %>% mutate(Lon = round(Lon,4), Lat = round(Lat,6))
#Volver a unir
climatic_faltantes <- merge(temps_daily,pp_faltantes,by=c("Lon","Lat","Fecha"))
climatic_total <- merge(climatic_faltantes,climatics_daily,all=T)
#write.csv(climatic_total, "Resultados/climatic_total.csv",row.names = FALSE)
#climatic_total <- read.csv("Resultados/climatic_total.csv", row.names = 1)

#----------------LEER LOS netCDF DE HORAS DE SOL--------------------
#Importar las rutas
paths <- list.files("Data/PISCO netCDF/Horas de Sol/", full.names=T)
paths1 <- paths[1:60]; paths2 <- paths[61:120]
paths3 <- paths[121:187];paths4 <- paths[188:252]
sd_list_df1 <- lapply(paths1,convert_netCDF) #Convertir a DF
sd_list_df2 <- lapply(paths2,convert_netCDF)
sd_list_df3 <- lapply(paths3,convert_netCDF)
sd_list_df4 <- lapply(paths4,convert_netCDF)
#Unirlos
sd_1996_2000 <- Reduce(function(d1, d2) merge(d1, d2, all = TRUE), sd_list_df1)
sd_2001_2005 <- Reduce(function(d1, d2) merge(d1, d2, all = TRUE), sd_list_df2)
sd_2006_2011 <- Reduce(function(d1, d2) merge(d1, d2, all = TRUE), sd_list_df3)
sd_2011_2016 <- Reduce(function(d1, d2) merge(d1, d2, all = TRUE), sd_list_df4)
sd_full = Reduce(function(d1, d2) merge(d1, d2, all = TRUE), 
                 list(sd_1996_2005,sd_2006_2011,sd_2011_2016)) # merge total
sd_full$Lon[sd_full$Lon == -68.85] <- -68.85001
climatic_total2 <- merge(climatic_total,sd_full, by=c("Lon","Lat","Fecha"))

#write.csv(sd_full, "Resultados/sd_full.csv",row.names = FALSE)
#write.csv(climatic_total2, "Resultados/climatic_total2.csv",row.names = FALSE)
#climatic_total2 <- read.csv("Resultados/climatic_total2.csv")
climatic_total2 <- climatic_total2 %>% mutate(Fecha = as.Date(Fecha))

#----CALCULAR RADIACION SOLAR
clima_total <- climatic_total2 %>%
  rowwise ( )  %>%
  mutate(Rs = rad_solar(Fecha,Lat,sd)) %>% 
  as.data.frame()
#write.csv(clima_total, "Resultados/clima_total.csv",row.names = FALSE)
clima_total <- read.csv("Resultados/clima_total.csv")
clima_total <- clima_total %>% mutate(Fecha = as.Date(Fecha))

# CALCULO DE INDICES Y AGRUPACIÓN A NIVEL MENSUAL-------------
Temp_indx <- clima_total %>% 
  mutate(GDD= ifelse(month(Fecha)== 10 & month(Fecha)==11,GDD_calculate(Tmax,Tmin,2),GDD_calculate(Tmax,Tmin,5)), Trange = Tmax-Tmin) %>%
  group_by(Lon,Lat,month(Fecha),year(Fecha)) %>%
  summarise(GDD=sum(GDD), TR=sum(Trange), Tmax=mean(Tmax), Tmin=mean(Tmin), Prec=sum(Prec), Rs=mean(Rs)) %>%
  data.frame()
#write.csv(Temp_indx, "Resultados/Temp_indx.csv",row.names = FALSE)

# RESHAPING---------------------------------------------------
clima <- Temp_indx %>% 
  mutate(Campaña = case_when(month.Fecha. >= 8 ~ paste0(year.Fecha.,"/",year.Fecha.+1), 
                             month.Fecha. < 8 ~ paste0(year.Fecha.-1,"/",year.Fecha.)),
         Mes = month.abb[month.Fecha.]) %>%
  dplyr::select(c(1,2,5:12)) %>%
  pivot_wider(names_from=Mes, values_from=c(GDD,TR,Tmax,Tmin,Prec,Rs)) %>%
  data.frame() %>% filter(Campaña != "1995/1996", Campaña != "2016/2017")
#write.csv(clima,"Resultados/clima.csv",row.names = FALSE) #Export

# CORTE PARA PUNO---------------------------------------------------
lim_distrital <- shapefile("Data/Limites Distritales/DISTRITOS.shp")
lim_puno <- lim_distrital[lim_distrital$DEPARTAMEN=="PUNO",c(2,4,6)]
clima <- read.csv("Resultados/Temp_indx_reshape.csv",row.names = 1)
#Clima a puntos espaciales
clima_sp_points = sp::SpatialPoints(clima[,1:2])
crs(clima_sp_points) = crs(lim_puno)
#shapefile(clima_sp_points, filename='Data/clima_sp_points.shp')  #Exportar los puntos .shp
#Cruzando datos
crossed_puno <- clima %>% 
  cbind(over(clima_sp_points, lim_puno)) %>% 
  filter(DEPARTAMEN == "PUNO") #11280 registros / 564 registros lonlat

# UNIÓN CON RENDIMIENTO-------------------------------------------
#Rendimientos MIDAGRI: 2288 reg / 104 reg distritos
rdtos <- read.csv("Data/Data Agrícola - DRA Puno/RESUMEN_RDTO.csv",sep=";", encoding="UTF-8")
names(rdtos) <- c("PROVINCIA","Campaña","DISTRITO","Rdto_kg.ha")
#Corregir nombres mal escritos
setdiff(rdtos$DISTRITO,crossed_puno$DISTRITO)
setdiff(crossed_puno$DISTRITO,rdtos$DISTRITO)
setdiff(rdtos$PROVINCIA,crossed_puno$PROVINCIA)
#Corrección de nombres mal escritos
rdtos$PROVINCIA[rdtos$PROVINCIA == "CHUCHUITO"] <- "CHUCUITO"
rdtos$PROVINCIA[rdtos$PROVINCIA == "PUTINA"] <- "SAN ANTONIO DE PUTINA"
rdtos$DISTRITO[rdtos$DISTRITO == "JOSE D. CHOQUEHUANCA"] <- "JOSE DOMINGO CHOQUEHUANCA"
crossed_puno$DISTRITO[crossed_puno$DISTRITO == "MUÃ‘ANI"] <- "MUÑANI"
crossed_puno$DISTRITO[crossed_puno$DISTRITO == "NUÃ‘OA"] <- "NUÑOA"
crossed_puno$DISTRITO[crossed_puno$DISTRITO == "MAÃ‘AZO"] <- "MAÑAZO"
#Actualización de la unión
climxrdto_papa_puno <- merge(crossed_puno,rdtos,by=c("PROVINCIA","DISTRITO","Campaña"))
write.csv(climxrdto_papa_puno,"Resultados/climxrdto_papa_puno.csv",row.names = FALSE)

dim(climxrdto_papa_puno) # 10280 rows

#Prueba para corroborar qué distritos quedaron excluidos
#prueba <- unique(crossed_puno[,c(43,45)])
#prueba[prueba["PROVINCIA"]=="YUNGUYO",]
#lim_distrital_filtrado <- as.data.frame(lim_distrital)
#lim_distrital_filtrado[lim_distrital_filtrado["PROVINCIA"]=="LAMPA",]

#Gráficas
plot(lim_puno[,2], col="gold", axes=TRUE,panel.first = grid(),main = "Departamento de Puno vs Puntos PISCO")
points(climxrdto_papa_puno[,4:5],pch = 20, col = "black")
points(crossed_puno[,1:2],pch = 20, col = "black")

# Bibliografía-------------------------
# Fuente data: http://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/.Temp/.v1p1/
# http://www.fao.org/3/x0490s/x0490s.pdf
# https://rstudio-pubs-static.s3.amazonaws.com/363315_9bb7a62d516545bcb19be2351be38347.html
# https://www.youtube.com/watch?v=H2499CQ9MTw
# https://lubridate.tidyverse.org/reference/mplus.html
# https://pjbartlein.github.io/REarthSysSci/netCDF.html
# https://www.marsja.se/r-add-column-to-dataframe-based-on-other-columns-conditions-dplyr/
# https://stackoverflow.com/questions/38295137/how-to-transpose-column-and-group-at-the-same-time
# https://mgimond.github.io/ES218/Week03b.html
# https://tidyselect.r-lib.org/reference/faq-external-vector.html
# https://sites.google.com/site/antoniopadillac/Home/pesosymedidas/meridianosyparalelos
# https://codeyarns.files.wordpress.com/2011/07/20110729-vim-named-colors.png?w=700
