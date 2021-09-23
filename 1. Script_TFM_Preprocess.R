#Machine Learning Algorithms
#Oswaldo Arturo Díaz Arca
#TFM - Universidad Internacional de la Rioja

rm(list=ls())
require(ncdf4)
require(lubridate)
require(dplyr)
require(tidyr)
library(raster)
library(sp)

setwd("D:/ruta_personal/TFM_UNIR-main")
# FUNCTIONS---------------------------------------
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

# IMPORT PISCO-----------------------------------------------
Tmax_daily_nc = 'Data/PISCO netCDF/PISCOv1.1_Tmax_daily_1981_2016.nc'
Tmin_daily_nc = 'Data/PISCO netCDF/PISCOv1.1_Tmin_daily_1981_2016.nc'
Pp_daily_nc = 'Data/PISCO netCDF/PISCOv2.1_Pp_daily_1981_2016.nc'
doc_names <- c(Tmax_daily_nc,Tmin_daily_nc,Pp_daily_nc)

for (i in 1:length(doc_names)){
  nc_data = nc_open(doc_names[i])
  print(nc_data)
  # Filter PUNO coordinates and period 1996-2016
  lon_indx <- which(nc_data$dim$X$vals > -71.2 & nc_data$dim$X$vals < -68.5)
  lat_inx <- which(nc_data$dim$Y$vals > -17.3 & nc_data$dim$Y$vals < -12.8)
  times_indx <- which(nc_data$dim$T$vals >= 13149.5 & nc_data$dim$T$vals <= 20819.5)
  # Extraction of variables
  lon <- nc_data$dim$X$val[lon_indx]
  lat <- nc_data$dim$Y$val[lat_inx]
  time <- nc_data$dim$T$val[times_indx] %>% -0.5
  dates = add_with_rollback(as.Date("1960-01-01"), days(time), roll_to_first = T)
  ntime <- length(dates); nlon <- length(lon); nlat <- length(lat)
  MyVariable <- ncvar_get(nc_data, if(i==1){"tmax"}
                          else if(i==2){"tmin"}
                          else if(i==3){"Prec"})[lon_indx, lat_inx, times_indx]
  nc_close(nc_data)
  # Conversion to Matrix
  variable_vec_long <-as.vector(MyVariable)
  variable_matrix <- matrix(variable_vec_long,nrow=nlon*nlat*ntime,ncol=1)
  lonlat <- as.matrix(expand.grid(lon,lat,dates))
  # Matrix to dataframe
  nom_var_tx <- "Tmax"; nom_var_tn <- "Tmin"; nom_var_prec <- "Prec"
  if(i==1){Tmax_daily_df <-  matrix_to_df(lonlat,variable_matrix,nom_var_tx)}
  else if(i==2){Tmin_daily_df <-  matrix_to_df(lonlat,variable_matrix,nom_var_tn)}
  else if(i==3){Prec_daily_df <-  matrix_to_df(lonlat,variable_matrix,nom_var_prec)}
}

# Merging Variables
temps_daily <- merge(Tmax_daily_df,Tmin_daily_df, by=c("Lon","Lat","Fecha"))
climatics_daily = merge(temps_daily, Prec_daily_df, by = c("Lon","Lat","Fecha"))
# Extraction of not coincident rows
dif <- setdiff(Prec_daily_df[,1:3], climatics_daily[,1:3])
pp_faltantes <- merge(dif, Prec_daily_df, by=c("Lon","Lat","Fecha"))
# Round Lon and Lat to make coincidence
pp_faltantes = pp_faltantes %>% mutate(Lon = round(Lon,4), Lat = round(Lat,6))
temps_daily = temps_daily %>% mutate(Lon = round(Lon,4), Lat = round(Lat,6))
# Merging again
climatic_faltantes <- merge(temps_daily,pp_faltantes,by=c("Lon","Lat","Fecha"))
climatic_total <- merge(climatic_faltantes,climatics_daily,all=T)

#----------------READ netCDF OF "HORAS DE SOL"--------------------
# Importing
sd_full <- read.csv("Data/sd_full.csv")
sd_full$Lon[sd_full$Lon == -68.85] <- -68.85001
# Merging with climatic data
climatic_total2 <- merge(climatic_total,sd_full, by=c("Lon","Lat","Fecha"))

#----CALCULATE "RADIACION SOLAR"
clima_total <- climatic_total2 %>%
  rowwise ( )  %>%
  mutate(Rs = rad_solar(Fecha,Lat,sd)) %>% 
  as.data.frame()
#----CALCULATE INDEXES AND GROUP MONTHLY
Temp_indx <- clima_total %>% 
  mutate(GDD= ifelse(month(Fecha)== 10 & month(Fecha)==11,GDD_calculate(Tmax,Tmin,2),GDD_calculate(Tmax,Tmin,5)), Trange = Tmax-Tmin) %>%
  group_by(Lon,Lat,month(Fecha),year(Fecha)) %>%
  summarise(GDD=sum(GDD), TR=sum(Trange), Tmax=mean(Tmax), Tmin=mean(Tmin), Prec=sum(Prec), Rs=mean(Rs)) %>%
  data.frame()
#----RESHAPING
clima <- Temp_indx %>% 
  mutate(Campaña=case_when(month.Fecha.>=8 ~ paste0(year.Fecha.,"/",year.Fecha.+1), 
                             month.Fecha.<8 ~ paste0(year.Fecha.-1,"/",year.Fecha.)),
         Mes = month.abb[month.Fecha.]) %>%
  dplyr::select(c(1,2,5:12)) %>%
  pivot_wider(names_from=Mes, values_from=c(GDD,TR,Tmax,Tmin,Prec,Rs)) %>%
  data.frame() %>% filter(Campaña != "1995/1996", Campaña != "2016/2017")

#---------------------CUTTING FOR "PUNO"------------------------------
# Import distrital limits
lim_distrital <- shapefile("Data/Limites Distritales/DISTRITOS.shp")
lim_puno <- lim_distrital[lim_distrital$DEPARTAMEN=="PUNO",c(2,4,6)]
# Convert "clima" to spatial points
clima_sp_points = sp::SpatialPoints(clima[,1:2])
crs(clima_sp_points) = crs(lim_puno)
# Cross datasets
crossed_puno <- clima %>% 
  cbind(over(clima_sp_points, lim_puno)) %>% 
  filter(DEPARTAMEN == "PUNO") #11280 registros / 564 registros lonlat

#-----------MERGING WITH "RENDIMIENTO"--------------------------------
# Rendimientos MIDAGRI: 2288 reg / 104 reg distritos
rdtos <- read.csv("Data/RESUMEN_RDTO.csv",sep=";", encoding="UTF-8")
names(rdtos) <- c("PROVINCIA","Campaña","DISTRITO","Rdto_kg.ha")
# Misspelled names
setdiff(rdtos$DISTRITO,crossed_puno$DISTRITO)
setdiff(crossed_puno$DISTRITO,rdtos$DISTRITO)
setdiff(rdtos$PROVINCIA,crossed_puno$PROVINCIA)
# Correct misspelled names
rdtos$PROVINCIA[rdtos$PROVINCIA == "CHUCHUITO"] <- "CHUCUITO"
rdtos$PROVINCIA[rdtos$PROVINCIA == "PUTINA"] <- "SAN ANTONIO DE PUTINA"
rdtos$DISTRITO[rdtos$DISTRITO == "JOSE D. CHOQUEHUANCA"] <- "JOSE DOMINGO CHOQUEHUANCA"
crossed_puno$DISTRITO[crossed_puno$DISTRITO == "MUÃ‘ANI"] <- "MUÑANI"
crossed_puno$DISTRITO[crossed_puno$DISTRITO == "NUÃ‘OA"] <- "NUÑOA"
crossed_puno$DISTRITO[crossed_puno$DISTRITO == "MAÃ‘AZO"] <- "MAÑAZO"
# Refresh de merge
climxrdto_papa_puno <- merge(crossed_puno,rdtos,by=c("PROVINCIA","DISTRITO","Campaña"))
#write.csv(climxrdto_papa_puno,"Data/climxrdto_papa_puno.csv",row.names = FALSE)
dim(climxrdto_papa_puno) # 10280 rows

# Plots
plot(lim_puno[,2], col="gold", axes=TRUE,panel.first = grid(),main = "Departamento de Puno vs Puntos PISCO")
points(climxrdto_papa_puno[,4:5],pch = 20, col = "black")
points(crossed_puno[,1:2],pch = 20, col = "black")

# Bibliography:
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
