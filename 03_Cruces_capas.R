############################################################################################################################
########################################             LIBRARY LOAD                ###########################################
############################################################################################################################

library(readxl)
library(data.table)
library(stringr)
library(rgdal)
library(rgeos)
library(xlsx)

## Environment cleanning

rm(list = ls())

############################################################################################################################
########################################             FUNCTION DEFS               ###########################################
############################################################################################################################

# Copy data out of R
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '^', na="")
  close(f)  
}

# Paste data into R

paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}

cols.tabla.comillas <- function(tabla.datos){
  cat(paste("c(\"", paste(colnames(tabla.datos), sep=" ", collapse = "\", \"")), "\")", sep="",collapse="")
}

cols.tabla <- function(tabla.datos){
  cat(paste("c(", paste(colnames(tabla.datos), sep="", collapse = ", ")), ")", sep="",collapse="")
}


############################################################################################################################
########################################            PATH & STATIC VARS               #######################################
############################################################################################################################

dir.capas <- 'outdata/shapefiles'
nombre.capa.bolsas.sin.fincas <- 'bolsas_sin_fincas'
nombre.capa.bolsas <- 'bolsas'
nombre.capa.fincas <- 'fincas'
fecha <- format(Sys.time(), "%Y%m%d")


############################################################################################################################
########################################            IMPORT DATA                      #######################################
############################################################################################################################

bolsas <- readOGR(dir.capas, nombre.capa.bolsas, disambiguateFIDs = T, encoding = 'UTF-8')
fincas <- readOGR(dir.capas, nombre.capa.fincas, disambiguateFIDs = T, encoding = 'UTF-8')



############################################################################################################################
########################################            MAIN CODE                        #######################################
############################################################################################################################

## Generamos una tabla con todas las fincas pobladas con la información de las bolsas

fincas.sobre.bolsas <- over(fincas, bolsas)

fincas.sobre.bolsas$indice <- rownames(fincas.sobre.bolsas)
fincas$indice <- rownames(fincas@data)
fincas.sobre.bolsas <- as.data.table(merge(fincas@data, fincas.sobre.bolsas, by.x = "indice", by.y = "indice"))
setnames(fincas.sobre.bolsas,
         colnames(fincas.sobre.bolsas),
         c("indice", "G17", "ine_mun", "Provincia", "Municipio", "X", "Y", "TipoVia", "Calle", "Npolic", "letra", "UUII_ES", "cod_postal", "huella", "UUII", "origen", "Estado", "Id", "TIPO_RED", "TIPO_EDIFICIO", "CONEXION", "OBSERVACIONES", "INE", "bolsa_numero", "bolsa_ID"))


fincas.sobre.bolsas[is.na(TIPO_RED) == T, TIPO_RED := "Sin Bolsa"]




###########################################################################################################################
########################################            REPORTS                          #######################################
############################################################################################################################

resumen.fincas <- fincas.sobre.bolsas[, .(UUII = sum(UUII)), by = c('ine_mun', 'Municipio','TIPO_RED', 'Estado')]
resumen.fincas[, .(sum(UUII)), by = 'TIPO_RED']
resumen.fincas <- dcast(resumen.fincas, ine_mun + Municipio + Estado ~ TIPO_RED , sum)
inebolsas <- unique(bolsas@data$INE)
resumen.fincas$datosAT <- 'no'
resumen.fincas[ine_mun %in% inebolsas, datosAT := 'si']

### bolsas sin fincas

total.bolsas <- unique(bolsas@data$bols_ID)
bolsas.con.fincas <- fincas.sobre.bolsas[TIPO_RED != "Sin Bolsa", .N, by = c('bolsa_ID')]$bolsa_ID
bolsas.sin.fincas <- total.bolsas[!total.bolsas %in% bolsas.con.fincas]

shape.bolsas.sin.fincas <- bolsas[bolsas@data$bols_ID %in% bolsas.sin.fincas,]

############################################################################################################################
########################################            EXPORTS                          #######################################
############################################################################################################################

suppressWarnings(writeOGR(shape.bolsas.sin.fincas, dsn = dir.capas, layer = nombre.capa.bolsas.sin.fincas, driver = "ESRI Shapefile", overwrite_layer = T, verbose = T, encoding = 'UTF-8' ))
write.xlsx(x = resumen.fincas, file = 'outdata/resumen_fincas.xlsx',row.names = F)



