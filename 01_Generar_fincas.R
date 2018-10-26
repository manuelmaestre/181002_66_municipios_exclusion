## Integra las fincas en historico y las adicionales de cobertura
## Genera las fincas sin xy para procesar su busqueda
## Agrega las x,y que se hayan recuperado anteriormente

## Library load

library(readxl)
library(data.table)
library(stringr)
library(zoo)
library(rgdal)

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


FTTH.file <- '../../000_DWH_txt_files/00_Coberturas/01_out_data/01_total_direcciones_FTTH.txt'
munis.file <- '../../00_Analisis_municipios/data/clean/00_Analisis_municipios.xlsx'
fecha <- format(Sys.time(), "%Y%m%d")
dir.salida.capas <- './outdata/shapefiles'
nombre.capa.fincas <- 'fincas'


############################################################################################################################
########################################            IMPORT DATA                      #######################################
############################################################################################################################


fincas.cobertura <- data.table(read.csv(file = FTTH.file,
                                    header = T,
                                    sep = ";",
                                    quote = "",
                                    dec = ",",
                                    colClasses = 'character',
                                    comment.char = "",
                                    encoding = 'UTF-8',
                                    strip.white = T))

fincas.historicas <- data.table(read.csv('./indata/Total_fincas_municipios_ZET.csv',
                                            header = T,
                                            sep = ";",
                                            quote = "",
                                            dec = ",",
                                            colClasses = 'character',
                                            comment.char = "",
                                            encoding = 'UTF-8',
                                            strip.white = T))


############################################################################################################################
########################################            MAIN CODE                        #######################################
############################################################################################################################


# Preparar datos fincas cobertura exclusion

fincas.cobertura <- fincas.cobertura[ranking == 1,]
fincas.cobertura$UUII <- as.integer(fincas.cobertura$UUII)
fincas.cobertura$accesos <- as.integer(fincas.cobertura$accesos)
fincas.cobertura.exclusion <- fincas.cobertura[Exclusion == 1, .(UUII = sum(UUII), accesos = sum(accesos)), by = c('ine.txt', 'Provincia', 'Municipio', 'G18', 'Tipo.via', 'Nombre.Via', 'Numero', 'ordenada.tipo.huella', 'Codigo.Postal')]
rm(fincas.cobertura)

##### Normalizar nombres de via de la tabla fincas.cobertura.exclusion

normalizador.vias.cobertura.exclusion <- fincas.cobertura.exclusion[, c('G18', 'Tipo.via', 'Nombre.Via', 'UUII')][order(-UUII)]
normalizador.vias.cobertura.exclusion <- normalizador.vias.cobertura.exclusion[!duplicated(normalizador.vias.cobertura.exclusion, by = 'G18'),]
normalizador.vias.cobertura.exclusion$UUII <- NULL
fincas.cobertura.exclusion[, c("Tipo.via", "Nombre.Via"):=NULL]
fincas.cobertura.exclusion <- merge(fincas.cobertura.exclusion, normalizador.vias.cobertura.exclusion, by.x = 'G18', by.y = 'G18')
fincas.cobertura.exclusion <- fincas.cobertura.exclusion[,.(UUII = sum(UUII), accesos = sum(accesos)), by = c('ine.txt', 'Provincia', 'Municipio', 'G18', 'Tipo.via', 'Nombre.Via', 'Numero', 'ordenada.tipo.huella', 'Codigo.Postal')]
rm(normalizador.vias.cobertura.exclusion)

##### Padding de la clave a 18 0 17 con 0 por la izquierda

fincas.cobertura.exclusion[nchar(G18) < 17, G18 := str_pad(G18,width = 17, side = 'left', pad = '0')]

##### Verificar claves de tablas

print(paste0("REGISTROS fincas cobertura exclusion - REGISTROS unicos: ", nrow(fincas.cobertura.exclusion)-length(unique(fincas.cobertura.exclusion$G18))))

if (nrow(fincas.cobertura.exclusion)-length(unique(fincas.cobertura.exclusion$G18))>0){
  
  fincas.cobertura.exclusion[duplicated(fincas.cobertura.exclusion$G18),]
  
}


# Preparar datos fincas historicas

##### Eliminamos duplicados del total fincas

fincas.historicas <- fincas.historicas[!duplicated(fincas.historicas, by = 'cod_finca'),]
fincas.historicas <- fincas.historicas[tipo_cod_finca == "G",]
fincas.historicas$tipo_cod_finca <- NULL
fincas.historicas[codigo_postal == '', cod_finca:= NA]


## 2 Poblar codigos postales arrastrando ascendente y descendente por calle desde el primer valor no NA

fincas.historicas <- fincas.historicas[,][order(cod_finca)]
fincas.historicas$Par.impar <- 'Par'
fincas.historicas[as.integer(str_sub(cod_finca, 13,17)) %% 2 != 0, Par.impar:= 'Impar']

### Arrastramos agrupando por via, par, impar

fincas.historicas[, codigo_postal:= na.locf(codigo_postal, na.rm = F), by = .(cod_finca, Par.impar)]
fincas.historicas[, codigo_postal:= na.locf(codigo_postal, na.rm = F, fromLast = T), by = .(cod_finca, Par.impar)]

### Arrastramos para el resto

fincas.historicas[, codigo_postal:= na.locf(codigo_postal, na.rm = F), by = cod_finca]
fincas.historicas[, codigo_postal:= na.locf(codigo_postal, na.rm = F, fromLast = T), by = cod_finca]


fincas.historicas$Par.impar <- NULL

print(paste0("REGISTROS total fincas - REGISTROS unicos: ", nrow(fincas.historicas)-length(unique(fincas.historicas$cod_finca))))

if (nrow(fincas.historicas)-length(unique(fincas.historicas$cod_finca))>0){
  
  fincas.historicas[duplicated(fincas.historicas$cod_finca),]
  
}

##### Padding de la clave a 17 con 0 por la izquierda

#fincas.historicas[, .N, by = (nchar(cod_finca))]
#fincas.historicas[nchar(cod_finca) == 16, cod_finca := str_pad(cod_finca,width = 17, side = 'left', pad = '0')]
fincas.historicas <- fincas.historicas[nchar(cod_finca) == 17 | nchar(cod_finca)== 18,]

##### Marcamos el tipo de huella en las fincas históricas en caso de estar cubiertas

fincas.historicas <- merge(fincas.historicas, fincas.cobertura.exclusion[, c("G18", "ordenada.tipo.huella", "UUII")], all.x = T, by.x = 'cod_finca', by.y = 'G18')
fincas.historicas[is.na(UUII), UUII := UUII_EST]

##### fincas en cobertura y no en historico a añadir al total fincas 

fincas.incrementales <- fincas.cobertura.exclusion[!(G18 %in% fincas.historicas$cod_finca),]
fincas.incrementales$accesos <- NULL
fincas.incrementales$X <- ''
fincas.incrementales$Y <- ''
fincas.incrementales$letra <- substr(fincas.incrementales$G18, 18,18)
fincas.incrementales$UUII_EST <- fincas.incrementales$UUII

setcolorder(fincas.incrementales, c( "G18", "ine.txt", "Provincia", "Municipio", "X", "Y", "Tipo.via", "Nombre.Via", "Numero", "letra", "UUII_EST", "Codigo.Postal", "ordenada.tipo.huella", "UUII"))
setnames(fincas.incrementales, old = colnames(fincas.incrementales), new = colnames(fincas.historicas))
fincas.historicas$origen <- 'historico'
fincas.incrementales$origen <- 'cobertura'
total.fincas <- rbind(fincas.historicas, fincas.incrementales)
total.fincas[is.na(ordenada.tipo.huella), ordenada.tipo.huella := '00_Libre']
total.fincas <- total.fincas[!is.na(UUII),]

### Marcamos libre o cubierta en funcion del tipo de huella

relacion.tipohuella.librecubierta <- data.table(ordenada.tipo.huella = c("00_Libre","01_StandAlone","02_Remedies","03_Mutualizada MMB","04_Mutualizada OSP","05_Ufinet","06_Adamo","07_AccesoFibra_JAZZ","08_AccesoFibra_OSP","09_AccesoFibra_VDF", "10_NEBA"),
                                                LibreCubierta = c("Cubierta")
                                                )
relacion.tipohuella.librecubierta[ordenada.tipo.huella == '00_Libre', LibreCubierta := 'Libre']
relacion.tipohuella.librecubierta[ordenada.tipo.huella == '10_NEBA', LibreCubierta := 'Libre']
orden.columnas.inicial <- colnames(total.fincas)
total.fincas <- merge(total.fincas, relacion.tipohuella.librecubierta, all.x = T, by.x = 'ordenada.tipo.huella', by.y = 'ordenada.tipo.huella')
nuevas.columnas <- colnames(total.fincas)[!colnames(total.fincas) %in% orden.columnas.inicial]
setcolorder(total.fincas,append(orden.columnas.inicial, nuevas.columnas))

# TODO:  Cambiar proyección a WGS84 de las fincas historicas. Vienen de origen en EDS50 UTM zONE 30

EPSG <- make_EPSG()
proyeccion.inicial <- as.character(EPSG[grepl("ED50 / UTM zone 30N", EPSG$note), c("code")])
proyeccion.final <- as.character(EPSG[grepl("WGS 84 / UTM zone 30N", EPSG$note), c("code")])

# Creamos la capa de puntos desde las xy de las fincas
total.fincas$X <- as.double(str_replace(total.fincas$X, ",", "."))
total.fincas$Y <- as.double(str_replace(total.fincas$Y, ",", "."))

# Eliminar outlayers xy para evitar problemas en la proyección
outlayers <- c("28000010000400140", "28000010839900001")
total.fincas <- total.fincas[!cod_finca %in% outlayers,]
total.fincas.xy <- total.fincas[X != 0 & is.na(X)==F,]
total.fincas.xy <- total.fincas.xy[,][order(Y)]

# Oviedo tiene mal las coordenadas hay que dividir la X entre 1000 y la Y entre 10
total.fincas.xy[MUNICIPIO == 'Oviedo', X := X/10000]
total.fincas.xy[MUNICIPIO == 'Oviedo', Y := Y/1000]
total.fincas.xy <- total.fincas.xy[,][order(Y)]

total.fincas.xy <- total.fincas.xy[,][order(X)]

## Coordenadas dentro de margenes de España ED50 UMT30

xmin <- -1088383
xmax <- 1304525
ymin <- 3097279
ymax <- 4900600

outlayers2 <- total.fincas.xy[!((X <= xmax & X >= xmin) & (Y <= ymax & Y >= ymin)),]
total.fincas.xy <- total.fincas.xy[(X <= xmax & X >= xmin) & (Y <= ymax & Y >= ymin),]


## Proyectamos a WGS84 y guardamos como shape

coordenadas <- as.matrix(total.fincas.xy[,.(X, Y)])
capa_puntos <- SpatialPointsDataFrame(coordenadas, total.fincas.xy, proj4string = CRS("+init=epsg:23030"), coords.nrs = c(5,6), match.ID = T )
# LLevamos a la misma proyeccion comun del proyecto
capa_puntos <- spTransform(capa_puntos, CRS("+init=epsg:32630"))
proj4string(capa_puntos) <- CRS("+init=epsg:32630")



# TODO: Poblar con datos de x,y obtenidos de Cartociudad











############################################################################################################################
########################################            EXPORTS                          #######################################
############################################################################################################################

### Capa de salida

suppressWarnings(writeOGR(capa_puntos, dsn = dir.salida.capas, layer = nombre.capa.fincas, driver = "ESRI Shapefile",overwrite_layer = T ))

### Ficheros de datos


write.table(x = total.fincas[(is.na(X) | X == 0) & !is.na(codigo_postal),c("cod_finca", "ine_mun", "PROVINCIA", "MUNICIPIO", "TipoVia", "Calle", "Npolic", "letra", "codigo_postal")],
          file = 'indata/fincas_sin_xy.txt',
          quote = F, sep = ";",
          row.names = F,
          col.names = T,
          fileEncoding = 'UTF-8' )


write.table(x = total.fincas.xy,
          file = 'outdata/total_fincas_xy.txt',
          quote = F, sep = ";",
          row.names = F,
          col.names = T,
          fileEncoding = 'UTF-8' )

write.table(x = total.fincas.xy[MUNICIPIO == 'Albacete',],
            file = 'outdata/total_fincas_Albacete_xy.txt',
            quote = F, sep = ";",
            row.names = F,
            col.names = T,
            fileEncoding = 'UTF-8' )

############################################################################################################################
########################################            REPORTS                          #######################################
############################################################################################################################









