## Integra las fincas en historico y las adicionales de cobertura
## Genera las fincas sin xy para procesar su busqueda
## Agrega las x,y que se hayan recuperado anteriormente

## Library load

library(readxl)
library(data.table)
library(stringr)

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
                                            encoding = 'Latin-1',
                                            strip.white = T))


############################################################################################################################
########################################            MAIN CODE                        #######################################
############################################################################################################################


# Preparar datos fincas cobertura exclusion

fincas.cobertura <- fincas.cobertura[ranking == 1,]
fincas.cobertura$UUII <- as.integer(fincas.cobertura$UUII)
fincas.cobertura$accesos <- as.integer(fincas.cobertura$accesos)
fincas.cobertura.exclusion <- fincas.cobertura[Exclusion == 1, .(UUII = sum(UUII), accesos = sum(accesos)), by = c('ine.txt', 'Provincia', 'Municipio', 'G18', 'Tipo.via', 'Nombre.Via', 'Numero', 'ordenada.tipo.huella')]
rm(fincas.cobertura)

##### Normalizar nombres de via de la tabla fincas.cobertura.exclusion

normalizador.vias.cobertura.exclusion <- fincas.cobertura.exclusion[, c('G18', 'Tipo.via', 'Nombre.Via', 'UUII')][order(-UUII)]
normalizador.vias.cobertura.exclusion <- normalizador.vias.cobertura.exclusion[!duplicated(normalizador.vias.cobertura.exclusion, by = 'G18'),]
normalizador.vias.cobertura.exclusion$UUII <- NULL
fincas.cobertura.exclusion[, c("Tipo.via", "Nombre.Via"):=NULL]
fincas.cobertura.exclusion <- merge(fincas.cobertura.exclusion, normalizador.vias.cobertura.exclusion, by.x = 'G18', by.y = 'G18')
fincas.cobertura.exclusion <- fincas.cobertura.exclusion[,.(UUII = sum(UUII), accesos = sum(accesos)), by = c('ine.txt', 'Provincia', 'Municipio', 'G18', 'Tipo.via', 'Nombre.Via', 'Numero', 'ordenada.tipo.huella')]
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

setcolorder(fincas.incrementales, c( "G18", "ine.txt", "Provincia", "Municipio", "X", "Y", "Tipo.via", "Nombre.Via", "Numero", "letra", "UUII_EST", "ordenada.tipo.huella", "UUII"))
setnames(fincas.incrementales, old = colnames(fincas.incrementales), new = colnames(fincas.historicas))
fincas.historicas$origen <- 'historico'
fincas.incrementales$origen <- 'cobertura'
total.fincas <- rbind(fincas.historicas, fincas.incrementales)
total.fincas[is.na(ordenada.tipo.huella), ordenada.tipo.huella := '00_Libre']
total.fincas <- total.fincas[!is.na(UUII),]


# TODO:  Cambiar proyección a WGS84








# TODO: Poblar con datos de x,y obtenidos de Cartociudad






############################################################################################################################
########################################            EXPORTS                          #######################################
############################################################################################################################


write.table(x = total.fincas[is.na(X) | X == "",],
          file = 'indata/fincas_sin_xy.txt',
          quote = F, sep = ";",
          row.names = F,
          col.names = T,
          fileEncoding = 'UTF-8' )


write.table(x = total.fincas[!(is.na(X) | X == ""),],
          file = 'outdata/total_fincas_xy.txt',
          quote = F, sep = ";",
          row.names = F,
          col.names = T,
          fileEncoding = 'UTF-8' )


############################################################################################################################
########################################            REPORTS                          #######################################
############################################################################################################################









