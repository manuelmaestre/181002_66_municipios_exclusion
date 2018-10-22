## Analizar fincas en municipios de exclusion sobre huella actual
## Analizar fincas en municipios de exclusion en huella actual y no en
## total de fincas con xy


## Library load

library(readxl)
library(data.table)
library(stringr)

## Environment cleanning

rm(list = ls())

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


## Path and static variables definition

## Definicion de rutas y variables estáticas
FTTH.file <- '../../000_DWH_txt_files/00_Coberturas/01_out_data/01_total_direcciones_FTTH.txt'
munis.file <- '../../00_Analisis_municipios/data/clean/00_Analisis_municipios.xlsx'
fecha <- format(Sys.time(), "%Y%m%d")

fincas.cobertura <- data.table(read.csv(file = FTTH.file,
                                    header = T,
                                    sep = ";",
                                    quote = "",
                                    dec = ",",
                                    colClasses = 'character',
                                    comment.char = "",
                                    encoding = 'UTF-8',
                                    strip.white = T))

total.fincas.66munis <- data.table(read.csv('./indata/Total_fincas_municipios_ZET.csv',
                                            header = T,
                                            sep = ";",
                                            quote = "",
                                            dec = ",",
                                            colClasses = 'character',
                                            comment.char = "",
                                            encoding = 'Latin-1',
                                            strip.white = T))

fincas.cobertura <- fincas.cobertura[ranking == 1,]
fincas.cobertura$UUII <- as.integer(fincas.cobertura$UUII)
fincas.cobertura$accesos <- as.integer(fincas.cobertura$accesos)
fincas.cobertura.exclusion <- fincas.cobertura[Exclusion == 1, .(UUII = sum(UUII), accesos = sum(accesos)), by = c("G18", "ine.txt", "Provincia", "Municipio", "Tipo.via", "Nombre.Via", "Numero", "BIS", "ordenada.tipo.huella")]

## Marcamos en el total fincas municipios de exclusion, las que cruzan con fincas en cobertura

total.fincas.66munis <- merge(total.fincas.66munis, fincas.cobertura.exclusion[, c("G18", "ordenada.tipo.huella", "UUII", "accesos")], all.x = T, by.x = 'cod_finca', by.y = 'G18')
total.fincas.66munis[is.na(UUII), UUII := UUII_EST]

## Agregamos las fincas en cobertura pero que no estaban en el total fincas











